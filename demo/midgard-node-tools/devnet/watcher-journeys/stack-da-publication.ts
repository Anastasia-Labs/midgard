import { readFile } from "node:fs/promises";
import { join } from "node:path";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import {
  createDaLibp2pProducerTransport,
  DaPayloadPublicationError,
  type DaProducerPublicationManifest,
  type DaProducerPublicationReport,
  type DaProducerTransport,
  parseDaProducerPublicationManifest,
  publishDaPayloadInsert,
} from "midgard-node/da/libp2p-producer";
import type { DaPayloadsDB } from "midgard-node/database/index";
import type { PublishedDaAttestationOutcome } from "midgard-watcher/tests/support/published-block-actor";
import {
  authenticatePublishedHeaderRemoval,
  type PublishedDaTargetCorrection,
  type PublishedHeaderConsumption,
  reconcilePublishedDaTargetConsumption,
} from "midgard-watcher/tests/support/published-da-target-consumption";

import type {
  JourneyBlock,
  JourneyCategory,
  JourneyFixtureStage,
} from "./fixture.js";
import type { StackJourneyContext } from "./stack-context.js";

/**
 * The families whose fault payload the honest committee refuses before it
 * signs (the committee-acceptance table). On an attached stack their drill
 * ends at the refusal: the block is never attested and ages out at the DA
 * attestation timeout. Every other family is attested and convicted.
 */
export const STACK_DA_REFUSED_CATEGORIES: ReadonlySet<JourneyCategory> =
  new Set<JourneyCategory>([
    "canonicalDecodability",
    "daHashPreimage",
    "l2TxMistag",
  ]);

/** What the stack's honest committee does with a block's payload. */
export type StackDaExpectation = "attestation" | "refusal";

export const stackDaExpectation = (
  category: JourneyCategory,
): StackDaExpectation =>
  STACK_DA_REFUSED_CATEGORIES.has(category) ? "refusal" : "attestation";

/** A refused block's authenticated removal at its DA attestation timeout. */
export type StackDaAgedOut = {
  readonly kind: "agedOut";
  readonly headerHash: string;
  readonly removalTxHash: string;
  readonly removedStateQueueOutRef: string;
};

/** How a stack block's producer-path DA ended. */
export type StackDaOutcome = PublishedDaAttestationOutcome | StackDaAgedOut;

/**
 * A block the committee was expected to attest aged out instead. Nothing it
 * committed can be attested any more, so its staging must start again.
 */
export class StackDaPayloadAgedOutError extends Error {
  constructor(readonly outcome: StackDaAgedOut) {
    super(
      `Header ${outcome.headerHash} aged out unattested (removed by ${outcome.removalTxHash}): the committee did not attest its payload within the DA attestation timeout`,
    );
    this.name = "StackDaPayloadAgedOutError";
  }
}

/** The producer's publication row for a block, exactly as a node stores it. */
export const stackDaPayloadInsert = (
  block: JourneyBlock,
): DaPayloadsDB.InsertInput => {
  const { header } = block;
  const payload = Buffer.from(block.payloadEnvelopeCbor);
  return {
    header_hash: Buffer.from(block.headerHash, "hex"),
    consensus_profile_id: MIDGARD_CONSENSUS_PROFILE.profileId,
    version: 1,
    payload_cbor: payload,
    payload_sha256: Buffer.from(SDK.daPayloadHashHex(payload), "hex"),
    utxos_root: header.utxosRoot,
    forced_transactions_root: header.forcedTransactionsRoot,
    transactions_root: header.transactionsRoot,
    deposits_root: header.depositsRoot,
    withdrawals_root: header.withdrawalsRoot,
    transition_trace_root: header.transitionTraceRoot,
    event_to_step_root: header.eventToStepRoot,
    validation_traces_root: header.validationTracesRoot,
    withdrawal_count: header.withdrawalCount,
    forced_transaction_count: header.forcedTransactionCount,
    l2_transaction_count: header.l2TransactionCount,
    deposit_count: header.depositCount,
    total_event_count: header.totalEventCount,
    transition_step_count: header.transitionStepCount,
    validation_trace_count: header.validationTraceCount,
    block_start_time: new Date(Number(header.startTime)),
    block_end_time: new Date(Number(header.endTime)),
    // A header commit's validity ends at its end time, which binds block_time.
    block_time: new Date(Number(header.endTime)),
  };
};

/**
 * The drill's producer publication manifest: the stack's own host producer
 * manifest (the committee peers, threshold and limits) with operator-2's
 * libp2p key in place of our node's. Refuses our node's own peer id, whose
 * second live connection would endanger our node's DA publishing. Keys are
 * named by their variables only; their values are secrets.
 */
export const readStackDaProducerManifest = async (input: {
  manifestPath: string;
  operatorKey: { name: string; source: string };
  honestProducerKey: { name: string; source: string };
}): Promise<DaProducerPublicationManifest> => {
  const peerIdOf = async ({
    name,
    source,
  }: {
    name: string;
    source: string;
  }) => {
    try {
      return (await loadDaLibp2pIdentity(source)).peerId;
    } catch {
      throw new Error(`${name} is not a libp2p private key source`);
    }
  };
  if (
    (await peerIdOf(input.operatorKey)) ===
    (await peerIdOf(input.honestProducerKey))
  )
    throw new Error(
      `${input.operatorKey.name} is our node's own DA producer identity (${input.honestProducerKey.name}); use operator-2's`,
    );
  return parseDaProducerPublicationManifest(
    JSON.parse(await readFile(input.manifestPath, "utf8")),
    { DA_LIBP2P_PRIVATE_KEY_SOURCE: input.operatorKey.source },
  );
};

/** One publication through a fresh dial-only producer transport. */
export const stackDaProducerPublisher =
  (
    manifest: DaProducerPublicationManifest,
    openTransport: (
      manifest: DaProducerPublicationManifest,
    ) => Promise<DaProducerTransport> = (value) =>
      createDaLibp2pProducerTransport(value, { mode: "dial-only" }),
  ) =>
  async (
    insert: DaPayloadsDB.InsertInput,
  ): Promise<DaProducerPublicationReport> => {
    const transport = await openTransport(manifest);
    try {
      return await publishDaPayloadInsert({ insert, manifest, transport });
    } finally {
      await transport.close?.();
    }
  };

/** A header's state queue node as L1 holds it now. */
export type StackDaTarget =
  | { readonly kind: "absent" }
  | { readonly kind: "unattested"; readonly blockTime: bigint }
  | { readonly kind: "attested"; readonly txHash: string };

/**
 * Authenticate a vanished target's removal. A fraud correction of the exact
 * header reconciles as the actor's corrections do. Otherwise the removal must
 * be the protocol's timeout removal: it spent the node while it was still
 * unattested, no earlier than its block time plus the attestation timeout.
 */
export const classifyStackDaRemoval = async (input: {
  readonly headerHash: string;
  readonly stateQueueAddress: string;
  readonly stateQueuePolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly consumptions: readonly PublishedHeaderConsumption[];
  readConfirmedTransaction(txHash: string): Promise<{ cbor: string }>;
  slotToUnixTime(slot: number): number;
  readonly attestationTimeoutMs: bigint;
}): Promise<PublishedDaTargetCorrection | StackDaAgedOut> => {
  const removal = await authenticatePublishedHeaderRemoval(input);
  if (removal.fraudProof !== undefined)
    return reconcilePublishedDaTargetConsumption({
      ...input,
      attestationOutputs: [],
      submitted: [],
    });
  const node = await Effect.runPromise(
    Effect.flatMap(
      SDK.utxoToStateQueueUTxO(removal.removedOutput, input.stateQueuePolicyId),
      (utxo) => SDK.getStateQueueNodeFromStateQueueDatum(utxo.datum),
    ),
  );
  const timedOutAt = node.block_time + input.attestationTimeoutMs;
  if (
    node.da_attestation !== SDK.NO_DA_ATTESTATION ||
    removal.validFromSlot === undefined ||
    BigInt(input.slotToUnixTime(Number(removal.validFromSlot))) < timedOutAt
  )
    throw new Error(
      `Removal ${removal.removalTxHash} of header ${input.headerHash} is neither its fraud correction nor its attestation timeout`,
    );
  return {
    kind: "agedOut",
    headerHash: input.headerHash,
    removalTxHash: removal.removalTxHash,
    removedStateQueueOutRef: removal.removedStateQueueOutRef,
  };
};

export type StackDaPublicationPorts = {
  readTarget(): Promise<StackDaTarget>;
  publish(
    insert: DaPayloadsDB.InsertInput,
  ): Promise<DaProducerPublicationReport>;
  /** The authenticated removal of the vanished target. */
  readRemoval(): Promise<PublishedDaTargetCorrection | StackDaAgedOut>;
  /** Ledger time in milliseconds. */
  now(): number;
  pause(): Promise<void>;
  readonly attestationTimeoutMs: bigint;
  onStage(name: string): void;
};

const settle = (
  headerHash: string,
  expectation: StackDaExpectation,
  outcome: StackDaOutcome,
): StackDaOutcome => {
  if (expectation === "attestation") {
    if (outcome.kind === "agedOut")
      throw new StackDaPayloadAgedOutError(outcome);
    return outcome;
  }
  if (outcome.kind !== "agedOut")
    throw new Error(
      outcome.kind === "attested"
        ? `The DA committee attested header ${headerHash}, whose payload it must refuse`
        : `Header ${headerHash}, whose payload the DA committee must refuse, was fraud-corrected by ${outcome.removalTxHash} instead of ageing out`,
    );
  return outcome;
};

/**
 * What a failed publication settled: every peer refused the payload, or
 * nothing (too few peers answered, or no session opened), which is retried.
 * A peer holding another payload for the header is a hard failure.
 */
const publicationFailure = (
  headerHash: string,
  error: unknown,
):
  | { readonly kind: "refused"; readonly reasons: readonly string[] }
  | { readonly kind: "unsettled"; readonly detail: string } => {
  if (!(error instanceof DaPayloadPublicationError))
    return {
      kind: "unsettled",
      detail: `publication failed (${error instanceof Error ? error.message : String(error)})`,
    };
  const answers = error.report.peerResults;
  const conflict = answers.find(({ status }) => status === "conflict");
  if (conflict !== undefined)
    throw new Error(
      `DA committee peer ${conflict.peerId} holds a different payload for header ${headerHash}`,
    );
  return answers.length > 0 &&
    answers.every(({ status }) => status === "rejected")
    ? {
        kind: "refused",
        reasons: answers.map(({ error: reason }) => reason ?? "unknown"),
      }
    : {
        kind: "unsettled",
        detail: `reached ${error.report.acceptedPeers.toString()} peer(s), below threshold`,
      };
};

/**
 * Publish a committed block's payload through the producer path, as its
 * operator's node would, and follow the stack's own committee: attested when
 * the coordinator applies the attestation, or removed. A publication below
 * threshold is retried while the block can still be attested. Peers that
 * reject the payload outright or hold a conflicting one are a hard failure
 * unless the refusal is the expectation.
 */
export const publishStackDaPayload = async (
  block: JourneyBlock,
  expectation: StackDaExpectation,
  ports: StackDaPublicationPorts,
): Promise<StackDaOutcome> => {
  const { headerHash } = block;
  let target = await ports.readTarget();
  if (target.kind === "absent")
    return settle(headerHash, expectation, await ports.readRemoval());
  if (target.kind === "attested")
    return settle(headerHash, expectation, target);
  const timedOutAt = Number(target.blockTime + ports.attestationTimeoutMs);
  const insert = stackDaPayloadInsert(block);
  let delivered = false;
  for (;;) {
    if (!delivered && ports.now() < timedOutAt) {
      try {
        const report = await ports.publish(insert);
        delivered = true;
        ports.onStage(
          `DA payload ${headerHash} accepted by ${report.acceptedPeers.toString()} committee peer(s); awaiting the committee's attestation`,
        );
      } catch (error) {
        const failure = publicationFailure(headerHash, error);
        if (failure.kind === "unsettled")
          ports.onStage(`DA payload ${headerHash} ${failure.detail}; retrying`);
        else if (expectation === "attestation")
          throw new Error(
            `The DA committee rejected payload ${headerHash}: ${failure.reasons.join(", ")}`,
          );
        else {
          delivered = true;
          ports.onStage(
            `DA committee rejected payload ${headerHash} (${failure.reasons.join(", ")}); awaiting its timeout removal`,
          );
        }
      }
    }
    target = await ports.readTarget();
    if (target.kind === "attested")
      return settle(headerHash, expectation, target);
    if (target.kind === "absent")
      return settle(headerHash, expectation, await ports.readRemoval());
    if (ports.now() > timedOutAt + Number(ports.attestationTimeoutMs))
      throw new Error(
        `Unattested header ${headerHash} was not removed within an attestation timeout after it aged out`,
      );
    await ports.pause();
  }
};

/** Whether a journey stages on an attached stack rather than its own devnet. */
export const isStackJourneyContext = (
  context: JourneyFixtureStage["context"],
): context is StackJourneyContext =>
  "mode" in context && context.mode === "stack";

/** The producer manifest the stack writes for a node publishing from the host. */
export const stackDaProducerManifestPath = (stackRunDirectory: string) =>
  join(stackRunDirectory, "services/producer-host.json");
