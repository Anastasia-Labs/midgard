import * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import {
  DaPayloadPublicationError,
  type DaProducerPeerResult,
  type DaProducerPublicationReport,
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
import { stackDaPayloadInsert } from "./stack-da-producer.js";

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

/**
 * Which committee peers took a payload, and how: `accepted`, `duplicate`, or
 * `rejected: <reason>`. A peer that took it either way had the bytes.
 */
export type StackDaDelivery = {
  readonly threshold: number;
  readonly peers: Readonly<Record<string, string>>;
};

/**
 * A refused block: at least the committee threshold of peers took its
 * payload, and no attestation appeared by its attestation deadline. The
 * block stays on the queue until its timeout removal, which `removal` holds
 * once it was already seen.
 */
export type StackDaRefused = {
  readonly kind: "refused";
  readonly headerHash: string;
  readonly attestationDeadlineMs: number;
  readonly delivery: StackDaDelivery;
  readonly removal?: StackDaAgedOut;
};

/** How a stack block's producer-path DA ended. */
export type StackDaOutcome = PublishedDaAttestationOutcome | StackDaRefused;

/**
 * A block the committee must refuse passed its attestation deadline
 * unattested, but fewer than the committee threshold of peers ever took its
 * payload: what went unattested was never delivered, so it proves nothing
 * about the committee's refusal.
 */
export class StackDaRefusalNotObserved extends Error {
  constructor(
    readonly headerHash: string,
    readonly reason:
      | "committee unreachable"
      | "committee never received payload",
    readonly delivery: StackDaDelivery,
  ) {
    super(
      `The committee's refusal of header ${headerHash} was not observed: ${reason} (${Object.keys(delivery.peers).length.toString()} of a threshold of ${delivery.threshold.toString()} peer(s) took its payload before its attestation deadline)`,
    );
    this.name = "StackDaRefusalNotObserved";
  }
}

/**
 * Committee reject codes about how the payload was delivered rather than
 * what it holds: another deployment, another payload schema, or a payload
 * over the committee's size bound. A refusal on one of them says nothing
 * about the committee's judgement of the family's fault.
 */
const DELIVERY_REJECT_CODES: ReadonlySet<string> = new Set([
  "deployment_fingerprint_mismatch",
  "payload_schema_version_mismatch",
  "payload_too_large",
]);

/** A committee peer rejected a refusal drill's payload for its delivery. */
export class StackDaDeliveryRejected extends Error {
  constructor(
    readonly headerHash: string,
    readonly peerId: string,
    readonly reasonCode: string,
    readonly delivery: StackDaDelivery,
  ) {
    super(
      `DA committee peer ${peerId} rejected the payload of header ${headerHash} with ${reasonCode}, a fault of the drill's delivery rather than of the payload, so the refusal proves nothing`,
    );
    this.name = "StackDaDeliveryRejected";
  }
}

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
  /** How many committee peers must take a payload. */
  readonly committeeThreshold: number;
  /** The delivery saved by an earlier attempt, and where to save it. */
  readonly delivery?: {
    readonly saved?: StackDaDelivery;
    save(delivery: StackDaDelivery): Promise<void>;
  };
  onStage(name: string): void;
};

/** An outcome where the committee had to attest. */
const settleAttestation = (
  outcome: PublishedDaAttestationOutcome | StackDaAgedOut,
): PublishedDaAttestationOutcome => {
  if (outcome.kind === "agedOut") throw new StackDaPayloadAgedOutError(outcome);
  return outcome;
};

/** The committee attested a block whose payload it must refuse. */
const refusedAttested = (headerHash: string): never => {
  throw new Error(
    `The DA committee attested header ${headerHash}, whose payload it must refuse`,
  );
};

/** Refuses a removal other than ageing out, where the committee had to refuse. */
const requireAgedOut = (
  headerHash: string,
  outcome: PublishedDaTargetCorrection | StackDaAgedOut,
): StackDaAgedOut => {
  if (outcome.kind !== "agedOut")
    throw new Error(
      `Header ${headerHash}, whose payload the DA committee must refuse, was fraud-corrected by ${outcome.removalTxHash} instead of ageing out`,
    );
  return outcome;
};

/** Statuses of a peer that took the payload's bytes. */
const tookDelivery = (status: DaProducerPeerResult["status"]) =>
  status === "accepted" || status === "duplicate" || status === "rejected";

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
 * operator's node would, and follow the stack's own committee.
 *
 * Attestation: attested when the coordinator applies the attestation. A
 * publication below threshold is retried while the block can still be
 * attested; peers that reject the payload outright are a hard failure, and a
 * block that ages out instead is `StackDaPayloadAgedOutError`.
 *
 * Refusal: publication is retried until at least the committee threshold of
 * peers took the payload (accepted or rejected it). The refusal is observed
 * once the block's attestation deadline passes with the block unattested,
 * before it ages out; with fewer peers reached by then it is
 * `StackDaRefusalNotObserved`. The committee attesting it fails the drill,
 * and so does a peer rejecting it for its delivery
 * (`StackDaDeliveryRejected`). Each peer's reject code is kept in the
 * delivery.
 *
 * A peer holding a conflicting payload for the header is a hard failure.
 */
export const publishStackDaPayload = async (
  block: JourneyBlock,
  expectation: StackDaExpectation,
  ports: StackDaPublicationPorts,
): Promise<StackDaOutcome> => {
  const { headerHash } = block;
  const delivered = new Map<string, string>(
    Object.entries(ports.delivery?.saved?.peers ?? {}),
  );
  const delivery = (): StackDaDelivery => ({
    threshold: ports.committeeThreshold,
    peers: Object.fromEntries(delivered),
  });
  const reachedQuorum = () => delivered.size >= ports.committeeThreshold;
  const record = async (results: readonly DaProducerPeerResult[]) => {
    let changed = false;
    for (const { peerId, status, error } of results)
      if (tookDelivery(status) && !delivered.has(peerId)) {
        delivered.set(
          peerId,
          status === "rejected" ? `rejected: ${error ?? "unknown"}` : status,
        );
        changed = true;
      }
    if (changed) await ports.delivery?.save(delivery());
    if (expectation === "refusal")
      for (const [peerId, taken] of delivered) {
        const reasonCode = taken.replace(/^rejected: /u, "");
        if (taken !== reasonCode && DELIVERY_REJECT_CODES.has(reasonCode))
          throw new StackDaDeliveryRejected(
            headerHash,
            peerId,
            reasonCode,
            delivery(),
          );
      }
  };
  const notObserved = () =>
    new StackDaRefusalNotObserved(
      headerHash,
      delivered.size === 0
        ? "committee unreachable"
        : "committee never received payload",
      delivery(),
    );
  const removed = async (
    attestationDeadlineMs?: number,
  ): Promise<StackDaOutcome> => {
    const removal = await ports.readRemoval();
    if (expectation === "attestation") return settleAttestation(removal);
    const agedOut = requireAgedOut(headerHash, removal);
    if (!reachedQuorum()) throw notObserved();
    return {
      kind: "refused",
      headerHash,
      // A header commit binds the node's block time to the header's end time.
      attestationDeadlineMs:
        attestationDeadlineMs ??
        Number(block.header.endTime + ports.attestationTimeoutMs),
      delivery: delivery(),
      removal: agedOut,
    };
  };
  let target = await ports.readTarget();
  if (target.kind === "absent") return removed();
  if (target.kind === "attested")
    return expectation === "attestation" ? target : refusedAttested(headerHash);
  const timedOutAt = Number(target.blockTime + ports.attestationTimeoutMs);
  const insert = stackDaPayloadInsert(block);
  let published = false;
  for (;;) {
    const publishing =
      expectation === "attestation" ? !published : !reachedQuorum();
    if (publishing && ports.now() < timedOutAt) {
      try {
        const report = await ports.publish(insert);
        published = true;
        await record(report.peerResults);
        ports.onStage(
          `DA payload ${headerHash} accepted by ${report.acceptedPeers.toString()} committee peer(s); awaiting the committee's ${expectation === "attestation" ? "attestation" : "attestation deadline"}`,
        );
      } catch (error) {
        const failure = publicationFailure(headerHash, error);
        if (error instanceof DaPayloadPublicationError)
          await record(error.report.peerResults);
        if (expectation === "attestation") {
          if (failure.kind === "refused")
            throw new Error(
              `The DA committee rejected payload ${headerHash}: ${failure.reasons.join(", ")}`,
            );
          ports.onStage(`DA payload ${headerHash} ${failure.detail}; retrying`);
        } else
          ports.onStage(
            failure.kind === "refused"
              ? `DA committee rejected payload ${headerHash} (${failure.reasons.join(", ")}); awaiting its attestation deadline`
              : reachedQuorum()
                ? `DA payload ${headerHash} taken by ${delivered.size.toString()} committee peer(s) (${[...delivered.values()].join(", ")}); awaiting its attestation deadline`
                : `DA payload ${headerHash} ${failure.detail}; retrying`,
          );
      }
    }
    target = await ports.readTarget();
    if (target.kind === "attested")
      return expectation === "attestation"
        ? target
        : refusedAttested(headerHash);
    if (target.kind === "absent") return removed(timedOutAt);
    if (expectation === "refusal" && ports.now() >= timedOutAt) {
      if (!reachedQuorum()) throw notObserved();
      ports.onStage(
        `DA committee refused payload ${headerHash}: ${delivered.size.toString()} peer(s) took it and none attested by its deadline`,
      );
      return {
        kind: "refused",
        headerHash,
        attestationDeadlineMs: timedOutAt,
        delivery: delivery(),
      };
    }
    if (ports.now() > timedOutAt + Number(ports.attestationTimeoutMs))
      throw new Error(
        `Unattested header ${headerHash} was not removed within an attestation timeout after it aged out`,
      );
    await ports.pause();
  }
};

/**
 * Follow a refused block, unattested past its attestation deadline, to its
 * authenticated timeout removal within one more attestation timeout. Its
 * attestation or fraud correction fails the drill.
 */
export const awaitStackDaAgedOut = async (
  headerHash: string,
  attestationDeadlineMs: number,
  ports: Pick<
    StackDaPublicationPorts,
    "readTarget" | "readRemoval" | "now" | "pause" | "attestationTimeoutMs"
  >,
): Promise<StackDaAgedOut> => {
  for (;;) {
    const target = await ports.readTarget();
    if (target.kind === "attested") return refusedAttested(headerHash);
    if (target.kind === "absent")
      return requireAgedOut(headerHash, await ports.readRemoval());
    if (
      ports.now() >
      attestationDeadlineMs + Number(ports.attestationTimeoutMs)
    )
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
