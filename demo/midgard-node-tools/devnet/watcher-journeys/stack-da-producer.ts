import { readFile } from "node:fs/promises";
import { join } from "node:path";

import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";
import { loadDaLibp2pIdentity } from "@al-ft/midgard-core/da-libp2p-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  createDaLibp2pProducerTransport,
  type DaProducerPublicationManifest,
  type DaProducerPublicationReport,
  type DaProducerTransport,
  parseDaProducerPublicationManifest,
  publishDaPayloadInsert,
} from "midgard-node/da/libp2p-producer";
import type { DaPayloadsDB } from "midgard-node/database/index";

import type { JourneyBlock } from "./fixture.js";
import type { OperatorPause } from "./operator-pause.js";

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

/**
 * Operator-2's DA producer: its publication manifest, and its publications
 * through fresh dial-only transports. The drill publishes with operator-2's
 * producer key only while it holds operator-2's node stopped: every
 * publication first proves it with the hold a session installs (`guard`), and
 * one without a hold, or whose proof fails, is refused before any transport
 * opens.
 */
export const stackDaProducer = (
  manifest: DaProducerPublicationManifest,
  openTransport?: (
    manifest: DaProducerPublicationManifest,
  ) => Promise<DaProducerTransport>,
) => {
  const publishOnce = stackDaProducerPublisher(manifest, openTransport);
  let hold: Pick<OperatorPause, "assertStopped"> | undefined;
  return Object.freeze({
    manifest,
    guard: (value: Pick<OperatorPause, "assertStopped">) => {
      hold = value;
    },
    publish: async (insert: DaPayloadsDB.InsertInput) => {
      try {
        if (hold === undefined)
          throw new Error("no session holds operator-2's node");
        await hold.assertStopped();
      } catch (cause) {
        throw new Error(
          `Operator-2's node is not proven stopped; refusing to publish as operator-2's DA producer: ${cause instanceof Error ? cause.message : String(cause)}`,
          { cause },
        );
      }
      return await publishOnce(insert);
    },
  });
};

export type StackDaProducer = ReturnType<typeof stackDaProducer>;

/** The producer manifest the stack writes for a node publishing from the host. */
export const stackDaProducerManifestPath = (stackRunDirectory: string) =>
  join(stackRunDirectory, "services/producer-host.json");
