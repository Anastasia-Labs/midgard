import {
  type BlockReplayMaterial,
  computeFraudProofRawL1PointId,
  FraudProofL1CheckpointChangedError,
  FraudProofL1UnavailableError,
} from "@al-ft/midgard-fault-proofs";
import type { FactStore } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  assertWatcherStateQueueObservation,
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherStateQueueHeaderObservation,
} from "../indexers/authenticated-state-queue-observation.js";
import {
  readWatcherUserEventAuthority,
  type WatcherUserEvent,
  type WatcherUserEventKind,
} from "../verification/user-event.js";
import type { WatcherObservationAuthority } from "./observation.js";
import { replayEventIds } from "./replay-events.js";
import { watcherReplayMaterial } from "./replay-material.js";
import type { WatcherReplayRetention } from "./replay-retention.js";
import { WATCHER_QUEUE_OUTPUTS_TABLE } from "./tables.js";
import type {
  WatcherUserEventIdentity,
  WatcherUserEvents,
  WatcherUserEventScripts,
} from "./user-events.js";
import { WatcherUserEventUnavailable } from "./user-events.js";

export type WatcherFollowerReplaySource = Readonly<{
  epoch(): number;
  confirmed(
    observation: WatcherAuthenticatedStateQueueObservation,
  ): Promise<Readonly<{ state: SDK.ConfirmedState; current(): boolean }>>;
  materialFor(payload: SDK.DaPayload): Promise<BlockReplayMaterial>;
  checkpointDurable(endTime: bigint): void;
  close(): void;
}>;

/** The follower owns all authority; retained DA contributes no ledger entries. */
export const createWatcherFollowerReplaySource = (
  input: Readonly<{
    store: FactStore;
    userEvents: WatcherUserEvents;
    identity: WatcherUserEventIdentity;
    scripts: WatcherUserEventScripts;
    retention: WatcherReplayRetention;
    minimumCheckpointDepth: number;
    observationAuthority: WatcherObservationAuthority;
  }>,
): WatcherFollowerReplaySource => {
  if (
    !Number.isSafeInteger(input.minimumCheckpointDepth) ||
    input.minimumCheckpointDepth < 2
  )
    throw new Error("replay checkpoint needs an explicit stable L1 depth");
  let generation = 0;
  let closed = false;
  const unsubscribe = input.store.onGeneration(({ rewound }) => {
    generation += 1;
    if (rewound.reset === true) input.retention.reset();
  });
  const headerFor = async (
    payload: SDK.DaPayload,
  ): Promise<WatcherStateQueueHeaderObservation> => {
    const hash = payload.block_body.header_hash;
    const row = (
      await input.store.transaction("read", (tx) =>
        tx.query(
          `SELECT * FROM ${WATCHER_QUEUE_OUTPUTS_TABLE} WHERE kind = 'node' AND header_hash = ? ORDER BY from_slot LIMIT 1`,
          [Buffer.from(hash, "hex")],
        ),
      )
    )[0];
    if (row === undefined)
      throw new FraudProofL1UnavailableError(
        `header ${hash}'s authenticated admission is unavailable`,
      );
    const hex = (field: string) =>
      Buffer.from(row[field] as Uint8Array).toString("hex");
    const headerCborHex = hex("header_cbor");
    if (
      SDK.encodeHeaderCbor(payload.block_body.header).toString("hex") !==
      headerCborHex
    )
      throw new Error(
        "retained replay header differs from its authenticated L1 admission",
      );
    const nodeCbor = hex("state_queue_node_cbor");
    const blockHash = hex("anchor_block_hash");
    const slot = String(row.anchor_slot);
    const blockNo = String(row.anchor_height);
    const canonical = await input.store.pointStatus({
      slot: Number(slot),
      hash: Buffer.from(blockHash, "hex"),
    });
    if (canonical.kind !== "canonical")
      throw new FraudProofL1CheckpointChangedError(
        "replay admission left the canonical chain",
      );
    return {
      headerHash: hash,
      headerCborHex,
      stateQueueNodeCborHex: nodeCbor,
      linkedListDatumCborHex: hex("datum_cbor"),
      daAvailability: Data.from(nodeCbor, SDK.StateQueueNode).da_attestation,
      queueOutRef: `${hex("tx_hash")}#${String(row.output_index)}`,
      nextHeaderHash:
        row.next_header_hash === null ? null : hex("next_header_hash"),
      observedTransactionHash: hex("anchor_tx_hash"),
      observedBlockHash: blockHash,
      observedSlot: slot,
      observedBlockNo: blockNo,
      observedChainPointId: computeFraudProofRawL1PointId({
        blockHash,
        blockNo,
        slot,
      }),
      finalityDepth: "1",
    };
  };
  return {
    epoch: () => generation,
    confirmed: async (observation) => {
      assertWatcherStateQueueObservation(observation);
      if (
        closed ||
        observation.deploymentIdentityDigest !==
          input.identity.deploymentManifestId ||
        observation.protocolScriptAuthorityDigest !==
          input.observationAuthority.authorityDigest ||
        observation.stateQueuePolicyId !==
          input.observationAuthority.protocolScriptHashes.stateQueueMint
      )
        throw new Error(
          "replay checkpoint observation has another deployment or a closed source",
        );
      if (
        BigInt(observation.nativePoint.finalityDepth) <
        BigInt(input.minimumCheckpointDepth)
      )
        throw new FraudProofL1UnavailableError(
          "checkpoint observation is below the configured stable depth",
        );
      const before = generation;
      const state = await input.store.transaction("read", async (tx) => {
        const point = observation.nativePoint;
        const block = (
          await tx.query("SELECT hash, height FROM l1_blocks WHERE slot = ?", [
            Number(point.slot),
          ])
        )[0];
        if (
          block === undefined ||
          Buffer.from(block.hash as Uint8Array).toString("hex") !==
            point.blockHash ||
          String(block.height) !== point.blockNo
        )
          throw new FraudProofL1CheckpointChangedError(
            "checkpoint observation left the canonical chain",
          );
        const cursor = (
          await tx.query("SELECT height FROM l1_follower_cursor")
        )[0];
        if (
          cursor === undefined ||
          Number(cursor.height) - Number(block.height) + 1 <
            input.minimumCheckpointDepth
        )
          throw new FraudProofL1UnavailableError(
            "checkpoint observation no longer meets the stable depth",
          );
        const roots = await tx.query(
          `SELECT tx_hash, output_index, datum_cbor FROM ${WATCHER_QUEUE_OUTPUTS_TABLE} WHERE kind = 'root' AND from_slot <= ? AND (to_slot IS NULL OR to_slot > ?)`,
          [Number(point.slot), Number(point.slot)],
        );
        const root = roots[0];
        if (roots.length !== 1 || root === undefined)
          throw new FraudProofL1UnavailableError(
            "replay checkpoint has no unique confirmed-state root",
          );
        const outRef = `${Buffer.from(root.tx_hash as Uint8Array).toString("hex")}#${String(root.output_index)}`;
        if (
          observation.finalizedQueue[0]?.outRef !== outRef ||
          observation.finalizedQueue[0]?.headerHash !== null
        )
          throw new FraudProofL1CheckpointChangedError(
            "checkpoint root differs from its admitted queue observation",
          );
        const datum = Data.from(
          Buffer.from(root.datum_cbor as Uint8Array).toString("hex"),
          SDK.LinkedListDatum,
        );
        if (!("Root" in datum.data))
          throw new Error("confirmed-state output is not a root");
        const confirmed = Data.castFrom(
          datum.data.Root.data,
          SDK.ConfirmedState,
        );
        if (SDK.confirmedStateNextHeaderProtocolVersion(confirmed) === null)
          throw new Error("confirmed-state protocol identity is invalid");
        return confirmed;
      });
      return { state, current: () => !closed && before === generation };
    },
    materialFor: async (payload) => {
      const before = generation;
      let fence:
        | Awaited<ReturnType<WatcherUserEvents["headerFence"]>>
        | undefined;
      try {
        if (closed)
          throw new FraudProofL1UnavailableError(
            "the follower replay source is closed",
          );
        const header = await headerFor(payload);
        const node = Data.from(
          header.stateQueueNodeCborHex,
          SDK.StateQueueNode,
        );
        if (
          SDK.encodeHeaderCbor(node.header).toString("hex") !==
          header.headerCborHex
        )
          throw new Error("replay clock node carries another header");
        const blockTimeMs = Number(node.block_time);
        if (!Number.isSafeInteger(blockTimeMs) || blockTimeMs < 0)
          throw new Error(
            "authenticated replay commit clock is outside the safe time range",
          );
        fence = await input.userEvents.headerFence(header);
        const events = new Map<string, WatcherUserEvent>();
        const due = await replayEventIds({ ...input, header, payload });
        for (const [kind, ids] of due)
          for (const id of ids) {
            const authority = await input.userEvents.eventAuthority({
              kind,
              eventId: id,
              throughHeader: header,
            });
            events.set(
              `${kind}:${id}`,
              (await readWatcherUserEventAuthority(authority)).event,
            );
          }
        await fence.refresh();
        const activeFence = fence;
        return watcherReplayMaterial({
          network: input.identity.network,
          header: payload.block_body.header,
          blockTimeMs,
          event: async (kind: WatcherUserEventKind, id: string) => {
            if (!activeFence.current())
              throw new FraudProofL1CheckpointChangedError(
                "replay events were retired by a rewind",
              );
            const read = events.get(`${kind}:${id}`);
            if (read === undefined)
              throw new Error(
                "replay requested an event outside its authenticated admission set",
              );
            return read;
          },
        });
      } catch (cause) {
        if (before !== generation || (fence !== undefined && !fence.current()))
          throw new FraudProofL1CheckpointChangedError(
            "replay admission changed while capturing its L1 material",
          );
        if (cause instanceof WatcherUserEventUnavailable)
          throw new FraudProofL1UnavailableError(cause.message, { cause });
        throw cause;
      }
    },
    checkpointDurable: input.retention.checkpointDurable,
    close: () => {
      closed = true;
      unsubscribe();
      input.userEvents.close();
    },
  };
};
