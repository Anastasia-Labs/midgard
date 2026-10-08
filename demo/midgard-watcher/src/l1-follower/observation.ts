import { computeFraudProofRawL1PointId } from "@al-ft/midgard-fault-proofs";
import type { FactStore, SqlRow, SqlTx } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  admittedHeaders,
  admittedObservations,
  HEX_28,
  WATCHER_AUTHENTICATED_STATE_QUEUE_OBSERVATION_SCHEMA_VERSION,
  type WatcherAuthenticatedStateQueueObservation,
  type WatcherReleasedHeaderProof,
  WatcherRetainedHeaderAttestationPendingError,
  type WatcherStateQueueHeaderObservation,
} from "../indexers/authenticated-state-queue-observation.parse-persisted-header.js";
import type { WatcherDeploymentProtocolScriptAuthority } from "../runtime/deployment-identity.js";
import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import { readWatcherCheckpoints } from "./checkpoints.read.js";
import {
  readDepartedAttestedHeader,
  readDepartures,
} from "./projection.departed-headers.js";
import {
  WATCHER_QUEUE_OUTPUTS_TABLE,
  WATCHER_QUEUE_UNIT_HISTORY_TABLE,
} from "./tables.js";
import { readWatcherQueueView, type WatcherQueueHeader } from "./view.js";

/**
 * The authenticated state-queue observation, read from the watcher
 * projection at one confirmation depth (ticket W1). Every value is a pure
 * read of the follower store in one read transaction: there is no cursor
 * chain to persist, restore or rewind, so a rollback of any depth below k
 * changes the next read and nothing else.
 *
 * Depth 1 is the tip (the old inclusion view); depth
 * `RELEASE_FINALITY_DEPTH` is the release-final view. A header keeps
 * the minting tx as its observed point, and its depth is one of the two
 * values the old cursor recorded (1 until the minting block is release-final,
 * then the release depth), so the decision digest of a header changes at
 * most once.
 */

export type WatcherObservationRead =
  | Readonly<{
      kind: "ok";
      observation: WatcherAuthenticatedStateQueueObservation;
    }>
  | Readonly<{ kind: "unready"; reason: string; detail: string }>;

export type WatcherObservationAuthority = Pick<
  WatcherDeploymentProtocolScriptAuthority,
  "authorityDigest" | "deploymentFingerprint" | "protocolScriptHashes"
>;

type Block = Readonly<{
  slot: number;
  hash: Buffer;
  height: number;
  parentHash: Buffer | null;
}>;

const blockOf = (row: SqlRow): Block => ({
  slot: Number(row.slot as number | string),
  hash: Buffer.from(row.hash as Uint8Array),
  height: Number(row.height as number | string),
  parentHash:
    row.parent_hash === null || row.parent_hash === undefined
      ? null
      : Buffer.from(row.parent_hash as Uint8Array),
});

const blockAtHeight = async (
  tx: SqlTx,
  height: number,
): Promise<Block | null> => {
  const row = (
    await tx.query(
      "SELECT slot, hash, height, parent_hash FROM l1_blocks WHERE height = ?",
      [height],
    )
  )[0];
  return row === undefined ? null : blockOf(row);
};

type CursorRow = Readonly<{
  height: number;
  originSlot: number;
  prunedThroughSlot: number;
}>;

/** The follower cursor, read in the caller's transaction. */
const cursorIn = async (tx: SqlTx): Promise<CursorRow | null> => {
  const row = (
    await tx.query(
      "SELECT height, origin_slot, pruned_through_slot FROM l1_follower_cursor",
      [],
    )
  )[0];
  return row === undefined
    ? null
    : {
        height: Number(row.height as number | string),
        originSlot: Number(row.origin_slot as number | string),
        prunedThroughSlot: Number(row.pruned_through_slot as number | string),
      };
};

const pointId = (blockHash: string, blockNo: string, slot: string): string =>
  computeFraudProofRawL1PointId({ blockHash, blockNo, slot });

/** The two header depths the old cursor recorded. */
const anchoredDepth = (
  tipHeight: number,
  anchorHeight: number,
  releaseDepth: number,
): string =>
  (tipHeight - anchorHeight + 1 >= releaseDepth ? releaseDepth : 1).toString();

const headerObservation = (
  header: WatcherQueueHeader,
  finalityDepth: string,
): WatcherStateQueueHeaderObservation => {
  const observed = Object.freeze({
    headerHash: header.headerHash,
    headerCborHex: header.headerCborHex,
    stateQueueNodeCborHex: header.stateQueueNodeCborHex,
    linkedListDatumCborHex: header.linkedListDatumCborHex,
    daAvailability: Data.from(header.stateQueueNodeCborHex, SDK.StateQueueNode)
      .da_attestation,
    queueOutRef: header.queueOutRef,
    nextHeaderHash: header.nextHeaderHash,
    observedTransactionHash: header.observedTransactionHash,
    observedBlockHash: header.observedBlockHash,
    observedSlot: header.observedSlot,
    observedBlockNo: header.observedBlockNo,
    observedChainPointId: pointId(
      header.observedBlockHash,
      header.observedBlockNo,
      header.observedSlot,
    ),
    finalityDepth,
  });
  admittedHeaders.add(observed);
  return observed;
};

/**
 * The observation of the block `depth` blocks deep (the tip is depth 1),
 * or why there is none yet.
 */
export const readWatcherObservation = async (
  store: FactStore,
  input: Readonly<{
    authority: WatcherObservationAuthority;
    sourceId: string;
    depth: number;
    /** The release depth (the manifest's confirmation depth, `RELEASE_FINALITY_DEPTH` in production). */
    releaseDepth: number;
  }>,
): Promise<WatcherObservationRead> => {
  if (!Number.isSafeInteger(input.depth) || input.depth < 1)
    throw new Error("observation depth must be a positive integer");
  const hashes = input.authority.protocolScriptHashes;
  return await store.transaction("read", async (tx) => {
    const cursor = await cursorIn(tx);
    if (cursor === null)
      return {
        kind: "unready",
        reason: "l1_follower_no_cursor",
        detail: "the follower has not applied a block",
      };
    const tip = await blockAtHeight(tx, cursor.height);
    const block = await blockAtHeight(tx, cursor.height - input.depth + 1);
    if (tip === null || block === null || block.slot <= cursor.originSlot)
      return {
        kind: "unready",
        reason: "l1_follower_short_chain",
        detail: `the follower holds fewer than ${input.depth.toString()} blocks after its origin`,
      };
    const view = await readWatcherQueueView(tx, block.slot);
    if (!view.healthy)
      return { kind: "unready", reason: view.reason, detail: view.detail };
    const blockPoint = {
      blockHash: block.hash.toString("hex"),
      slot: block.slot.toString(),
      blockNo: block.height.toString(),
    };
    const chainPointId = pointId(
      blockPoint.blockHash,
      blockPoint.blockNo,
      blockPoint.slot,
    );
    const checkpoints = await readWatcherCheckpoints(tx, block, {
      deploymentIdentityDigest: input.authority.deploymentFingerprint,
      stateQueuePolicyId: hashes.stateQueueMint,
      finalityDepth: input.depth,
      prunedThroughSlot: cursor.prunedThroughSlot,
    });
    if (checkpoints.kind === "failed")
      return {
        kind: "unready",
        reason: "state_queue_checkpoint_failed",
        detail: `${checkpoints.transactionHash}: ${checkpoints.failure}`,
      };
    if (checkpoints.kind === "beyond_retention")
      return {
        kind: "unready",
        reason: "state_queue_checkpoints_beyond_retention",
        detail: `block ${blockPoint.slot} is at or below the pruned slot ${checkpoints.prunedThroughSlot.toString()}`,
      };
    const lock = view.correctionLock;
    const canonical = {
      schemaVersion:
        WATCHER_AUTHENTICATED_STATE_QUEUE_OBSERVATION_SCHEMA_VERSION,
      deploymentIdentityDigest: input.authority.deploymentFingerprint,
      protocolScriptAuthorityDigest: input.authority.authorityDigest,
      stateQueuePolicyId: hashes.stateQueueMint,
      hubOraclePolicyId: hashes.hubOracleMint,
      nativePoint: Object.freeze({
        ...blockPoint,
        parentBlockHash: block.parentHash?.toString("hex") ?? null,
        chainPointId,
        finalityDepth: input.depth.toString(),
      }),
      sourceId: input.sourceId,
      previousObservationDigest: null,
      checkpoints: Object.freeze([...checkpoints.checkpoints]),
      finalizedQueue: Object.freeze(
        view.queue.map((node) => Object.freeze({ ...node })),
      ),
      finalizedHeaders: Object.freeze(
        view.headers.map((header) =>
          headerObservation(
            header,
            anchoredDepth(
              tip.height,
              Number(header.observedBlockNo),
              input.releaseDepth,
            ),
          ),
        ),
      ),
      finalizedCorrectionLock:
        lock === null
          ? null
          : Object.freeze({
              outRef: lock.outRef,
              datum: Data.from(lock.datumCborHex, SDK.CorrectionLockDatum),
              observedTransactionHash: lock.observedTransactionHash,
              observedBlockHash: lock.observedBlockHash,
              observedSlot: lock.observedSlot,
              observedBlockNo: lock.observedBlockNo,
              observedChainPointId: pointId(
                lock.observedBlockHash,
                lock.observedBlockNo,
                lock.observedSlot,
              ),
              finalityDepth: anchoredDepth(
                tip.height,
                Number(lock.observedBlockNo),
                input.releaseDepth,
              ),
            }),
      correctionLockWitnesses: Object.freeze([
        ...checkpoints.correctionLockWitnesses,
      ]),
    };
    const observation: WatcherAuthenticatedStateQueueObservation =
      Object.freeze({
        ...canonical,
        observationDigest: watcherSha256CanonicalJson(canonical),
      });
    admittedObservations.add(observation);
    return { kind: "ok", observation };
  });
};

/** What the decision bridge and the availability runtime read beyond an observation. */
export type WatcherQueueHeaderSource = Readonly<{
  /**
   * A header that is or was in the queue, at its newest attested node:
   * the predecessor context of a classified header.
   */
  resolveRetainedHeader(
    input: Readonly<{ headerHash: string }>,
  ): Promise<WatcherStateQueueHeaderObservation>;
  /**
   * Headers of `observation` that left the queue after its point and at or
   * before the release-final block, merged or removed, by hash.
   */
  resolveMergedHeaders(
    input: Readonly<{
      observation: WatcherAuthenticatedStateQueueObservation;
    }>,
  ): Promise<ReadonlyMap<string, WatcherReleasedHeaderProof>>;
}>;

const hex = (value: unknown): string =>
  Buffer.from(value as Uint8Array).toString("hex");

export const createWatcherQueueHeaderSource = (
  store: FactStore,
  options: Readonly<{ releaseDepth: number }>,
): WatcherQueueHeaderSource => {
  const resolveRetainedHeader = async ({
    headerHash,
  }: Readonly<{ headerHash: string }>) => {
    if (!HEX_28.test(headerHash))
      throw new Error(
        "retained HeaderV1 lookup requires a 28-byte header hash",
      );
    const found = await store.transaction("read", async (tx) => {
      const cursor = await cursorIn(tx);
      if (cursor === null) return null;
      const versions = await tx.query(
        `SELECT q.tx_hash, q.output_index, q.header_cbor, q.state_queue_node_cbor, q.datum_cbor, q.next_header_hash, u.block_hash, u.block_height, u.from_slot FROM ${WATCHER_QUEUE_OUTPUTS_TABLE} q JOIN ${WATCHER_QUEUE_UNIT_HISTORY_TABLE} u ON u.header_hash = q.header_hash AND u.tx_hash = q.tx_hash WHERE q.header_hash = ? AND q.kind = 'node' ORDER BY u.block_height DESC, u.from_slot DESC, q.tx_hash DESC, q.output_index DESC`,
        [Buffer.from(headerHash, "hex")],
      );
      const departed = await readDepartedAttestedHeader(tx, headerHash);
      return { cursor, versions, departed };
    });
    if (found === null)
      throw new Error(
        "retained HeaderV1 lookup before the follower applied a block",
      );
    const { cursor, versions, departed } = found;
    const header = (
      fields: Omit<
        WatcherStateQueueHeaderObservation,
        "daAvailability" | "observedChainPointId" | "finalityDepth"
      >,
    ): WatcherStateQueueHeaderObservation | null => {
      const daAvailability = Data.from(
        fields.stateQueueNodeCborHex,
        SDK.StateQueueNode,
      ).da_attestation;
      if (daAvailability === SDK.NO_DA_ATTESTATION) return null;
      const observed = Object.freeze({
        ...fields,
        daAvailability,
        observedChainPointId: pointId(
          fields.observedBlockHash,
          fields.observedBlockNo,
          fields.observedSlot,
        ),
        finalityDepth: (
          cursor.height -
          Number(fields.observedBlockNo) +
          1
        ).toString(),
      });
      admittedHeaders.add(observed);
      return observed;
    };
    for (const row of versions) {
      const attested = header({
        headerHash,
        headerCborHex: hex(row.header_cbor),
        stateQueueNodeCborHex: hex(row.state_queue_node_cbor),
        linkedListDatumCborHex: hex(row.datum_cbor),
        queueOutRef: `${hex(row.tx_hash)}#${Number(row.output_index as number | string).toString()}`,
        nextHeaderHash:
          row.next_header_hash === null || row.next_header_hash === undefined
            ? null
            : hex(row.next_header_hash),
        observedTransactionHash: hex(row.tx_hash),
        observedBlockHash: hex(row.block_hash),
        observedSlot: Number(row.from_slot as number | string).toString(),
        observedBlockNo: Number(row.block_height as number | string).toString(),
      });
      if (attested !== null) return attested;
    }
    if (departed !== null && departed !== "unattested") {
      const attested = header({
        headerHash,
        headerCborHex: departed.headerCborHex,
        stateQueueNodeCborHex: departed.stateQueueNodeCborHex,
        linkedListDatumCborHex: departed.linkedListDatumCborHex,
        queueOutRef: departed.queueOutRef,
        nextHeaderHash: departed.nextHeaderHash,
        observedTransactionHash: departed.transactionHash,
        observedBlockHash: departed.blockHash,
        observedSlot: departed.slot.toString(),
        observedBlockNo: departed.height.toString(),
      });
      if (attested !== null) return attested;
    }
    if (versions.length > 0 || departed !== null)
      throw new WatcherRetainedHeaderAttestationPendingError(headerHash);
    throw new Error(
      "retained HeaderV1 has no state-queue output in the follower store",
    );
  };

  const resolveMergedHeaders = async ({
    observation,
  }: Readonly<{ observation: WatcherAuthenticatedStateQueueObservation }>) =>
    await store.transaction("read", async (tx) => {
      const released = new Map<string, WatcherReleasedHeaderProof>();
      const cursor = await cursorIn(tx);
      if (cursor === null) return released;
      const boundary = await blockAtHeight(
        tx,
        cursor.height - options.releaseDepth + 1,
      );
      if (boundary === null) return released;
      const departures = await readDepartures(
        tx,
        observation.finalizedHeaders.map(({ headerHash }) => headerHash),
        Number(observation.nativePoint.slot),
        boundary.slot,
      );
      // Chain order; a transition the watcher does not prove ends the walk.
      const ordered = [...departures].sort(
        (left, right) => left.slot - right.slot,
      );
      for (const departure of ordered) {
        const confirmationDepth = (
          cursor.height -
          departure.height +
          1
        ).toString();
        if (departure.kind === "other") break;
        released.set(
          departure.headerHash,
          departure.kind === "merged"
            ? Object.freeze({
                headerHash: departure.headerHash,
                mergeTransactionHash: departure.transactionHash,
                mergeBlockHash: departure.blockHash,
                mergeSlot: departure.slot.toString(),
                mergeBlockNo: departure.height.toString(),
                confirmationDepth,
              })
            : Object.freeze({
                headerHash: departure.headerHash,
                removalTransactionHash: departure.transactionHash,
                removalKind: departure.kind,
                removalBlockHash: departure.blockHash,
                removalSlot: departure.slot.toString(),
                removalBlockNo: departure.height.toString(),
                confirmationDepth,
              }),
        );
      }
      return released;
    });

  return Object.freeze({ resolveRetainedHeader, resolveMergedHeaders });
};
