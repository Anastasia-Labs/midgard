import { open } from "node:fs/promises";

import type { ChainPoint } from "../domain.js";

export type CardanoNetwork = "Mainnet" | "Preprod" | "Preview" | "Custom";

export type CanonicalChainPoint = ChainPoint & {
  readonly network: string;
  readonly slot: number;
  readonly blockHash: string;
  readonly providerSource: string;
  readonly observedAt: string;
};

export type ChainSyncCursor = {
  readonly sequence: number;
  readonly point: CanonicalChainPoint;
  readonly rollbackGeneration: number;
};

export type ChainSyncEvent =
  | {
      readonly direction: "roll_forward";
      readonly point: CanonicalChainPoint;
    }
  | {
      readonly direction: "roll_backward";
      readonly point: CanonicalChainPoint;
    };

export type ChainSyncEventBatch = {
  readonly event?: ChainSyncEvent;
  readonly tip: CanonicalChainPoint;
};

export interface ChainSyncEventSource {
  next(
    cursor: ChainSyncCursor | undefined,
    intersectionCandidates?: readonly CanonicalChainPoint[],
  ): Promise<ChainSyncEventBatch>;
}

export interface ChainSyncCursorStore {
  load(): Promise<ChainSyncCursor | undefined>;
  append(event: ChainSyncEvent, cursor: ChainSyncCursor): Promise<void>;
  replay(afterSequence: number): Promise<readonly ChainSyncEvent[]>;
  /**
   * The durable cursor journaled with the event at `sequence`, or undefined
   * when the journal does not hold it (pruned, or not yet appended).
   */
  cursorAt(sequence: number): Promise<ChainSyncCursor | undefined>;
  intersectionPoints?(limit: number): Promise<readonly CanonicalChainPoint[]>;
  prune?(consumedSequence: number): Promise<void>;
}

/**
 * Distinct recent points a chain-sync resumption offers the node to intersect
 * with: the node rolls back at most k = 2160 blocks. The journal keeps the
 * entries that hold them, and older entries are pruned once consumed.
 */
export const CHAIN_SYNC_INTERSECTION_POINTS = 2160;

/**
 * Prunable entries a chain-sync journal accumulates before it is rewritten,
 * so the rewrite is amortized over many appends instead of every tick.
 */
export const CHAIN_SYNC_JOURNAL_PRUNE_SLACK = 1080;

/**
 * Events one chain-sync chunk appends at most. A synchronization runs chunk
 * after chunk until it reaches the tip, so this bounds the work between two
 * progress checks, not how far behind a member may be.
 */
export const CHAIN_SYNC_CHUNK_EVENTS = 4096;

/**
 * A chain-sync chunk filled up without moving the durable cursor forward on
 * the chain: the source keeps rolling back and forth instead of delivering the
 * chain. Nothing durable is wrong, so this is retryable, not a quarantine.
 */
export class ChainSyncNoProgressError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "ChainSyncNoProgressError";
  }
}

/** Where a chain-sync that has not reached the tip yet stands. */
export type ChainSyncCatchUpProgress = Readonly<{
  /** Events this synchronization has appended so far. */
  events: number;
  cursorSlot: number;
  tipSlot: number;
}>;

export interface ChainSyncReplayProvider {
  currentChainSyncCursor(): Promise<ChainSyncCursor>;
  replayChainSyncEvents(
    afterSequence: number,
  ): Promise<readonly ChainSyncEvent[]>;
  loadConsumedChainSyncCursor(): Promise<ChainSyncCursor | undefined>;
  acknowledgeChainSyncCursor(
    cursor: ChainSyncCursor,
  ): Promise<ChainSyncAcknowledgement>;
  /** Set while a synchronization is still catching up to the tip. */
  chainSyncCatchUpProgress?(): ChainSyncCatchUpProgress | undefined;
}

/**
 * The outcome of acknowledging a consumed chain-sync cursor. The authority may
 * have moved on since the consumer captured the cursor, rollbacks included:
 * those events come after the acknowledged cursor, so the consumer's next
 * replay delivers them.
 */
export type ChainSyncAcknowledgement = {
  /** A rollback the consumer has not replayed yet follows the cursor. */
  readonly rollbackSinceCapture: boolean;
};

/** Where the durable consumer records how far it has replayed the journal. */
export interface ChainSyncConsumerCursorStore {
  load(): Promise<ChainSyncCursor | undefined>;
  save(cursor: ChainSyncCursor): Promise<void>;
}

export type PersistedChainSyncState = {
  readonly schemaVersion: 2;
  readonly authorityFingerprint: string;
  readonly cursor?: ChainSyncCursor;
};

export type PersistedChainSyncConsumerState = {
  readonly schemaVersion: 1;
  readonly authorityFingerprint: string;
  readonly cursor: ChainSyncCursor;
};

export type PersistedChainSyncJournalEntry = {
  readonly sequence: number;
  readonly event: ChainSyncEvent;
  readonly cursor: ChainSyncCursor;
};

/**
 * The journal as it stands on disk. A line is committed only once its
 * terminating newline is written, so `entries` holds the complete lines and
 * `committedBytes` is where they end; anything past that is an interrupted
 * append that nothing references.
 */
export type CommittedChainSyncJournal = {
  readonly entries: readonly PersistedChainSyncJournalEntry[];
  readonly committedBytes: number;
  readonly totalBytes: number;
};

export const sameCanonicalPoint = (
  left: Pick<CanonicalChainPoint, "network" | "slot" | "blockHash">,
  right: Pick<CanonicalChainPoint, "network" | "slot" | "blockHash">,
): boolean =>
  left.network === right.network &&
  left.slot === right.slot &&
  left.blockHash === right.blockHash;

export const syncFileData = async (path: string): Promise<void> => {
  const handle = await open(path, "r+");
  try {
    await handle.datasync();
  } finally {
    await handle.close();
  }
};

export const truncateFileDurably = async (
  path: string,
  length: number,
): Promise<void> => {
  const handle = await open(path, "r+");
  try {
    await handle.truncate(length);
    await handle.datasync();
  } finally {
    await handle.close();
  }
};

export const safeSlot = (value: unknown, label: string): number => {
  if (!Number.isSafeInteger(value) || (value as number) < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return value as number;
};

export const safeBlockHash = (value: unknown, label: string): string => {
  if (typeof value !== "string" || !/^[0-9a-f]{64}$/u.test(value)) {
    throw new Error(`${label} must be a lowercase 32-byte hex value`);
  }
  return value;
};

export const getRecord = (
  value: unknown,
  label: string,
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  return value as Record<string, unknown>;
};

export const parsePersistedChainSyncState = (
  value: unknown,
  expectedAuthorityFingerprint: string,
): PersistedChainSyncState => {
  const record = getRecord(value, "persisted chain-sync state");
  if (
    record.schemaVersion !== 2 ||
    typeof record.authorityFingerprint !== "string"
  ) {
    throw new Error("persisted chain-sync state has an unsupported schema");
  }
  if (record.authorityFingerprint !== expectedAuthorityFingerprint) {
    throw new Error(
      "persisted chain-sync cursor authority fingerprint does not match the configured local node endpoint",
    );
  }
  const cursor =
    record.cursor === undefined
      ? undefined
      : parsePersistedChainSyncCursor(record.cursor);
  return {
    schemaVersion: 2,
    authorityFingerprint: record.authorityFingerprint,
    ...(cursor === undefined ? {} : { cursor }),
  };
};

export const parsePersistedChainSyncCursor = (
  value: unknown,
): ChainSyncCursor => {
  const cursor = getRecord(value, "persisted chain-sync cursor");
  return {
    sequence: safeSlot(cursor.sequence, "persisted chain-sync sequence"),
    rollbackGeneration: safeSlot(
      cursor.rollbackGeneration,
      "persisted chain-sync rollback generation",
    ),
    point: parsePersistedCanonicalPoint(
      cursor.point,
      "persisted chain-sync point",
    ),
  };
};

export const parsePersistedChainSyncEvent = (
  value: unknown,
): ChainSyncEvent => {
  const event = getRecord(value, "persisted chain-sync event");
  if (
    event.direction !== "roll_forward" &&
    event.direction !== "roll_backward"
  ) {
    throw new Error("persisted chain-sync event has an invalid direction");
  }
  return {
    direction: event.direction,
    point: parsePersistedCanonicalPoint(
      event.point,
      "persisted chain-sync event point",
    ),
  };
};

const parsePersistedCanonicalPoint = (
  value: unknown,
  label: string,
): CanonicalChainPoint => {
  const point = getRecord(value, label);
  if (
    typeof point.network !== "string" ||
    point.network.length === 0 ||
    typeof point.providerSource !== "string" ||
    point.providerSource.length === 0 ||
    typeof point.observedAt !== "string" ||
    !Number.isFinite(Date.parse(point.observedAt))
  ) {
    throw new Error(`${label} has invalid provenance`);
  }
  return {
    network: point.network,
    slot: safeSlot(point.slot, `${label} slot`),
    blockHash: safeBlockHash(point.blockHash, `${label} block hash`),
    providerSource: point.providerSource,
    observedAt: point.observedAt,
  };
};

export const samePersistedEventPoint = (
  event: ChainSyncEvent,
  point: CanonicalChainPoint,
): boolean => samePersistedCanonicalPoint(event.point, point);

export const samePersistedCanonicalPoint = (
  left: CanonicalChainPoint,
  right: CanonicalChainPoint,
): boolean =>
  sameCanonicalPoint(left, right) &&
  left.providerSource === right.providerSource &&
  left.observedAt === right.observedAt;
