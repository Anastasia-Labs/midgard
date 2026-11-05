import {
  type ChainLevel,
  depth,
  type DepthParameters,
  isSafe,
  levelAtDepth,
  type LinkedQueueEntry,
  type LinkedQueueUnhealthyReason,
  type SqlRow,
  type SqlTx,
  walkLinkedQueue,
} from "@al-ft/midgard-l1-follower";
import type { StateQueueOutputProblem } from "@al-ft/midgard-sdk";

import { COMMITTEE_QUEUE_TABLE } from "./queue-table.js";

/** One `committee_queue_outputs` row. Hex fields are lowercase. */
export type QueueRow = Readonly<{
  /** `<tx hash hex>#<index>`, as the current scanner labels outputs. */
  outRef: string;
  kind: "root" | "node" | "invalid";
  assetName: string | null;
  nodeKey: string | null;
  nextKey: string | null;
  headerHash: string | null;
  daStatus: string | null;
  endTimeMs: bigint | null;
  blockTimeMs: bigint | null;
  problems: readonly StateQueueOutputProblem[];
  datumHex: string | null;
  createdSlot: number;
  createdHeight: number;
  createdTxIndex: number;
  spentSlot: number | null;
}>;

const asNumber = (value: unknown): number => {
  const parsed = typeof value === "number" ? value : Number(value as string);
  if (!Number.isSafeInteger(parsed))
    throw new Error(`committee queue row: not an integer: ${String(value)}`);
  return parsed;
};

const asText = (value: unknown): string | null => {
  if (value === null || value === undefined) return null;
  if (typeof value !== "string")
    throw new Error(`committee queue row: not text: ${typeof value}`);
  return value;
};

const asBytes = (value: unknown): Buffer | null =>
  value === null || value === undefined
    ? null
    : Buffer.from(value as Uint8Array);

const asBigInt = (value: unknown): bigint | null =>
  value === null || value === undefined
    ? null
    : BigInt(value as string | number | bigint);

const toRow = (row: SqlRow): QueueRow => {
  const txHash = asBytes(row.tx_hash);
  if (txHash === null) throw new Error("committee queue row has no tx hash");
  const kind = asText(row.kind);
  if (kind !== "root" && kind !== "node" && kind !== "invalid")
    throw new Error(`committee queue row has kind ${kind}`);
  const problems = asText(row.problems) ?? "";
  return {
    outRef: `${txHash.toString("hex")}#${asNumber(row.output_index).toString()}`,
    kind,
    assetName: asText(row.asset_name),
    nodeKey: asText(row.node_key),
    nextKey: asText(row.next_key),
    headerHash: asText(row.header_hash),
    daStatus: asText(row.da_status),
    endTimeMs: asBigInt(row.end_time_ms),
    blockTimeMs: asBigInt(row.block_time_ms),
    problems:
      problems === "" ? [] : (problems.split(",") as StateQueueOutputProblem[]),
    datumHex: asBytes(row.datum)?.toString("hex") ?? null,
    createdSlot: asNumber(row.created_slot),
    createdHeight: asNumber(row.created_height),
    createdTxIndex: asNumber(row.created_tx_index),
    spentSlot:
      row.spent_slot === null || row.spent_slot === undefined
        ? null
        : asNumber(row.spent_slot),
  };
};

const COLUMNS =
  "tx_hash, output_index, kind, asset_name, node_key, next_key, header_hash, da_status, end_time_ms, block_time_ms, problems, datum, created_slot, created_height, created_tx_index, spent_slot";

/**
 * The queue outputs live at the cursor: every row still open. Ordered by
 * outref, so the result is a pure function of the facts whatever order the
 * backend keeps rows in.
 */
export const readLiveQueueRows = async (tx: SqlTx): Promise<QueueRow[]> =>
  (
    await tx.query(
      `SELECT ${COLUMNS} FROM ${COMMITTEE_QUEUE_TABLE} WHERE spent_slot IS NULL ORDER BY tx_hash, output_index`,
    )
  ).map(toRow);

/**
 * The header hashes the queue outputs live at `atSlot` carry (roots and
 * nodes; an invalid output carries none), sorted.
 */
export const readQueueHeaderHashesAt = async (
  tx: SqlTx,
  atSlot: number,
): Promise<string[]> =>
  (
    await tx.query(
      `SELECT DISTINCT header_hash FROM ${COMMITTEE_QUEUE_TABLE} WHERE kind <> 'invalid' AND header_hash IS NOT NULL AND created_slot <= ? AND (spent_slot IS NULL OR spent_slot > ?)`,
      [atSlot, atSlot],
    )
  )
    .map((row) => String(row.header_hash))
    .sort((a, b) => (a < b ? -1 : a > b ? 1 : 0));

export type QueueNodeStatus = "unattested" | "attested" | "conflicted";

export type LandedQueueNode = Readonly<{
  outRef: string;
  assetName: string;
  nodeKey: string;
  headerHash: string;
  /** As the current scanner classifies it: any problem makes it conflicted. */
  status: QueueNodeStatus;
  daStatus: string;
  problems: readonly StateQueueOutputProblem[];
  endTimeMs: bigint;
  blockTimeMs: bigint | null;
  createdSlot: number;
  createdHeight: number;
  datumHex: string;
}>;

export type LandedQueueRoot = Readonly<{
  outRef: string;
  headerHash: string;
  nextKey: string | null;
  createdHeight: number;
}>;

export type LandedQueue = Readonly<{
  healthy: boolean;
  /** Null when healthy. */
  reason: LinkedQueueUnhealthyReason | null;
  detail: string | null;
  root: LandedQueueRoot | null;
  /** The walk from the root, in list order (cut where the walk failed). */
  nodes: readonly LandedQueueNode[];
  /** Live queue outputs the walk did not reach, by outref. */
  strays: readonly string[];
}>;

const nodeStatus = (row: QueueRow): QueueNodeStatus =>
  row.problems.length > 0
    ? "conflicted"
    : row.daStatus === "Unattested"
      ? "unattested"
      : "attested";

const toNode = (row: QueueRow): LandedQueueNode => ({
  outRef: row.outRef,
  assetName: row.assetName ?? "",
  nodeKey: row.nodeKey ?? "",
  headerHash: row.headerHash ?? "",
  status: nodeStatus(row),
  daStatus: row.daStatus ?? "",
  problems: row.problems,
  endTimeMs: row.endTimeMs ?? 0n,
  blockTimeMs: row.blockTimeMs,
  createdSlot: row.createdSlot,
  createdHeight: row.createdHeight,
  datumHex: row.datumHex ?? "",
});

type Entry = LinkedQueueEntry & Readonly<{ row: QueueRow }>;

/**
 * The landed queue: the live queue outputs walked from the single root along
 * the links with the follower package's shared walk. A pure function of the
 * rows. Anything that is not one list from one root (a malformed queue
 * output, a broken link, a cycle, a node nothing links to) is unhealthy,
 * never silently dropped. An invalid output's detail names its problems.
 */
export const walkLandedQueue = (rows: readonly QueueRow[]): LandedQueue => {
  const walk = walkLinkedQueue(
    rows.map(
      (row): Entry => ({
        id: row.outRef,
        kind: row.kind,
        key: row.nodeKey,
        next: row.nextKey,
        row,
      }),
    ),
  );
  const root = walk.root?.row;
  return {
    healthy: walk.healthy,
    reason: walk.reason,
    detail:
      walk.reason === "invalid_output"
        ? rows
            .filter((row) => row.kind === "invalid")
            .map((row) => `${row.outRef}:${row.problems.join("+")}`)
            .join(" ")
        : walk.detail,
    root:
      root === undefined
        ? null
        : {
            outRef: root.outRef,
            headerHash: root.headerHash ?? "",
            nextKey: root.nextKey,
            createdHeight: root.createdHeight,
          },
    nodes: walk.nodes.map((entry) => toNode(entry.row)),
    strays: walk.strays,
  };
};

/** An unattested header in the landed queue, with where it stands. */
export type AwaitingHeader = Readonly<{
  headerHash: string;
  outRef: string;
  endTimeMs: bigint;
  blockTimeMs: bigint | null;
  /** Of the node's current output (plan §9: the tip block has depth 1). */
  depth: number;
  level: ChainLevel | null;
  /**
   * The queue is healthy and the node's output is `safe` (depth ≥ cd).
   * Liveness only: signing is truthful on any fork (C4), so a signature
   * needs no durability, only a reason to believe the header is worth it.
   */
  signable: boolean;
}>;

/**
 * Headers awaiting attestation: the landed queue's unattested nodes with
 * their depth and level under the tip at `tipHeight`. A pure function of
 * the landed queue and the tip.
 */
export const headersAwaitingAttestation = (
  queue: LandedQueue,
  tipHeight: number,
  parameters: DepthParameters,
): AwaitingHeader[] =>
  queue.nodes
    .filter((node) => node.status === "unattested")
    .map((node) => {
      const atDepth = depth(tipHeight, node.createdHeight);
      return {
        headerHash: node.headerHash,
        outRef: node.outRef,
        endTimeMs: node.endTimeMs,
        blockTimeMs: node.blockTimeMs,
        depth: atDepth,
        level: levelAtDepth(atDepth, parameters),
        signable: queue.healthy && isSafe(atDepth, parameters),
      };
    });
