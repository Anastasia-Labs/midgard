import {
  type ChainLevel,
  depth,
  type DepthParameters,
  isSafe,
  levelAtDepth,
  type SqlRow,
  type SqlTx,
} from "@al-ft/midgard-l1-follower";

import type { QueueOutputProblem } from "./queue-derivation.js";
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
  problems: readonly QueueOutputProblem[];
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
    problems:
      problems === "" ? [] : (problems.split(",") as QueueOutputProblem[]),
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
  "tx_hash, output_index, kind, asset_name, node_key, next_key, header_hash, da_status, end_time_ms, problems, datum, created_slot, created_height, created_tx_index, spent_slot";

/**
 * The queue outputs live at `atSlot` (default: the cursor, where every row
 * still open is live). Ordered by outref, so the result is a pure function of
 * the facts whatever order the backend keeps rows in.
 */
export const readLiveQueueRows = async (
  tx: SqlTx,
  atSlot?: number,
): Promise<QueueRow[]> => {
  const rows =
    atSlot === undefined
      ? await tx.query(
          `SELECT ${COLUMNS} FROM ${COMMITTEE_QUEUE_TABLE} WHERE spent_slot IS NULL ORDER BY tx_hash, output_index`,
        )
      : await tx.query(
          `SELECT ${COLUMNS} FROM ${COMMITTEE_QUEUE_TABLE} WHERE created_slot <= ? AND (spent_slot IS NULL OR spent_slot > ?) ORDER BY tx_hash, output_index`,
          [atSlot, atSlot],
        );
  return rows.map(toRow);
};

/** Why the landed queue cannot be walked into one list from one root. */
export type QueueUnhealthyReason =
  | "no_root"
  | "multiple_roots"
  /** An output under the queue policy that is not a root or a V1 node. */
  | "invalid_output"
  /** A link names a key no live node has. */
  | "broken_link"
  /** Two live nodes share a key. */
  | "duplicate_key"
  | "cycle"
  /** A live node the walk from the root never reaches. */
  | "orphan_node";

export type QueueNodeStatus = "unattested" | "attested" | "conflicted";

export type LandedQueueNode = Readonly<{
  outRef: string;
  assetName: string;
  nodeKey: string;
  headerHash: string;
  /** As the current scanner classifies it: any problem makes it conflicted. */
  status: QueueNodeStatus;
  daStatus: string;
  problems: readonly QueueOutputProblem[];
  endTimeMs: bigint;
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
  reason: QueueUnhealthyReason | null;
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
  createdSlot: row.createdSlot,
  createdHeight: row.createdHeight,
  datumHex: row.datumHex ?? "",
});

/**
 * The landed queue: the live queue outputs walked from the single root along
 * the links. A pure function of the rows. Anything that is not one list from
 * one root (a malformed queue output, a broken link, a cycle, a node nothing
 * links to) is unhealthy, never silently dropped.
 */
export const walkLandedQueue = (rows: readonly QueueRow[]): LandedQueue => {
  const roots = rows.filter((row) => row.kind === "root");
  const nodes = rows.filter((row) => row.kind === "node");
  const invalid = rows.filter((row) => row.kind === "invalid");
  const unhealthy = (
    reason: QueueUnhealthyReason,
    detail: string,
    root: LandedQueueRoot | null,
    walked: readonly LandedQueueNode[],
  ): LandedQueue => {
    const reached = new Set(walked.map((node) => node.outRef));
    if (root !== null) reached.add(root.outRef);
    return {
      healthy: false,
      reason,
      detail,
      root,
      nodes: walked,
      strays: rows
        .map((row) => row.outRef)
        .filter((outRef) => !reached.has(outRef)),
    };
  };
  const first = roots[0];
  if (first === undefined)
    return unhealthy("no_root", "no live root", null, []);
  const root: LandedQueueRoot = {
    outRef: first.outRef,
    headerHash: first.headerHash ?? "",
    nextKey: first.nextKey,
    createdHeight: first.createdHeight,
  };
  if (roots.length > 1)
    return unhealthy(
      "multiple_roots",
      roots.map((row) => row.outRef).join(" "),
      root,
      [],
    );
  const byKey = new Map<string, QueueRow[]>();
  for (const row of nodes) {
    const key = row.nodeKey ?? "";
    byKey.set(key, [...(byKey.get(key) ?? []), row]);
  }
  const walked: LandedQueueNode[] = [];
  const seen = new Set<string>();
  let key = root.nextKey;
  while (key !== null) {
    if (seen.has(key)) return unhealthy("cycle", key, root, walked);
    seen.add(key);
    const matches = byKey.get(key) ?? [];
    if (matches.length === 0)
      return unhealthy("broken_link", key, root, walked);
    if (matches.length > 1)
      return unhealthy(
        "duplicate_key",
        matches.map((row) => row.outRef).join(" "),
        root,
        walked,
      );
    const row = matches[0]!;
    walked.push(toNode(row));
    key = row.nextKey;
  }
  if (invalid.length > 0)
    return unhealthy(
      "invalid_output",
      invalid.map((row) => `${row.outRef}:${row.problems.join("+")}`).join(" "),
      root,
      walked,
    );
  if (walked.length < nodes.length) {
    const reached = new Set(walked.map((node) => node.outRef));
    return unhealthy(
      "orphan_node",
      nodes
        .filter((row) => !reached.has(row.outRef))
        .map((row) => row.outRef)
        .join(" "),
      root,
      walked,
    );
  }
  return {
    healthy: true,
    reason: null,
    detail: null,
    root,
    nodes: walked,
    strays: [],
  };
};

/** An unattested header in the landed queue, with where it stands. */
export type AwaitingHeader = Readonly<{
  headerHash: string;
  outRef: string;
  endTimeMs: bigint;
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
        depth: atDepth,
        level: levelAtDepth(atDepth, parameters),
        signable: queue.healthy && isSafe(atDepth, parameters),
      };
    });
