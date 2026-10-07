import type { SqlRow, SqlTx } from "@al-ft/midgard-l1-follower";

import { WATCHER_QUEUE_OUTPUTS_TABLE } from "./tables.js";

/**
 * The watcher's state-queue view at one chain point, read from the
 * projection rows live there. It has the fields of the authenticated
 * observation that later stages read: the queue root to tail, each header's
 * bytes, node, datum, link and minting anchor, and the CorrectionLock.
 */
export type WatcherQueueNode = Readonly<{
  headerHash: string | null;
  outRef: string;
}>;

export type WatcherQueueHeader = Readonly<{
  headerHash: string;
  headerCborHex: string;
  stateQueueNodeCborHex: string;
  linkedListDatumCborHex: string;
  queueOutRef: string;
  nextHeaderHash: string | null;
  observedTransactionHash: string;
  observedBlockHash: string;
  observedSlot: string;
  observedBlockNo: string;
}>;

export type WatcherCorrectionLock = Readonly<{
  outRef: string;
  datumCborHex: string;
  observedTransactionHash: string;
  observedBlockHash: string;
  observedSlot: string;
  observedBlockNo: string;
}>;

/** Why the live rows do not form one canonical queue. */
export type WatcherQueueUnhealthyReason =
  | "malformed_output"
  | "duplicate_root"
  | "missing_root"
  | "duplicate_header"
  | "broken_link"
  | "orphan_node"
  | "duplicate_correction_lock";

export type WatcherQueueView =
  | Readonly<{
      healthy: true;
      queue: readonly WatcherQueueNode[];
      headers: readonly WatcherQueueHeader[];
      correctionLock: WatcherCorrectionLock | null;
    }>
  | Readonly<{
      healthy: false;
      reason: WatcherQueueUnhealthyReason;
      detail: string;
    }>;

const hex = (value: unknown): string =>
  Buffer.from(value as Uint8Array).toString("hex");

const optionalHex = (value: unknown): string | null =>
  value === null || value === undefined ? null : hex(value);

const outRefOf = (row: SqlRow): string =>
  `${hex(row.tx_hash)}#${Number(row.output_index as number | string).toString()}`;

const decimal = (value: unknown): string =>
  BigInt(value as number | string | bigint).toString();

const unhealthy = (
  reason: WatcherQueueUnhealthyReason,
  detail: string,
): WatcherQueueView => ({ healthy: false, reason, detail });

/** The view at the block whose slot is `slot` (rows created at or before it, unspent there). */
export const readWatcherQueueView = async (
  tx: SqlTx,
  slot: number,
): Promise<WatcherQueueView> => {
  const rows = await tx.query(
    `SELECT tx_hash, output_index, kind, header_hash, next_header_hash, header_cbor, state_queue_node_cbor, datum_cbor, malformed, anchor_tx_hash, anchor_slot, anchor_block_hash, anchor_height FROM ${WATCHER_QUEUE_OUTPUTS_TABLE} WHERE from_slot <= ? AND (to_slot IS NULL OR to_slot > ?) ORDER BY tx_hash, output_index`,
    [slot, slot],
  );
  const malformed = rows.find((row) => row.kind === "malformed");
  if (malformed !== undefined)
    return unhealthy(
      "malformed_output",
      `${outRefOf(malformed)}: ${String(malformed.malformed)}`,
    );
  const locks = rows.filter((row) => row.kind === "lock");
  if (locks.length > 1)
    return unhealthy(
      "duplicate_correction_lock",
      locks.map(outRefOf).join(", "),
    );
  const roots = rows.filter((row) => row.kind === "root");
  const nodes = rows.filter((row) => row.kind === "node");
  if (roots.length > 1)
    return unhealthy("duplicate_root", roots.map(outRefOf).join(", "));
  const byHeader = new Map<string, SqlRow>();
  for (const node of nodes) {
    const key = hex(node.header_hash);
    if (byHeader.has(key))
      return unhealthy("duplicate_header", `header ${key} is live twice`);
    byHeader.set(key, node);
  }
  const root = roots[0];
  if (root === undefined && nodes.length > 0)
    return unhealthy(
      "missing_root",
      `${nodes.length.toString()} live nodes and no root`,
    );
  const queue: WatcherQueueNode[] = [];
  const headers: WatcherQueueHeader[] = [];
  if (root !== undefined) {
    queue.push({ headerHash: null, outRef: outRefOf(root) });
    let next = optionalHex(root.next_header_hash);
    while (next !== null) {
      const node = byHeader.get(next);
      if (node === undefined)
        return unhealthy(
          "broken_link",
          `link to ${next} has no live node (after ${queue.at(-1)?.outRef ?? "root"})`,
        );
      byHeader.delete(next);
      const outRef = outRefOf(node);
      const nextHeaderHash = optionalHex(node.next_header_hash);
      queue.push({ headerHash: next, outRef });
      headers.push({
        headerHash: next,
        headerCborHex: hex(node.header_cbor),
        stateQueueNodeCborHex: hex(node.state_queue_node_cbor),
        linkedListDatumCborHex: hex(node.datum_cbor),
        queueOutRef: outRef,
        nextHeaderHash,
        observedTransactionHash: hex(node.anchor_tx_hash),
        observedBlockHash: hex(node.anchor_block_hash),
        observedSlot: decimal(node.anchor_slot),
        observedBlockNo: decimal(node.anchor_height),
      });
      next = nextHeaderHash;
    }
  }
  if (byHeader.size > 0)
    return unhealthy(
      "orphan_node",
      `${byHeader.size.toString()} live nodes are not linked from the root: ${[...byHeader.keys()].join(", ")}`,
    );
  const lock = locks[0];
  return {
    healthy: true,
    queue,
    headers,
    correctionLock:
      lock === undefined
        ? null
        : {
            outRef: outRefOf(lock),
            datumCborHex: hex(lock.datum_cbor),
            observedTransactionHash: hex(lock.anchor_tx_hash),
            observedBlockHash: hex(lock.anchor_block_hash),
            observedSlot: decimal(lock.anchor_slot),
            observedBlockNo: decimal(lock.anchor_height),
          },
  };
};
