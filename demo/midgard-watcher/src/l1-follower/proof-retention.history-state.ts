import type { Cursor, SqlTx } from "@al-ft/midgard-l1-follower";

export type KeyedTable = Readonly<{ table: string; column: string }>;

/**
 * Whether a prune step has reached part of a header's or unit's rows: some
 * row of `key` is closed at or before the slot pruning has reached and no
 * pin holds it. Such a row survives only inside a budget-cut prune step,
 * whose sibling rows may already be deleted, so the rows are not a whole
 * history. The unpinned raw reads apply the same test.
 */
export const partlyPrunedIn = async (
  tx: SqlTx,
  prunedThroughSlot: number,
  key: Buffer,
  rows: readonly KeyedTable[],
): Promise<boolean> => {
  for (const { table, column } of rows) {
    const closed = await tx.query(
      `SELECT 1 AS one FROM ${table} WHERE ${column} = ? AND to_slot IS NOT NULL AND to_slot <= ? LIMIT 1`,
      [key, prunedThroughSlot],
    );
    if (closed.length > 0) return true;
  }
  return false;
};

/**
 * How a header's or unit's rows stand under the cursor lock:
 * - `held`: a pin row already holds them;
 * - `pruned`: a prune step has reached part of them (`partlyPrunedIn`), or
 *   deleted some (`pruned`, the key's record);
 * - `none`: no rows at all;
 * - `whole`: no prune step has reached any of them.
 */
export type HistoryState = "held" | "pruned" | "none" | "whole";

export const historyState = async (
  tx: SqlTx,
  cursor: Cursor,
  key: Buffer,
  held: KeyedTable,
  rows: readonly [KeyedTable, ...KeyedTable[]],
  pruned: (tx: SqlTx, key: Buffer) => Promise<boolean>,
): Promise<HistoryState> => {
  const holds = await tx.query(
    `SELECT 1 AS one FROM ${held.table} WHERE ${held.column} = ? LIMIT 1`,
    [key],
  );
  if (holds.length > 0) return "held";
  if (await pruned(tx, key)) return "pruned";
  const present = await tx.query(
    `SELECT 1 AS one FROM ${rows[0].table} WHERE ${rows[0].column} = ? LIMIT 1`,
    [key],
  );
  if (present.length === 0) return "none";
  return (await partlyPrunedIn(tx, cursor.prunedThroughSlot, key, rows))
    ? "pruned"
    : "whole";
};

/** Whether no prune step has run since the store's origin. */
export const unpruned = (cursor: Cursor): boolean =>
  cursor.prunedThroughSlot <= cursor.origin.slot;
