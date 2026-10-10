import type { Dialect, SqlTx, SqlValue } from "../sql/backend.js";
import type { View } from "../types.js";
import { readCursor } from "./rows.js";

/** The current view V = (g, P_b) (§8.1), read in the caller's transaction. */
export const currentViewIn = async (
  tx: SqlTx,
  dialect: Dialect,
): Promise<View | null> => {
  const cursor = await readCursor(tx, dialect);
  return cursor === null
    ? null
    : {
        generation: cursor.generation,
        point: cursor.point,
        height: cursor.height,
      };
};

/**
 * The §8.1 validity check as one statement. Returns one row with a `valid`
 * column (boolean on Postgres, 0/1 on SQLite). On Postgres the statement
 * takes `FOR SHARE` on the cursor row, so a rewind cannot commit between
 * the check and the guarded write; on SQLite the caller's transaction must
 * be `BEGIN IMMEDIATE`.
 */
const viewValidQuery = (
  dialect: Dialect,
  view: View,
): Readonly<{ sql: string; params: readonly SqlValue[] }> => ({
  sql: `SELECT (c.generation = ? OR EXISTS (
      SELECT 1 FROM l1_blocks b WHERE b.hash = ? AND b.slot = ?)) AS valid
    FROM l1_follower_cursor c${dialect.lockClause("share")}`,
  params: [view.generation, view.point.hash, view.point.slot],
});

/**
 * `viewValid(V)` (§8.1) in the caller's transaction: no rewind since V was
 * read, or V's point is still on the stored chain.
 */
export const viewValidIn = async (
  tx: SqlTx,
  dialect: Dialect,
  view: View,
): Promise<boolean> => {
  const { sql, params } = viewValidQuery(dialect, view);
  const row = (await tx.query(sql, params))[0];
  return row !== undefined && dialect.readBool(row.valid);
};
