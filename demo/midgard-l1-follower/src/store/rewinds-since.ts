import {
  asBuffer,
  asNumber,
  type Dialect,
  type SqlTx,
} from "../sql/backend.js";
import type { Point } from "../types.js";
import { readCursor } from "./rows.js";

/**
 * What changed by generation since a reader last handled one: the store's
 * current generation, and the lowest point a rewind or reset after the
 * reader's generation went back to (null when none did).
 */
export type RewindsSince = Readonly<{
  generation: number;
  target: Point | null;
}>;

/**
 * The pull side of the generation notification, for a reader that may have
 * missed it (it subscribed after the store's start, or its process stopped
 * before it handled one): every rewind and every reset of a store with a
 * cursor leaves an `l1_rollbacks` row at its generation, and generations
 * after a cursor's first are consecutive.
 *
 * - `after` null (the reader never handled a generation): the lowest target
 *   of every logged row;
 * - `after` at or above nothing new: no target;
 * - a row missing from `(after, generation]` (pruned past the log's last
 *   1,000 rows, or deleted by a second reset), or `after` above the store's
 *   generation: the origin, the lowest target there is.
 *
 * Null before the store has a cursor.
 */
export const rewindsSinceIn = async (
  tx: SqlTx,
  dialect: Dialect,
  after: number | null,
): Promise<RewindsSince | null> => {
  const cursor = await readCursor(tx, dialect);
  if (cursor === null) return null;
  const generation = cursor.generation;
  if (after !== null && after === generation)
    return { generation, target: null };
  if (after !== null && after > generation)
    return { generation, target: cursor.origin };
  const rows = await tx.query(
    after === null
      ? "SELECT generation, to_slot, to_hash FROM l1_rollbacks WHERE generation <= ? ORDER BY to_slot LIMIT 1"
      : "SELECT generation, to_slot, to_hash FROM l1_rollbacks WHERE generation > ? AND generation <= ? ORDER BY to_slot LIMIT 1",
    after === null ? [generation] : [after, generation],
  );
  if (after !== null) {
    const count = asNumber(
      (
        await tx.query(
          "SELECT count(*) AS n FROM l1_rollbacks WHERE generation > ? AND generation <= ?",
          [after, generation],
        )
      )[0]?.n,
    );
    if (count < generation - after)
      return { generation, target: cursor.origin };
  }
  const lowest = rows[0];
  return {
    generation,
    target:
      lowest === undefined
        ? null
        : { slot: asNumber(lowest.to_slot), hash: asBuffer(lowest.to_hash) },
  };
};
