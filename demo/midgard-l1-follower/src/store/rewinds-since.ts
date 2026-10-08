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
 * - `after` at the store's generation: no target;
 * - otherwise, over `(after, generation]` (`after` null reads as 0: the
 *   reader never handled a generation, and no row has generation 0), the
 *   lowest target of the logged rows;
 * - a row missing from that range (pruned past the log's last 1,000 rows,
 *   or deleted by a second reset), or `after` above the store's
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
  const from = after ?? 0;
  if (from === generation) return { generation, target: null };
  if (from > generation) return { generation, target: cursor.origin };
  const count = asNumber(
    (
      await tx.query(
        "SELECT count(*) AS n FROM l1_rollbacks WHERE generation > ? AND generation <= ?",
        [from, generation],
      )
    )[0]?.n,
  );
  if (count < generation - from) return { generation, target: cursor.origin };
  const lowest = (
    await tx.query(
      "SELECT to_slot, to_hash FROM l1_rollbacks WHERE generation > ? AND generation <= ? ORDER BY to_slot LIMIT 1",
      [from, generation],
    )
  )[0];
  return {
    generation,
    target:
      lowest === undefined
        ? null
        : { slot: asNumber(lowest.to_slot), hash: asBuffer(lowest.to_hash) },
  };
};
