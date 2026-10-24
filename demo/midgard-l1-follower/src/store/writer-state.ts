import { asNumber, type Dialect, type SqlTx } from "../sql/backend.js";
import type { StoreLocked } from "../types.js";

/** The `l1_follower_writer` row (bookkeeping; reset keeps it). */
export type WriterState = Readonly<{
  /** Bumped by every lease holder at start and by reset: the write fence. */
  writerEpoch: number;
  /**
   * The generation the next `initialize` writes. Reset raises it above
   * every generation the store used, so generations never repeat and a
   * view taken before a reset never validates by generation after it.
   */
  nextGeneration: number;
}>;

export const readWriterStateIn = async (
  tx: SqlTx,
  dialect: Dialect,
  lock: "update" | "share",
): Promise<WriterState> => {
  const row = (
    await tx.query(
      `SELECT writer_epoch, next_generation FROM l1_follower_writer${dialect.lockClause(lock)}`,
    )
  )[0];
  if (row === undefined)
    throw new Error("l1_follower_writer has no row; migrate the store first");
  return {
    writerEpoch: asNumber(row.writer_epoch),
    nextGeneration: asNumber(row.next_generation),
  };
};

/**
 * Takes the fence for a new lease holder. Every later write transaction
 * compares the epoch under a share lock, so a write from an older holder
 * that is still in flight either commits before this bump or is refused
 * after it.
 */
export const bumpWriterEpochIn = async (tx: SqlTx): Promise<number> => {
  const row = (
    await tx.query(
      "UPDATE l1_follower_writer SET writer_epoch = writer_epoch + 1 RETURNING writer_epoch",
    )
  )[0];
  if (row === undefined)
    throw new Error("l1_follower_writer has no row; migrate the store first");
  return asNumber(row.writer_epoch);
};

export const storeLocked = (detail: string): StoreLocked => ({
  kind: "store_locked",
  detail,
});

export const HELD_ELSEWHERE =
  "another process holds this store's writer lease (a running follower, or a reset); retry with backoff";
