import {
  asBuffer,
  asNumber,
  type Dialect,
  type SqlTx,
} from "../sql/backend.js";
import type {
  Cursor,
  OutputSummary,
  OutRef,
  Point,
  TrackedSet,
} from "../types.js";
import { insertOutputs, readCursor } from "./rows.js";
import {
  trackedSetItems,
  writeTrackedSetRecordIn,
} from "./tracked-set-record.js";
import { readWriterStateIn } from "./writer-state.js";

export type SeedOutput = Readonly<{ outRef: OutRef; output: OutputSummary }>;

export type SeedResult = Readonly<{
  kind: "seeded";
  /** The cursor the rows were written at (their `seed_slot` is its slot). */
  cursor: Cursor;
  inserted: readonly OutRef[];
  /** Already stored as a row (created or seeded). */
  skipped: readonly OutRef[];
}>;

/**
 * The cursor is no longer at the point the UTxOs were read at: a block or a
 * rewind landed in between, so the read may miss a spend the store has not
 * recorded. Nothing was written; read again at the new cursor.
 */
export type SeedCursorMoved = Readonly<{
  kind: "cursor_moved";
  cursor: Cursor;
}>;

/**
 * Inserts wallet UTxOs read from the ledger as seed rows (§5.3 step 4)
 * through the class A writer: `created_slot NULL`, `seed_slot = at.slot`.
 * The UTxOs must be the ledger's at `at`, which must still be the cursor: a
 * seed row carries no spend, so a spend applied after `at` would be lost.
 * An outref the store already holds a row for is skipped. An output of a
 * stored tx that got no row (its address was not tracked when the block was
 * applied) becomes a seed row; a rewind below `at` deletes it again.
 */
export const insertSeedOutputsIn = async (
  tx: SqlTx,
  dialect: Dialect,
  at: Point,
  outputs: readonly SeedOutput[],
): Promise<SeedResult | SeedCursorMoved | null> => {
  const cursor = await readCursor(tx, dialect, "update");
  if (cursor === null) return null;
  if (cursor.point.slot !== at.slot || !cursor.point.hash.equals(at.hash))
    return { kind: "cursor_moved", cursor };
  const { inserted, skipped } = await insertSeedRowsIn(
    tx,
    dialect,
    at.slot,
    outputs,
  );
  return { kind: "seeded", cursor, inserted, skipped };
};

/**
 * The seed-row write without the cursor check. Only test tooling uses it
 * directly: the fork simulator's fresh-replay reference, to carry the
 * store's seed rows into a store that never saw the seed's cursor, and
 * stand-ins for a followed chain (the testing export).
 */
export const insertSeedRowsIn = async (
  tx: SqlTx,
  dialect: Dialect,
  seedSlot: number,
  outputs: readonly SeedOutput[],
): Promise<{ inserted: OutRef[]; skipped: OutRef[] }> => {
  const inserted: SeedOutput[] = [];
  const skipped: OutRef[] = [];
  for (const seed of outputs) {
    const known = await tx.query(
      "SELECT 1 AS one FROM l1_outputs WHERE tx_hash = ? AND output_index = ?",
      [seed.outRef.txHash, seed.outRef.index],
    );
    if (
      known.length > 0 ||
      inserted.some((other) => sameOutRef(other.outRef, seed.outRef))
    )
      skipped.push(seed.outRef);
    else inserted.push(seed);
  }
  await insertOutputs(
    tx,
    dialect,
    inserted.map(({ outRef, output }) => ({
      outRef,
      output,
      placement: { kind: "seed" as const, seedSlot },
    })),
  );
  return { inserted: inserted.map(({ outRef }) => outRef), skipped };
};

const sameOutRef = (left: OutRef, right: OutRef): boolean =>
  left.index === right.index && left.txHash.equals(right.txHash);

/**
 * Writes the origin block row and the cursor. The generation starts at the
 * writer row's `next_generation`: 0 on a new store, and above every
 * generation used before a reset. Idempotent for the same origin; a
 * different origin on an initialized store is refused. A new cursor records
 * `trackedSet`, the protocol tracked set (`tracked-set-record.ts`).
 */
export const initializeIn = async (
  tx: SqlTx,
  dialect: Dialect,
  origin: Readonly<{ point: Point; height: number }>,
  trackedSet: TrackedSet,
): Promise<
  | { kind: "initialized" | "already_initialized"; cursor: Cursor }
  | { kind: "origin_mismatch"; cursor: Cursor }
> => {
  const existing = await readCursor(tx, dialect, "update");
  if (existing !== null)
    return existing.origin.slot === origin.point.slot &&
      existing.origin.hash.equals(origin.point.hash)
      ? { kind: "already_initialized", cursor: existing }
      : { kind: "origin_mismatch", cursor: existing };
  const { nextGeneration } = await readWriterStateIn(tx, dialect, "share");
  await tx.query(
    "INSERT INTO l1_blocks (slot, hash, height, parent_hash, qualifying_tx_count) VALUES (?, ?, ?, NULL, 0)",
    [origin.point.slot, origin.point.hash, origin.height],
  );
  await tx.query(
    `INSERT INTO l1_follower_cursor (id, slot, hash, height, generation, origin_slot, origin_hash, pruned_through_slot)
     VALUES (?, ?, ?, ?, ?, ?, ?, ?)`,
    [
      dialect.name === "postgres" ? true : 1,
      origin.point.slot,
      origin.point.hash,
      origin.height,
      nextGeneration,
      origin.point.slot,
      origin.point.hash,
      origin.point.slot,
    ],
  );
  await writeTrackedSetRecordIn(tx, trackedSetItems(trackedSet), "keep");
  const cursor = await readCursor(tx, dialect);
  if (cursor === null) throw new Error("cursor row vanished after insert");
  return { kind: "initialized", cursor };
};

/** Keyset-paged load of every live outref (the tracked-outref set, §6 item 5). */
export const loadLiveOutRefs = async (
  page: (after: OutRef | null) => Promise<Record<string, unknown>[]>,
  add: (outRef: OutRef) => void,
): Promise<number> => {
  let after: OutRef | null = null;
  let count = 0;
  for (;;) {
    const rows = await page(after);
    for (const row of rows) {
      const outRef = {
        txHash: asBuffer(row.tx_hash),
        index: asNumber(row.output_index),
      };
      add(outRef);
      after = outRef;
      count += 1;
    }
    if (rows.length === 0) return count;
  }
};
