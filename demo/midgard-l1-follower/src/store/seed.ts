import {
  asBuffer,
  asNumber,
  type Dialect,
  type SqlTx,
} from "../sql/backend.js";
import type { Cursor, OutputSummary, OutRef, Point } from "../types.js";
import { insertOutputs, readCursor } from "./rows.js";

export type SeedOutput = Readonly<{ outRef: OutRef; output: OutputSummary }>;

export type SeedResult = Readonly<{
  kind: "seeded";
  inserted: readonly OutRef[];
  /** Already stored, or created by a tx the follower stored (INV6). */
  skipped: readonly OutRef[];
}>;

/**
 * Inserts pre-origin wallet UTxOs as seed rows (§5.3 step 4) through the
 * class A writer: `created_slot NULL`, `seed_slot = seedSlot`. An outref that
 * is already stored, or whose creating tx the follower stored, is skipped.
 */
export const insertSeedOutputsIn = async (
  tx: SqlTx,
  dialect: Dialect,
  seedSlot: number,
  outputs: readonly SeedOutput[],
): Promise<SeedResult | null> => {
  const cursor = await readCursor(tx, dialect, "update");
  if (cursor === null) return null;
  const inserted: SeedOutput[] = [];
  const skipped: OutRef[] = [];
  for (const seed of outputs) {
    const known = await tx.query(
      `SELECT 1 AS one FROM l1_outputs WHERE tx_hash = ? AND output_index = ?
       UNION ALL SELECT 1 AS one FROM l1_txs WHERE tx_hash = ?`,
      [seed.outRef.txHash, seed.outRef.index, seed.outRef.txHash],
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
  return {
    kind: "seeded",
    inserted: inserted.map(({ outRef }) => outRef),
    skipped,
  };
};

const sameOutRef = (left: OutRef, right: OutRef): boolean =>
  left.index === right.index && left.txHash.equals(right.txHash);

/**
 * Writes the origin block row and the cursor (generation 0). Idempotent for
 * the same origin; a different origin on an initialized store is refused.
 */
export const initializeIn = async (
  tx: SqlTx,
  dialect: Dialect,
  origin: Readonly<{ point: Point; height: number }>,
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
  await tx.query(
    "INSERT INTO l1_blocks (slot, hash, height, parent_hash, qualifying_tx_count) VALUES (?, ?, ?, NULL, 0)",
    [origin.point.slot, origin.point.hash, origin.height],
  );
  await tx.query(
    `INSERT INTO l1_follower_cursor (id, slot, hash, height, generation, origin_slot, origin_hash, pruned_through_slot)
     VALUES (?, ?, ?, ?, 0, ?, ?, ?)`,
    [
      dialect.name === "postgres" ? true : 1,
      origin.point.slot,
      origin.point.hash,
      origin.height,
      origin.point.slot,
      origin.point.hash,
      origin.point.slot,
    ],
  );
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
