/**
 * Indexed reads of the intents a slot range or a recording window touches
 * (migration 0003), so S6 and the prune hook read what changed instead of
 * the whole journal. Each read returns tx hashes; the caller derives their
 * states (`deriveIntentClosureIn`).
 */
import {
  asBuffer,
  asNumber,
  asString,
  type Dialect,
  type SqlTx,
} from "../sql/backend.js";
import { chunks, distinctHashes, placeholders } from "./journal.js";

const hashesOf = (rows: readonly Record<string, unknown>[]): Buffer[] =>
  rows.map((row) => asBuffer(row.tx_hash));

/** A slot range `(after, through]`; `after` null means from the start. */
export type SlotRange = Readonly<{ after: number | null; through: number }>;

const rangeClause = (
  column: string,
  range: SlotRange,
): Readonly<{ sql: string; params: number[] }> =>
  range.after === null
    ? { sql: `${column} <= ?`, params: [range.through] }
    : {
        sql: `${column} > ? AND ${column} <= ?`,
        params: [range.after, range.through],
      };

/**
 * The intents that spend, reference or use as collateral an output spent
 * in `range` (by any transaction, the intent itself included): every
 * intent that landed, failed with its collateral taken, or was conflicted
 * there. The spent-slot index and the spend rows' primary key serve it.
 */
export const intentsTouchedBySpendsIn = async (
  tx: SqlTx,
  range: SlotRange,
): Promise<Buffer[]> => {
  const where = rangeClause("o.spent_slot", range);
  return hashesOf(
    await tx.query(
      `SELECT DISTINCT s.tx_hash FROM l1_outputs o
         JOIN l1_intent_spends s ON s.out_tx = o.tx_hash AND s.out_index = o.output_index
        WHERE o.spent_slot IS NOT NULL AND ${where.sql}`,
      where.params,
    ),
  );
};

/** The intents whose `invalid_hereafter` lies in `range`. */
export const intentsExpiringIn = async (
  tx: SqlTx,
  range: SlotRange,
): Promise<Buffer[]> => {
  const where = rangeClause("valid_to_slot", range);
  return hashesOf(
    await tx.query(
      `SELECT tx_hash FROM l1_intents WHERE valid_to_slot IS NOT NULL AND ${where.sql}`,
      where.params,
    ),
  );
};

/** The intents with an abandon event written at a tip slot at or below `through`. */
export const intentsAbandonedThroughIn = async (
  tx: SqlTx,
  through: number,
): Promise<Buffer[]> =>
  hashesOf(
    await tx.query(
      "SELECT DISTINCT tx_hash FROM l1_intent_events WHERE kind = 'abandoned' AND tip_slot <= ?",
      [through],
    ),
  );

/**
 * Every journaled intent that spends, references or uses as collateral an
 * output of one of `parents`, transitively (the parents excluded).
 */
export const dependantsIn = async (
  tx: SqlTx,
  parents: readonly Buffer[],
): Promise<Buffer[]> => {
  const seen = new Set(parents.map((hash) => hash.toString("hex")));
  const found: Buffer[] = [];
  let frontier = distinctHashes(parents);
  while (frontier.length > 0) {
    const next: Buffer[] = [];
    for (const chunk of chunks(frontier))
      for (const hash of hashesOf(
        await tx.query(
          `SELECT DISTINCT tx_hash FROM l1_intent_spends WHERE out_tx IN (${placeholders(chunk.length)})`,
          chunk,
        ),
      )) {
        const key = hash.toString("hex");
        if (seen.has(key)) continue;
        seen.add(key);
        found.push(hash);
        next.push(hash);
      }
    frontier = next;
  }
  return found;
};

/**
 * A position in the order intents became visible in: every intent recorded
 * by a transaction a read at this position did not see is returned by
 * `intentsRecordedSinceIn` from it. Postgres: the snapshot's `xmin`;
 * SQLite: the next recording sequence number.
 */
export type RecordingPosition = string;

export const recordingPositionIn = async (
  tx: SqlTx,
  dialect: Dialect,
): Promise<RecordingPosition> => {
  const rows =
    dialect.name === "postgres"
      ? await tx.query(
          "SELECT pg_snapshot_xmin(pg_current_snapshot())::text AS position",
        )
      : await tx.query(
          "SELECT CAST(next AS TEXT) AS position FROM l1_intent_record_seq",
        );
  return asString(rows[0]?.position);
};

/** The intents recorded at or after `position` (a superset of those not seen there). */
export const intentsRecordedSinceIn = async (
  tx: SqlTx,
  dialect: Dialect,
  position: RecordingPosition,
): Promise<Buffer[]> =>
  hashesOf(
    await tx.query(
      dialect.name === "postgres"
        ? "SELECT tx_hash FROM l1_intents WHERE recorded_seq >= CAST(? AS xid8)"
        : "SELECT tx_hash FROM l1_intents WHERE recorded_seq >= CAST(? AS INTEGER)",
      [position],
    ),
  );

/** Every journaled intent (the prune hook's full derivation). */
export const allIntentsIn = async (tx: SqlTx): Promise<Buffer[]> =>
  hashesOf(await tx.query("SELECT tx_hash FROM l1_intents"));

/**
 * Whether the retained rollback log explains every generation in
 * `(from, to]` as an ordinary rewind: one rollback row each, none of them
 * back to the store's origin. A reset leaves its own row to the origin
 * (`resetIn`) and deletes the rows before it, so a change across a reset is
 * unexplained; so is one across rows the log no longer keeps.
 */
export const rollbacksExplainIn = async (
  tx: SqlTx,
  from: number,
  to: number,
  origin: Readonly<{ slot: number; hash: Buffer }>,
): Promise<boolean> => {
  if (to === from) return true;
  if (to < from) return false;
  const rows = await tx.query(
    "SELECT count(*) AS n FROM l1_rollbacks WHERE generation > ? AND generation <= ? AND NOT (to_slot = ? AND to_hash = ?)",
    [from, to, origin.slot, origin.hash],
  );
  return asNumber(rows[0]?.n) === to - from;
};

/** The generation and boundary the prune hook last ran under (migration 0004). */
export type PruneMark = Readonly<{ generation: number; boundarySlot: number }>;

export const readPruneMarkIn = async (tx: SqlTx): Promise<PruneMark | null> => {
  const row = (
    await tx.query(
      "SELECT generation, boundary_slot FROM l1_intent_prune_mark WHERE id = 1",
    )
  )[0];
  return row === undefined
    ? null
    : {
        generation: asNumber(row.generation),
        boundarySlot: asNumber(row.boundary_slot),
      };
};

export const writePruneMarkIn = async (
  tx: SqlTx,
  mark: PruneMark,
): Promise<void> => {
  await tx.query(
    `INSERT INTO l1_intent_prune_mark (id, generation, boundary_slot) VALUES (1, ?, ?)
     ON CONFLICT (id) DO UPDATE SET generation = excluded.generation, boundary_slot = excluded.boundary_slot`,
    [mark.generation, mark.boundarySlot],
  );
};
