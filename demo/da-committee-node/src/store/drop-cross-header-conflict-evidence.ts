import type { Pool } from "pg";

import type { StoreData } from "../store.committee-store.js";
import { normalizeStoreData } from "../store.normalize-store-data.js";

// Owner ruling 2026-10-07: a signature pair over two different header hashes
// is not equivocation (docs/midgard/decisions/
// da-sibling-signatures-not-slashable.md). Builds before the ruling could store
// one relayed by a peer; the codec now refuses it, so such a row would throw
// out of every conflict-evidence read (recovery, retention). Stores drop them
// on open. The conflicting_header_hash column itself goes with the C2 schema
// rework.

const warnDropped = (store: "json" | "postgres", count: number): void => {
  process.stderr.write(
    `${JSON.stringify({
      level: "warn",
      event: "committee_store_dropped_cross_header_conflict_evidence",
      store,
      count,
    })}\n`,
  );
};

/** Deletes cross-header conflict-evidence rows in one transaction; idempotent. */
export const dropCrossHeaderConflictEvidenceRows = async (
  pool: Pool,
): Promise<number> => {
  const client = await pool.connect();
  try {
    await client.query("BEGIN");
    await client.query("SELECT pg_advisory_xact_lock(172947,725726)");
    const result = await client.query(
      `DELETE FROM committee_da_conflict_evidence
       WHERE header_hash <> conflicting_header_hash`,
    );
    await client.query("COMMIT");
    const count = result.rowCount ?? 0;
    if (count > 0) warnDropped("postgres", count);
    return count;
  } catch (error) {
    await client.query("ROLLBACK").catch(() => undefined);
    throw error;
  } finally {
    client.release();
  }
};

/**
 * Normalizes raw JSON store data after removing cross-header conflict
 * evidence. When `persist` is given and records were removed, the cleaned
 * state is written back through it.
 */
export const dropCrossHeaderConflicts = async (
  raw: unknown,
  persist: false | ((data: StoreData) => Promise<void>),
): Promise<StoreData> => {
  const evidence = (raw as { daConflictEvidence?: unknown } | null)
    ?.daConflictEvidence;
  if (
    typeof evidence !== "object" ||
    evidence === null ||
    Array.isArray(evidence)
  )
    return normalizeStoreData(raw);
  const kept = Object.entries(evidence).filter(([, record]) => {
    const row = record as {
      headerHash?: unknown;
      conflictingHeaderHash?: unknown;
    };
    return row?.headerHash === row?.conflictingHeaderHash;
  });
  const dropped = Object.keys(evidence).length - kept.length;
  if (dropped === 0) return normalizeStoreData(raw);
  const data = normalizeStoreData({
    ...(raw as object),
    daConflictEvidence: Object.fromEntries(kept),
  });
  if (persist !== false) {
    await persist(data);
    warnDropped("json", dropped);
  }
  return data;
};
