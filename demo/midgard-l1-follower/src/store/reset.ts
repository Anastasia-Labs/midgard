import { followerMigrations } from "../schema/follower-migrations.js";
import { applyMigrations } from "../schema/migrate.js";
import {
  asString,
  type Dialect,
  type SqlBackend,
  type SqlTx,
} from "../sql/backend.js";
import type { StoreLocked } from "../types.js";
import { readCursor } from "./rows.js";
import { readWriterStateIn, storeLocked } from "./writer-state.js";

/**
 * The classes reset deletes (§5.1). Class B (own signed material) is never
 * deleted. Class C (immutable, content-addressed: `l1_scripts`, foreign
 * payloads) is kept: a row is correct for any chain, is kept while
 * referenced, and prune removes it once no retained row references it.
 */
export const RESET_CLASSES = ["A", "D-t", "D-x"] as const;

export type ResetResult =
  | Readonly<{
      kind: "reset";
      /** The catalog tables whose rows were deleted, sorted. */
      tables: readonly string[];
      /** The generation the next `initialize` writes. */
      nextGeneration: number;
    }>
  | StoreLocked;

const quote = (name: string): string =>
  name
    .split(".")
    .map((part) => `"${part.replace(/"/gu, '""')}"`)
    .join(".");

/**
 * SQLite applies `ON DELETE` actions even with deferred checks, so a table
 * outside the reset set that references one inside it could lose rows by
 * cascade. Refuse instead, as Postgres `TRUNCATE` does.
 */
const refuseOutsideReferences = async (
  tx: SqlTx,
  tables: ReadonlySet<string>,
): Promise<void> => {
  const all = await tx.query(
    "SELECT name FROM sqlite_master WHERE type = 'table' AND name NOT LIKE 'sqlite_%'",
  );
  for (const row of all) {
    const name = asString(row.name);
    if (tables.has(name.toLowerCase())) continue;
    const keys = await tx.query(
      'SELECT "table" AS target FROM pragma_foreign_key_list(?)',
      [name],
    );
    for (const key of keys)
      if (tables.has(asString(key.target).toLowerCase()))
        throw new Error(
          `table ${name} is outside the reset set but references ${asString(key.target)}; reset refuses to delete rows it depends on`,
        );
  }
};

const resetIn = async (
  tx: SqlTx,
  dialect: Dialect,
): Promise<Exclude<ResetResult, StoreLocked>> => {
  const state = await readWriterStateIn(tx, dialect, "update");
  const cursor = await readCursor(tx, dialect, "update");
  const nextGeneration = Math.max(
    state.nextGeneration,
    cursor === null ? 0 : cursor.generation + 1,
  );
  const tables = (
    await tx.query(
      `SELECT table_name FROM l1_follower_tables WHERE table_class IN (${RESET_CLASSES.map(() => "?").join(", ")}) ORDER BY table_name`,
      [...RESET_CLASSES],
    )
  ).map((row) => asString(row.table_name));
  if (tables.length > 0) {
    if (dialect.name === "postgres")
      // One statement for the whole set: foreign keys among the set are
      // satisfied, and one from any table outside it refuses the reset.
      await tx.exec(`TRUNCATE ${tables.map(quote).join(", ")}`);
    else {
      await refuseOutsideReferences(tx, new Set(tables));
      await tx.exec("PRAGMA defer_foreign_keys = ON");
      for (const table of tables) await tx.exec(`DELETE FROM ${quote(table)}`);
    }
  }
  // The fence: a former holder that never noticed it lost the lease now
  // fails its next write.
  await tx.query(
    "UPDATE l1_follower_writer SET next_generation = ?, writer_epoch = writer_epoch + 1",
    [nextGeneration],
  );
  if (dialect.name === "postgres")
    // Delivered on commit, like a rewind's (§7.1 step 7).
    await tx.query("SELECT pg_notify('l1_generation', ?)", [
      String(nextGeneration),
    ]);
  return { kind: "reset", tables, nextGeneration };
};

/**
 * `follower reset --to-origin` (§7.5 R1, R2, R5): in one transaction, deletes
 * every row of every catalog table of class A, D-t or D-x (facts, seeds, the
 * cursor, the rollback log, and every such role table the follower
 * migrated), and never a class B or class C row or the bookkeeping. The
 * next prune removes the `l1_scripts` rows the reset left unreferenced. The
 * next start initializes at the configured origin and replays from it.
 *
 * Takes the writer lease first and refuses with `store_locked` while a
 * follower holds it. Raises the next generation above every generation the
 * store used and notifies `l1_generation`, so no view taken before the reset
 * validates by generation after it. Idempotent: a second reset deletes
 * nothing and leaves the next generation as it was. D-x rows are deleted;
 * the external stores they version (MPF roots, Level nodes) are not touched.
 */
export const resetToOrigin = async (
  backend: SqlBackend,
): Promise<ResetResult> => {
  const lease = await backend.acquireWriterLease();
  if (lease === null)
    return storeLocked(
      "a running follower holds this store's writer lease; stop it before resetting",
    );
  try {
    await applyMigrations(backend, [followerMigrations(backend.dialect.name)]);
    return await backend.transaction("write", (tx) =>
      resetIn(tx, backend.dialect),
    );
  } finally {
    await lease.release();
  }
};
