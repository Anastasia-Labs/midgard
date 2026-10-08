import { createHash } from "node:crypto";

import { asString, type SqlBackend, type SqlTx } from "../sql/backend.js";
import { migrationTables } from "./lint.js";

export type Migration = Readonly<{ id: string; sql: string }>;

/**
 * Migrations under one namespace. The follower's own set is `l1-follower`;
 * a role registers its D-t tables under its own namespace. The ledger table
 * is separate from every role's `schema_migrations`, so the follower's
 * tables can live in a role's existing database without colliding.
 */
export type MigrationSet = Readonly<{
  namespace: string;
  migrations: readonly Migration[];
}>;

export class FollowerMigrationError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "FollowerMigrationError";
  }
}

/**
 * The follower's bookkeeping, created before any migration and outside the
 * catalog, so `follower reset --to-origin` never deletes it: the migration
 * ledger, the table catalog (each migrated table's declared class, which
 * reset reads), the writer row (the lease's fencing epoch and the
 * generation the next `initialize` starts at), and the tracked-set record
 * (the protocol tracked set the facts were built under, and whether a
 * tracked-set reset is still replaying; `store/tracked-set-record.ts`).
 */
export const FOLLOWER_BOOKKEEPING_DDL = `
-- class: A; retention: one row per applied migration, forever; reset keeps it
CREATE TABLE IF NOT EXISTS l1_follower_migrations (
  namespace text NOT NULL,
  id text NOT NULL,
  checksum text NOT NULL,
  PRIMARY KEY (namespace, id)
);

-- class: A; retention: one row per migrated table, forever; reset keeps it
CREATE TABLE IF NOT EXISTS l1_follower_tables (
  table_name text PRIMARY KEY,
  table_class text NOT NULL,
  namespace text NOT NULL,
  migration text NOT NULL
);

-- class: A; retention: one row forever; reset keeps it
CREATE TABLE IF NOT EXISTS l1_follower_writer (
  id integer PRIMARY KEY CHECK (id = 1),
  writer_epoch bigint NOT NULL,
  next_generation bigint NOT NULL
);

INSERT INTO l1_follower_writer (id, writer_epoch, next_generation)
  SELECT 1, 0, 0 WHERE NOT EXISTS (SELECT 1 FROM l1_follower_writer);

-- class: A; retention: one row forever; reset keeps it (and rewrites it on a tracked-set reset)
CREATE TABLE IF NOT EXISTS l1_follower_tracked_set (
  id integer PRIMARY KEY CHECK (id = 1),
  addresses text NOT NULL,
  payment_credentials text NOT NULL,
  policies text NOT NULL,
  replaying integer NOT NULL
);
`;

const checksum = (sql: string): string =>
  createHash("sha256").update(sql).digest("hex");

/** Records each table a migration declares, with its class, in the catalog. */
const catalogTables = async (
  tx: SqlTx,
  namespace: string,
  migration: Migration,
): Promise<void> => {
  const { declared, problems } = migrationTables(
    namespace,
    migration.id,
    migration.sql,
  );
  const problem = problems[0];
  if (problem !== undefined)
    throw new FollowerMigrationError(
      `migration ${namespace}/${migration.id}${
        problem.table === null ? "" : ` table ${problem.table}`
      }: ${problem.message}`,
    );
  for (const table of declared)
    await tx.query(
      `INSERT INTO l1_follower_tables (table_name, table_class, namespace, migration)
       SELECT ?, ?, ?, ? WHERE NOT EXISTS (SELECT 1 FROM l1_follower_tables WHERE table_name = ?)`,
      [table.table, table.tableClass, namespace, migration.id, table.table],
    );
};

/**
 * Applies each set's pending migrations in order, in one write transaction
 * (on Postgres under a transaction-scoped advisory lock, so two processes
 * never migrate concurrently). An applied migration whose text changed is
 * refused: an undeployed schema is replaced in place, never edited under a
 * running store. A migration with a table that lacks its class header is
 * refused, and every declared table is recorded in the catalog.
 */
export const applyMigrations = async (
  backend: SqlBackend,
  sets: readonly MigrationSet[],
): Promise<{ applied: string[] }> =>
  backend.transaction("write", async (tx) => {
    if (backend.dialect.name === "postgres")
      await tx.query(
        "SELECT pg_advisory_xact_lock(hashtext('midgard-l1-follower:migrations'))",
      );
    await tx.exec(FOLLOWER_BOOKKEEPING_DDL);
    const applied: string[] = [];
    for (const set of sets) {
      const ids = new Set<string>();
      for (const migration of set.migrations) {
        if (ids.has(migration.id))
          throw new FollowerMigrationError(
            `duplicate migration ${set.namespace}/${migration.id}`,
          );
        ids.add(migration.id);
        const digest = checksum(migration.sql);
        const rows = await tx.query(
          "SELECT checksum FROM l1_follower_migrations WHERE namespace = ? AND id = ?",
          [set.namespace, migration.id],
        );
        const existing = rows[0];
        if (existing !== undefined) {
          if (asString(existing.checksum) !== digest)
            throw new FollowerMigrationError(
              `migration ${set.namespace}/${migration.id} changed after it was applied`,
            );
          await catalogTables(tx, set.namespace, migration);
          continue;
        }
        await catalogTables(tx, set.namespace, migration);
        await tx.exec(migration.sql);
        await tx.query(
          "INSERT INTO l1_follower_migrations (namespace, id, checksum) VALUES (?, ?, ?)",
          [set.namespace, migration.id, digest],
        );
        applied.push(`${set.namespace}/${migration.id}`);
      }
    }
    return { applied };
  });
