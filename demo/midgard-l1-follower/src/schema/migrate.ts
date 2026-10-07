import { createHash } from "node:crypto";

import { asString, type SqlBackend } from "../sql/backend.js";

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

export const MIGRATION_LEDGER_DDL = `
-- class: A; retention: one row per applied migration, forever
CREATE TABLE IF NOT EXISTS l1_follower_migrations (
  namespace text NOT NULL,
  id text NOT NULL,
  checksum text NOT NULL,
  PRIMARY KEY (namespace, id)
);
`;

const checksum = (sql: string): string =>
  createHash("sha256").update(sql).digest("hex");

/**
 * Applies each set's pending migrations in order, in one write transaction
 * (on Postgres under a transaction-scoped advisory lock, so two processes
 * never migrate concurrently). An applied migration whose text changed is
 * refused: an undeployed schema is replaced in place, never edited under a
 * running store.
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
    await tx.exec(MIGRATION_LEDGER_DDL);
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
          continue;
        }
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
