/**
 * Databases for the extra nodes of an L2 transaction sharing test: each one
 * fresh, migrated and owned by this worktree's test prefix.
 */
import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Duration, Effect, Layer, Redacted } from "effect";

import { MigrationRunner } from "../../src/database/index.js";
import { Database } from "../../src/services/database.js";
import { NodeConfig } from "../../src/services/index.js";
import { TEST_DATABASE_PREFIX } from "../test-env.js";

const maintenanceLayer = (database: string) =>
  PgClient.layer({
    host: process.env.POSTGRES_HOST ?? "127.0.0.1",
    port: Number(process.env.POSTGRES_PORT ?? "5433"),
    username: process.env.POSTGRES_USER ?? "postgres",
    password: Redacted.make(process.env.POSTGRES_PASSWORD ?? "postgres"),
    database,
    maxConnections: 1,
    connectTimeout: Duration.seconds(5),
  });

/** Runs `build` with `POSTGRES_DB` naming `database`, then restores it. */
export const withPostgresDb = async <A>(
  database: string,
  build: () => Promise<A>,
): Promise<A> => {
  const before = process.env.POSTGRES_DB;
  process.env.POSTGRES_DB = database;
  try {
    return await build();
  } finally {
    process.env.POSTGRES_DB = before;
  }
};

/**
 * A fresh, migrated database for a second node, owned by this worktree's
 * test prefix; `drop` removes it.
 */
export const createPeerDatabase = async (label: string) => {
  const database = `${TEST_DATABASE_PREFIX}_${label}_${process.pid.toString()}`;
  if (!/^[a-z_][a-z0-9_]{0,62}$/u.test(database))
    throw new Error(`Invalid peer database name ${database}`);
  const admin = <A, E>(work: Effect.Effect<A, E, SqlClient.SqlClient>) =>
    Effect.runPromise(Effect.provide(work, maintenanceLayer("postgres")));
  await admin(
    Effect.flatMap(SqlClient.SqlClient, (sql) =>
      sql
        .unsafe(`DROP DATABASE IF EXISTS "${database}"`)
        .pipe(
          Effect.zipRight(sql.unsafe(`CREATE DATABASE "${database}"`)),
          Effect.zipRight(
            sql.unsafe(
              `ALTER DATABASE "${database}" SET synchronous_commit = on`,
            ),
          ),
        ),
    ),
  );
  await withPostgresDb(database, () =>
    Effect.runPromise(
      MigrationRunner.migrate({
        appVersion: "midgard-node-l2-peer-test",
        actor: "midgard-node-l2-peer-test",
      }).pipe(
        Effect.provide(Layer.provideMerge(Database.layer, NodeConfig.layer)),
      ),
    ),
  );
  return {
    database,
    drop: () =>
      admin(
        Effect.flatMap(SqlClient.SqlClient, (sql) =>
          sql.unsafe(`DROP DATABASE IF EXISTS "${database}"`),
        ),
      ),
  };
};
