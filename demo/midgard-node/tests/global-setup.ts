/**
 * Provision one Postgres database per test worker, once per run.
 *
 * The suite shards its database by worker slot so that database-touching files
 * can run in parallel (tests/test-env.ts explains the invariant). Each shard
 * needs three things before a worker can use it:
 *
 *   1. the database itself;
 *   2. `synchronous_commit = on` as a per-database setting — CI runs `on`, and
 *      `database.test.ts` / `tx-admissions-claim-load.test.ts` assert the
 *      session value is `on` to prove the write-behind's
 *      `SET LOCAL synchronous_commit = off` relaxation stays transaction-local.
 *      `scripts/start-test-postgres.sh` starts the local server with `on`
 *      server-wide, but a server started some other way may default to `off`.
 *      Per-database settings are NOT inherited through
 *      `CREATE DATABASE ... TEMPLATE`, so every shard needs its own `ALTER`;
 *   3. the schema — most database files migrate themselves in `beforeAll`, but
 *      not all of them do (`da-publication-reconciler-e2e.test.ts` relied on
 *      CI's separate `db:migrate` step against a single shared database), and a
 *      fresh shard has no schema to inherit.
 */
import { spawnSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Effect, Layer, Redacted } from "effect";

import { MigrationRunner } from "../src/database/index.js";
import { MigrationError } from "../src/database/migrations/runner.js";
import { NodeConfig } from "../src/services/config.js";
import { Database } from "../src/services/database.js";
import {
  nativeOwnerBinaryPath,
  nativeOwnerBinaryPresent,
} from "./helpers/native-owner-binary.js";
import { applyMidgardNodeTestEnv, testDatabaseNames } from "./test-env.js";

const maintenanceClient = (database: string) =>
  PgClient.layer({
    host: process.env.POSTGRES_HOST ?? "127.0.0.1",
    port: Number(process.env.POSTGRES_PORT ?? "5433"),
    username: process.env.POSTGRES_USER ?? "postgres",
    password: Redacted.make(process.env.POSTGRES_PASSWORD ?? "postgres"),
    database,
    maxConnections: 1,
    applicationName: "midgard-node-test-global-setup",
  });

const createShard = (shard: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const existing = yield* sql<{
      readonly datname: string;
    }>`SELECT datname FROM pg_database WHERE datname = ${shard}`;
    if (existing.length === 0) {
      // Unsafe interpolation is unavoidable — CREATE DATABASE takes no
      // parameters — so the name is generated, never operator-supplied.
      yield* sql.unsafe(`CREATE DATABASE "${shard}"`);
    }
    yield* sql.unsafe(`ALTER DATABASE "${shard}" SET synchronous_commit = on`);
  }).pipe(Effect.provide(maintenanceClient("postgres")));

const dropShard = (shard: string) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    // Plain DROP (no FORCE) refuses a shard another process is connected to,
    // so a concurrent run on that shard fails loudly instead of being killed.
    yield* sql.unsafe(`DROP DATABASE IF EXISTS "${shard}"`);
  }).pipe(Effect.provide(maintenanceClient("postgres")));

const migrateShard = (shard: string) =>
  Effect.gen(function* () {
    process.env.POSTGRES_DB = shard;
    yield* MigrationRunner.migrate({
      appVersion: "midgard-node-test-global-setup",
      actor: "midgard-node-test-global-setup",
    }).pipe(
      Effect.provide(Layer.provideMerge(Database.layer, NodeConfig.layer)),
    );
  });

/**
 * Migration codes that mean the shard's applied schema belongs to a different
 * revision of the migration set than the one under test — a checkout that
 * edited a migration, or another worktree that migrated the shard ahead. The
 * runner refuses such a database because a production database must never be
 * silently rewritten; a test shard has no data worth keeping, so provisioning
 * recreates it instead of leaving the whole suite red until someone drops it
 * by hand. Every other migration failure (connection, SQL, lock) still throws.
 */
const STALE_SHARD_SCHEMA_CODES: ReadonlySet<string> = new Set([
  "schema_checksum_mismatch",
  "schema_manifest_hash_mismatch",
  "schema_name_mismatch",
  "schema_version_ahead",
]);

const isStaleShardSchema = (error: unknown): error is MigrationError =>
  error instanceof MigrationError && STALE_SHARD_SCHEMA_CODES.has(error.code);

const provisionShard = (shard: string) =>
  createShard(shard).pipe(
    Effect.andThen(migrateShard(shard)),
    Effect.catchIf(isStaleShardSchema, (error) =>
      Effect.gen(function* () {
        process.stderr.write(
          `[global-setup] ${shard}: ${error.message} (${error.code}) — recreating the shard for the current migration set\n`,
        );
        yield* dropShard(shard);
        yield* createShard(shard);
        yield* migrateShard(shard);
      }),
    ),
  );

/**
 * Build the native architecture-G owner binary the mpf-differential and
 * mpf-native-owner-service files spawn. The dedicated scripts
 * (`test:mpf:differential`, `test:mpf-native-owner`) build it themselves, but
 * the plain `test` run never did, so a fresh worktree failed those files on a
 * missing artifact (#642). With a warm cargo target dir `--locked` rebuilds
 * are sub-second; a cold build pays once per worktree.
 *
 * Only a MISSING toolchain is non-fatal: without cargo (or with
 * MIDGARD_SKIP_NATIVE_BUILD=1) the two consumer files skip LOUDLY with the
 * build command in the reason — never a silent pass. But cargo present with a
 * FAILING build throws: that is a compile regression in the crate, and letting
 * it degrade seven real assertions into skips would hide it.
 */
const buildNativeOwnerBinary = (): void => {
  if (process.env.MIDGARD_SKIP_NATIVE_BUILD === "1") {
    process.stderr.write(
      "[global-setup] MIDGARD_SKIP_NATIVE_BUILD=1 — native architecture-g-owner build skipped; dependent files will skip loudly\n",
    );
    return;
  }
  const packageRoot = resolve(dirname(fileURLToPath(import.meta.url)), "..");
  const cargoProbe = spawnSync("cargo", ["--version"], { stdio: "ignore" });
  if (cargoProbe.error !== undefined || cargoProbe.status !== 0) {
    process.stderr.write(
      "[global-setup] cargo is unavailable — native architecture-g-owner build skipped; dependent files will skip loudly unless the binary already exists\n",
    );
    return;
  }
  // The package script is the single source of truth for the build argv.
  const result = spawnSync("pnpm", ["run", "native:mpf-owner:build"], {
    cwd: packageRoot,
    stdio: ["ignore", "pipe", "pipe"],
    encoding: "utf8",
  });
  if (result.error !== undefined || result.status !== 0) {
    const detail =
      result.error !== undefined
        ? result.error.message
        : (result.stderr ?? "").split("\n").slice(-15).join("\n");
    throw new Error(
      `[global-setup] native architecture-g-owner build FAILED with cargo available — a compile regression in the crate, not a missing toolchain:\n${detail.trim()}`,
    );
  }
  if (!nativeOwnerBinaryPresent()) {
    throw new Error(
      `[global-setup] native:mpf-owner:build succeeded but ${nativeOwnerBinaryPath} is absent — the package script and the consumers disagree about the artifact path`,
    );
  }
};

/**
 * Create and migrate every shard the current fork bound can reach. Shared with
 * midgard-node-tools, whose suite reuses this shard scheme under its own
 * database prefix (tests/test-env.ts).
 */
export const provisionMidgardNodeTestDatabaseShards =
  async (): Promise<void> => {
    applyMidgardNodeTestEnv();
    const pinnedDatabase = process.env.POSTGRES_DB;
    try {
      for (const shard of testDatabaseNames()) {
        await Effect.runPromise(provisionShard(shard));
      }
    } finally {
      process.env.POSTGRES_DB = pinnedDatabase;
    }
  };

export const setup = async (): Promise<void> => {
  buildNativeOwnerBinary();
  // The package's existing opt-out for database-backed tests. Provisioning
  // needs a live Postgres, and most files in this suite do not, so a run that
  // has already declared it is skipping the database files must not be made to
  // need one. Anything that actually touches Postgres still fails loudly.
  if (process.env.MIDGARD_SKIP_DB_TESTS === "1") {
    return;
  }
  await provisionMidgardNodeTestDatabaseShards();
};
