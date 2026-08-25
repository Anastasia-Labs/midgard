/**
 * Provision one Postgres database per test worker, once per run.
 *
 * The suite shards its database by worker slot so that database-touching files
 * can run in parallel (tests/test-env.ts explains the invariant). Each shard
 * needs three things before a worker can use it:
 *
 *   1. the database itself;
 *   2. `synchronous_commit = on` as a per-database setting — the local test
 *      server runs with the global default `off` for speed, CI runs `on`, and
 *      `database.test.ts` / `tx-admissions-claim-load.test.ts` assert the
 *      session value is `on` to prove the write-behind's
 *      `SET LOCAL synchronous_commit = off` relaxation stays transaction-local.
 *      Per-database settings are NOT inherited through
 *      `CREATE DATABASE ... TEMPLATE`, so every shard needs its own `ALTER`.
 *      This mirrors `scripts/create-test-db.sh` at the repo root;
 *   3. the schema — most database files migrate themselves in `beforeAll`, but
 *      not all of them do (`da-publication-reconciler-e2e.test.ts` relied on
 *      CI's separate `db:migrate` step against a single shared database), and a
 *      fresh shard has no schema to inherit.
 */
import { spawnSync } from "node:child_process";
import { existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Effect, Layer, Redacted } from "effect";

import { MigrationRunner } from "@/database/index.js";
import { NodeConfig } from "@/services/config.js";
import { Database } from "@/services/database.js";

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
 * Build the native architecture-G owner binary the mpf-differential and
 * mpf-native-owner-service files spawn. The dedicated scripts
 * (`test:mpf:differential`, `test:mpf-native-owner`) build it themselves, but
 * the plain `test` run never did, so a fresh worktree failed those files on a
 * missing artifact (#642). With a warm cargo target dir `--locked` rebuilds
 * are sub-second; a cold build pays once per worktree.
 *
 * Failure here is deliberately non-fatal: without cargo (or with
 * MIDGARD_SKIP_NATIVE_BUILD=1) the two consumer files skip LOUDLY with the
 * build command in the reason — never a silent pass, never a whole-suite
 * abort over an optional toolchain.
 */
const buildNativeOwnerBinary = (): void => {
  if (process.env.MIDGARD_SKIP_NATIVE_BUILD === "1") {
    process.stderr.write(
      "[global-setup] MIDGARD_SKIP_NATIVE_BUILD=1 — native architecture-g-owner build skipped; dependent files will skip loudly\n",
    );
    return;
  }
  const packageRoot = resolve(dirname(fileURLToPath(import.meta.url)), "..");
  const manifestPath = resolve(
    packageRoot,
    "native/mpf-event-flat-wasm/Cargo.toml",
  );
  const binaryPath = resolve(
    packageRoot,
    "native/mpf-event-flat-wasm/target/release/architecture-g-owner",
  );
  const result = spawnSync(
    "cargo",
    [
      "build",
      "--release",
      "--locked",
      "--bin",
      "architecture-g-owner",
      "--manifest-path",
      manifestPath,
    ],
    { stdio: ["ignore", "pipe", "pipe"], encoding: "utf8" },
  );
  if (result.error !== undefined || result.status !== 0) {
    const detail =
      result.error !== undefined
        ? result.error.message
        : (result.stderr ?? "").split("\n").slice(-15).join("\n");
    process.stderr.write(
      `[global-setup] native architecture-g-owner build FAILED (${detail.trim()}); dependent files will skip loudly unless ${binaryPath} already exists\n`,
    );
    return;
  }
  if (!existsSync(binaryPath)) {
    process.stderr.write(
      `[global-setup] cargo build succeeded but ${binaryPath} is absent — dependent files will skip loudly\n`,
    );
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
  applyMidgardNodeTestEnv();
  const pinnedDatabase = process.env.POSTGRES_DB;
  try {
    for (const shard of testDatabaseNames()) {
      await Effect.runPromise(createShard(shard));
      await Effect.runPromise(migrateShard(shard));
    }
  } finally {
    process.env.POSTGRES_DB = pinnedDatabase;
  }
};
