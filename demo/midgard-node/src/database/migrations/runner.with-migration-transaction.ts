import { performance } from "node:perf_hooks";

import { SqlClient, SqlError } from "@effect/sql";
import { Data, Effect } from "effect";

import { Database } from "../../services/database.js";
import {
  APPLICATION_INDEX_NAMES,
  APPLICATION_TABLE_NAMES,
  Migration,
} from "./index.js";

const MIGRATION_ADVISORY_LOCK_KEY = 0x4d494447415244n;
// "MIDGARD"

export const migrationExecutionMs = (
  startedAtMs: number,
  nowMs: number = performance.now(),
): number => Math.max(0, Math.round(nowMs - startedAtMs));

export class MigrationError extends Data.TaggedError("MigrationError")<{
  readonly code: string;
  readonly message: string;
  readonly cause?: unknown;
}> {}

export type AppliedMigrationRow = {
  readonly version: number;
  readonly name: string;
  readonly checksum_sha256: string;
  readonly manifest_hash_sha256: string;
  readonly applied_at: Date;
  readonly app_version: string;
  readonly execution_ms: number;
  readonly applied_by: string;
};

export type MigrationStatus = {
  readonly expectedVersion: number;
  readonly actualVersion: number | null;
  readonly manifestHash: string;
  readonly applied: readonly AppliedMigrationRow[];
  readonly pending: readonly Migration[];
  readonly unknownVersions: readonly number[];
  readonly checksumMismatches: readonly number[];
  readonly applicationTablesPresent: readonly string[];
  readonly missingApplicationTables: readonly string[];
  readonly missingApplicationIndexes: readonly string[];
  readonly compatible: boolean;
  readonly failureCode: string | null;
};

export const migrationError = (
  code: string,
  message: string,
  cause?: unknown,
): MigrationError => new MigrationError({ code, message, cause });

const sqlError = (code: string, message: string) =>
  Effect.mapError((cause: SqlError.SqlError) =>
    migrationError(code, message, cause),
  );

export const ensureMetadataTables: Effect.Effect<
  void,
  MigrationError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql.withTransaction(
    Effect.gen(function* () {
      yield* sql`CREATE TABLE IF NOT EXISTS schema_migrations (
        version INTEGER PRIMARY KEY CHECK (version > 0),
        name TEXT NOT NULL,
        checksum_sha256 TEXT NOT NULL CHECK (checksum_sha256 ~ '^[0-9a-f]{64}$'),
        manifest_hash_sha256 TEXT NOT NULL CHECK (manifest_hash_sha256 ~ '^[0-9a-f]{64}$'),
        applied_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        app_version TEXT NOT NULL,
        execution_ms INTEGER NOT NULL CHECK (execution_ms >= 0),
        applied_by TEXT NOT NULL,
        UNIQUE (version, checksum_sha256)
      );`;
      yield* sql`CREATE TABLE IF NOT EXISTS schema_migration_events (
        id BIGSERIAL PRIMARY KEY,
        version INTEGER,
        name TEXT,
        checksum_sha256 TEXT,
        event_type TEXT NOT NULL CHECK (
          event_type IN (
            'started',
            'succeeded',
            'failed',
            'verification_failed'
          )
        ),
        created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
        app_version TEXT NOT NULL,
        actor TEXT NOT NULL,
        details JSONB NOT NULL DEFAULT '{}'::jsonb
      );`;
    }),
  );
}).pipe(sqlError("schema_metadata_create_failed", "Failed to create metadata"));

/**
 * Runs schema work on one transaction-pinned connection. Session-level SETs
 * are unsafe through a pool: each statement may land on a different backend
 * and the setting then leaks to unrelated borrowers. SET LOCAL scopes every
 * option to this transaction. Migration/compatibility operations reserve one
 * physical connection and take the session lock before BEGIN, so a waiter
 * starts its serializable snapshot only after the previous migrator commits.
 * The same reserved connection performs the matching unlock.
 */
export const withMigrationTransaction = <A, E, R>({
  mode,
  lock,
  effect,
}: {
  readonly mode: "migrate" | "verify";
  readonly lock: boolean;
  readonly effect: Effect.Effect<A, E, R>;
}): Effect.Effect<A, E | MigrationError, R | Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const transaction = sql.withTransaction(
      Effect.gen(function* () {
        // This must precede every migration read in the transaction.
        yield* sql.unsafe("SET LOCAL transaction_isolation = 'serializable'");
        yield* sql.unsafe("SET LOCAL client_min_messages = 'error'");
        yield* sql.unsafe(
          mode === "migrate"
            ? "SET LOCAL lock_timeout = '30s'"
            : "SET LOCAL lock_timeout = '5s'",
        );
        yield* sql.unsafe(
          mode === "migrate"
            ? "SET LOCAL statement_timeout = '15min'"
            : "SET LOCAL statement_timeout = '30s'",
        );
        return yield* effect;
      }),
    );
    if (!lock) {
      return yield* transaction;
    }

    return yield* Effect.scoped(
      Effect.gen(function* () {
        const connection = yield* sql.reserve.pipe(
          sqlError(
            "schema_lock_failed",
            "Failed to reserve schema migration connection",
          ),
        );
        const lockRows = yield* connection
          .execute(
            "SELECT pg_try_advisory_lock($1) AS acquired",
            [MIGRATION_ADVISORY_LOCK_KEY],
            undefined,
          )
          .pipe(
            sqlError(
              "schema_lock_failed",
              "Failed to acquire schema migration lock",
            ),
          );
        if (lockRows[0]?.acquired !== true) {
          return yield* Effect.fail(
            migrationError(
              "schema_migration_in_progress",
              "Could not acquire Midgard schema migration advisory lock",
            ),
          );
        }
        return yield* transaction.pipe(
          // A depth of -1 makes withTransaction start the outer transaction
          // on this already-reserved connection instead of a pool borrower.
          Effect.provideService(SqlClient.TransactionConnection, [
            connection,
            -1,
          ]),
          Effect.ensuring(
            connection
              .execute(
                "SELECT pg_advisory_unlock($1)",
                [MIGRATION_ADVISORY_LOCK_KEY],
                undefined,
              )
              .pipe(Effect.orDie),
          ),
        );
      }),
    );
  }).pipe(
    Effect.mapError((error) =>
      error instanceof MigrationError
        ? error
        : migrationError(
            "schema_session_setup_failed",
            "Failed to configure schema transaction",
            error,
          ),
    ),
  );

export const readAppliedMigrations: Effect.Effect<
  readonly AppliedMigrationRow[],
  MigrationError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  return yield* sql<AppliedMigrationRow>`SELECT
      version,
      name,
      checksum_sha256,
      manifest_hash_sha256,
      applied_at,
      app_version,
      execution_ms,
      applied_by
    FROM schema_migrations
    ORDER BY version ASC`;
}).pipe(sqlError("schema_migration_read_failed", "Failed to read migrations"));

export const readApplicationTables: Effect.Effect<
  readonly string[],
  MigrationError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{ readonly table_name: string }>`SELECT table_name
    FROM information_schema.tables
    WHERE table_schema = 'public'
      AND table_type = 'BASE TABLE'
      AND ${sql.in("table_name", [...APPLICATION_TABLE_NAMES])}
    ORDER BY table_name ASC`;
  return rows.map((row) => row.table_name);
}).pipe(
  sqlError("schema_table_introspection_failed", "Failed to inspect tables"),
);

export const readApplicationIndexes: Effect.Effect<
  readonly string[],
  MigrationError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{ readonly indexname: string }>`SELECT indexname
    FROM pg_indexes
    WHERE schemaname = 'public'
      AND ${sql.in("indexname", [...APPLICATION_INDEX_NAMES])}
    ORDER BY indexname ASC`;
  return rows.map((row) => row.indexname);
}).pipe(
  sqlError("schema_index_introspection_failed", "Failed to inspect indexes"),
);

export const insertMigrationEvent = ({
  migration,
  eventType,
  appVersion,
  actor,
  details,
}: {
  readonly migration?: Migration;
  readonly eventType:
    | "started"
    | "succeeded"
    | "failed"
    | "verification_failed";
  readonly appVersion: string;
  readonly actor: string;
  readonly details: Record<string, unknown>;
}): Effect.Effect<void, MigrationError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO schema_migration_events (
        version,
        name,
        checksum_sha256,
        event_type,
        app_version,
        actor,
        details
      ) VALUES (
        ${migration?.version ?? null},
        ${migration?.name ?? null},
        ${migration?.checksumSha256 ?? null},
        ${eventType},
        ${appVersion},
        ${actor},
        ${JSON.stringify(details)}::jsonb
      )`;
  }).pipe(
    sqlError(
      "schema_migration_event_insert_failed",
      "Failed to record migration event",
    ),
  );
