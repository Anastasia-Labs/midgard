import { SqlClient, SqlError } from "@effect/sql";
import { Data, Effect } from "effect";
import { Database } from "@/services/database.js";
import {
  APPLICATION_INDEX_NAMES,
  APPLICATION_TABLE_NAMES,
  EXPECTED_SCHEMA_VERSION,
  MIGRATION_MANIFEST_HASH,
  MIGRATIONS,
  Migration,
  migrationByVersion,
} from "@/database/migrations/index.js";

const MIGRATION_ADVISORY_LOCK_KEY = 0x4d494447415244n; // "MIDGARD"

export class MigrationError extends Data.TaggedError("MigrationError")<{
  readonly code: string;
  readonly message: string;
  readonly cause?: unknown;
}> {}

type AppliedMigrationRow = {
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

const migrationError = (
  code: string,
  message: string,
  cause?: unknown,
): MigrationError => new MigrationError({ code, message, cause });

const sqlError = (code: string, message: string) =>
  Effect.mapError((cause: SqlError.SqlError) =>
    migrationError(code, message, cause),
  );

const ensureMetadataTables: Effect.Effect<void, MigrationError, Database> =
  Effect.gen(function* () {
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
  }).pipe(
    sqlError("schema_metadata_create_failed", "Failed to create metadata"),
  );

const setSessionOptions = (
  mode: "migrate" | "verify",
): Effect.Effect<void, MigrationError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.unsafe("SET client_min_messages = 'error'");
    yield* sql.unsafe("SET default_transaction_isolation TO 'serializable'");
    yield* sql.unsafe(
      mode === "migrate"
        ? "SET lock_timeout = '30s'"
        : "SET lock_timeout = '5s'",
    );
    yield* sql.unsafe(
      mode === "migrate"
        ? "SET statement_timeout = '15min'"
        : "SET statement_timeout = '30s'",
    );
  }).pipe(
    sqlError("schema_session_setup_failed", "Failed to configure DB session"),
  );

const acquireMigrationLock: Effect.Effect<void, MigrationError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      readonly acquired: boolean;
    }>`SELECT pg_try_advisory_lock(${MIGRATION_ADVISORY_LOCK_KEY}) AS acquired`.pipe(
      sqlError("schema_lock_failed", "Failed to acquire schema migration lock"),
    );
    if (rows[0]?.acquired !== true) {
      return yield* Effect.fail(
        migrationError(
          "schema_migration_in_progress",
          "Could not acquire Midgard schema migration advisory lock",
        ),
      );
    }
  });

const releaseMigrationLock: Effect.Effect<void, never, Database> = Effect.gen(
  function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`SELECT pg_advisory_unlock(${MIGRATION_ADVISORY_LOCK_KEY})`;
  },
).pipe(Effect.ignore);

const withMigrationLock = <A, E, R>(
  effect: Effect.Effect<A, E, R>,
): Effect.Effect<A, E | MigrationError, R | Database> =>
  acquireMigrationLock.pipe(
    Effect.andThen(effect),
    Effect.ensuring(releaseMigrationLock),
  );

const readAppliedMigrations: Effect.Effect<
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

const readApplicationTables: Effect.Effect<
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

const readApplicationIndexes: Effect.Effect<
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

const insertMigrationEvent = ({
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

const validateAppliedLedger = (
  applied: readonly AppliedMigrationRow[],
  mode: "exact" | "allowBehind",
): void => {
  const seen = new Set<number>();
  for (let i = 0; i < applied.length; i += 1) {
    const row = applied[i]!;
    const expectedVersion = i + 1;
    if (row.version !== expectedVersion) {
      throw migrationError(
        "schema_non_contiguous_versions",
        `Schema migration versions must be contiguous; expected ${expectedVersion}, found ${row.version}`,
      );
    }
    if (seen.has(row.version)) {
      throw migrationError(
        "schema_duplicate_version",
        `Duplicate schema migration version ${row.version}`,
      );
    }
    seen.add(row.version);
    const migration = migrationByVersion.get(row.version);
    if (migration === undefined) {
      throw migrationError(
        "schema_version_ahead",
        `Database has unknown schema version ${row.version}`,
      );
    }
    if (row.checksum_sha256 !== migration.checksumSha256) {
      throw migrationError(
        "schema_checksum_mismatch",
        `Checksum mismatch for schema version ${row.version}`,
      );
    }
  }
  const actualVersion = applied.at(-1)?.version ?? 0;
  if (mode === "exact" && actualVersion !== EXPECTED_SCHEMA_VERSION) {
    throw migrationError(
      actualVersion < EXPECTED_SCHEMA_VERSION
        ? "schema_version_behind"
        : "schema_version_ahead",
      `Database schema version ${actualVersion} does not match expected version ${EXPECTED_SCHEMA_VERSION}`,
    );
  }
};

const validateAppliedLedgerEffect = (
  applied: readonly AppliedMigrationRow[],
  mode: "exact" | "allowBehind",
): Effect.Effect<void, MigrationError> =>
  Effect.try({
    try: () => validateAppliedLedger(applied, mode),
    catch: (cause) =>
      cause instanceof MigrationError
        ? cause
        : migrationError(
            "schema_ledger_validation_failed",
            "Failed to validate schema migration ledger",
            cause,
          ),
  });

const verifyApplicationShape: Effect.Effect<void, MigrationError, Database> =
  Effect.gen(function* () {
    const [tables, indexes] = yield* Effect.all(
      [readApplicationTables, readApplicationIndexes],
      { concurrency: "unbounded" },
    );
    const tableSet = new Set(tables);
    const indexSet = new Set(indexes);
    const missingTables = APPLICATION_TABLE_NAMES.filter(
      (tableName) => !tableSet.has(tableName),
    );
    const missingIndexes = APPLICATION_INDEX_NAMES.filter(
      (indexName) => !indexSet.has(indexName),
    );
    if (missingTables.length > 0 || missingIndexes.length > 0) {
      return yield* Effect.fail(
        migrationError(
          "schema_drift_detected",
          `Schema drift detected: missingTables=${missingTables.join(",") || "<none>"}, missingIndexes=${missingIndexes.join(",") || "<none>"}`,
        ),
      );
    }
  });

const dollarQuoteAt = (sqlText: string, index: number): string | null => {
  if (sqlText[index] !== "$") {
    return null;
  }
  const remainder = sqlText.slice(index);
  const match = /^\$[A-Za-z_][A-Za-z0-9_]*\$|^\$\$/.exec(remainder);
  return match?.[0] ?? null;
};

const isIdentifierChar = (value: string | undefined): boolean =>
  value !== undefined && /[A-Za-z0-9_]/.test(value);

const singleQuoteUsesBackslashEscapes = (
  sqlText: string,
  quoteIndex: number,
): boolean => {
  const prefixIndex = quoteIndex - 1;
  if (!/[eE]/.test(sqlText[prefixIndex] ?? "")) {
    return false;
  }
  return !isIdentifierChar(sqlText[prefixIndex - 1]);
};

/**
 * Splits PostgreSQL migration text into executable statements without treating
 * semicolons inside strings, identifiers, comments, or dollar-quoted bodies as
 * statement boundaries.
 */
export const splitSqlStatements = (sqlText: string): readonly string[] => {
  const statements: string[] = [];
  let current = "";
  let index = 0;
  let state:
    | "normal"
    | "singleQuote"
    | "doubleQuote"
    | "lineComment"
    | "blockComment"
    | "dollarQuote" = "normal";
  let blockCommentDepth = 0;
  let dollarQuoteTag = "";
  let backslashEscapesSingleQuote = false;

  const pushStatement = () => {
    const statement = current.trim();
    if (statement.length > 0) {
      statements.push(statement);
    }
    current = "";
  };

  while (index < sqlText.length) {
    const char = sqlText[index]!;
    const next = sqlText[index + 1];

    if (state === "normal") {
      if (char === ";") {
        pushStatement();
        index += 1;
        continue;
      }
      if (char === "'") {
        state = "singleQuote";
        backslashEscapesSingleQuote = singleQuoteUsesBackslashEscapes(
          sqlText,
          index,
        );
        current += char;
        index += 1;
        continue;
      }
      if (char === '"') {
        state = "doubleQuote";
        current += char;
        index += 1;
        continue;
      }
      if (char === "-" && next === "-") {
        state = "lineComment";
        current += "--";
        index += 2;
        continue;
      }
      if (char === "/" && next === "*") {
        state = "blockComment";
        blockCommentDepth = 1;
        current += "/*";
        index += 2;
        continue;
      }
      const dollarTag = dollarQuoteAt(sqlText, index);
      if (dollarTag !== null) {
        state = "dollarQuote";
        dollarQuoteTag = dollarTag;
        current += dollarTag;
        index += dollarTag.length;
        continue;
      }
      current += char;
      index += 1;
      continue;
    }

    if (state === "singleQuote") {
      if (char === "'" && next === "'") {
        current += "''";
        index += 2;
        continue;
      }
      if (backslashEscapesSingleQuote && char === "\\" && next !== undefined) {
        current += char + next;
        index += 2;
        continue;
      }
      current += char;
      index += 1;
      if (char === "'") {
        state = "normal";
      }
      continue;
    }

    if (state === "doubleQuote") {
      if (char === '"' && next === '"') {
        current += '""';
        index += 2;
        continue;
      }
      current += char;
      index += 1;
      if (char === '"') {
        state = "normal";
      }
      continue;
    }

    if (state === "lineComment") {
      current += char;
      index += 1;
      if (char === "\n") {
        state = "normal";
      }
      continue;
    }

    if (state === "blockComment") {
      if (char === "/" && next === "*") {
        blockCommentDepth += 1;
        current += "/*";
        index += 2;
        continue;
      }
      if (char === "*" && next === "/") {
        blockCommentDepth -= 1;
        current += "*/";
        index += 2;
        if (blockCommentDepth === 0) {
          state = "normal";
        }
        continue;
      }
      current += char;
      index += 1;
      continue;
    }

    if (state === "dollarQuote") {
      if (sqlText.startsWith(dollarQuoteTag, index)) {
        current += dollarQuoteTag;
        index += dollarQuoteTag.length;
        state = "normal";
        dollarQuoteTag = "";
        continue;
      }
      current += char;
      index += 1;
      continue;
    }
  }

  if (state === "singleQuote") {
    throw migrationError(
      "schema_migration_sql_parse_failed",
      "Unterminated single-quoted string in migration SQL",
    );
  }
  if (state === "doubleQuote") {
    throw migrationError(
      "schema_migration_sql_parse_failed",
      "Unterminated double-quoted identifier in migration SQL",
    );
  }
  if (state === "blockComment") {
    throw migrationError(
      "schema_migration_sql_parse_failed",
      "Unterminated block comment in migration SQL",
    );
  }
  if (state === "dollarQuote") {
    throw migrationError(
      "schema_migration_sql_parse_failed",
      `Unterminated dollar-quoted string ${dollarQuoteTag} in migration SQL`,
    );
  }

  pushStatement();
  return statements;
};

const executeMigrationSql = (
  migration: Migration,
): Effect.Effect<void, MigrationError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const statements = yield* Effect.try({
      try: () => splitSqlStatements(migration.sql),
      catch: (cause) =>
        cause instanceof MigrationError
          ? cause
          : migrationError(
              "schema_migration_sql_parse_failed",
              `Failed to parse migration ${migration.version}_${migration.name}`,
              cause,
            ),
    });
    for (const statement of statements) {
      yield* sql.unsafe(statement);
    }
  }).pipe(
    Effect.mapError((error) =>
      error instanceof SqlError.SqlError
        ? migrationError(
            "schema_migration_sql_failed",
            `Failed to execute migration ${migration.version}_${migration.name}`,
            error,
          )
        : error,
    ),
  );

const applyMigration = ({
  migration,
  appVersion,
  actor,
}: {
  readonly migration: Migration;
  readonly appVersion: string;
  readonly actor: string;
}): Effect.Effect<void, MigrationError, Database> =>
  Effect.gen(function* () {
    const startedAt = Date.now();
    yield* insertMigrationEvent({
      migration,
      eventType: "started",
      appVersion,
      actor,
      details: {
        manifestHash: MIGRATION_MANIFEST_HASH,
      },
    });
    const sql = yield* SqlClient.SqlClient;
    const txProgram = Effect.gen(function* () {
      yield* executeMigrationSql(migration);
      const executionMs = Date.now() - startedAt;
      yield* sql`INSERT INTO schema_migrations (
          version,
          name,
          checksum_sha256,
          manifest_hash_sha256,
          app_version,
          execution_ms,
          applied_by
        ) VALUES (
          ${migration.version},
          ${migration.name},
          ${migration.checksumSha256},
          ${MIGRATION_MANIFEST_HASH},
          ${appVersion},
          ${executionMs},
          ${actor}
        )`;
      yield* insertMigrationEvent({
        migration,
        eventType: "succeeded",
        appVersion,
        actor,
        details: {
          executionMs,
          manifestHash: MIGRATION_MANIFEST_HASH,
        },
      });
    });
    const result = yield* Effect.either(sql.withTransaction(txProgram));
    if (result._tag === "Left") {
      yield* insertMigrationEvent({
        migration,
        eventType: "failed",
        appVersion,
        actor,
        details: {
          failure: String(result.left),
          manifestHash: MIGRATION_MANIFEST_HASH,
        },
      });
      return yield* Effect.fail(
        result.left instanceof MigrationError
          ? result.left
          : migrationError(
              "schema_migration_apply_failed",
              `Failed to apply migration ${migration.version}_${migration.name}`,
              result.left,
            ),
      );
    }
  });

export const migrate = ({
  appVersion = "unknown",
  actor = "midgard-node",
}: {
  readonly appVersion?: string;
  readonly actor?: string;
} = {}): Effect.Effect<MigrationStatus, MigrationError, Database> =>
  setSessionOptions("migrate").pipe(
    Effect.andThen(
      withMigrationLock(
        Effect.gen(function* () {
          yield* ensureMetadataTables;
          const applied = yield* readAppliedMigrations;
          const existingTables = yield* readApplicationTables;
          if (applied.length === 0 && existingTables.length > 0) {
            return yield* Effect.fail(
              migrationError(
                "schema_unversioned_database",
                `Refusing to migrate unversioned database with existing application tables: ${existingTables.join(",")}`,
              ),
            );
          }
          yield* validateAppliedLedgerEffect(applied, "allowBehind");
          const actualVersion = applied.at(-1)?.version ?? 0;
          const pending = MIGRATIONS.filter(
            (migration) => migration.version > actualVersion,
          );
          for (const migration of pending) {
            yield* applyMigration({ migration, appVersion, actor });
          }
          const finalApplied = yield* readAppliedMigrations;
          yield* validateAppliedLedgerEffect(finalApplied, "exact");
          yield* verifyApplicationShape;
          return yield* getStatusUnsafe;
        }),
      ),
    ),
  );

const getStatusUnsafe: Effect.Effect<
  MigrationStatus,
  MigrationError,
  Database
> = Effect.gen(function* () {
  const applied = yield* readAppliedMigrations;
  const tables = yield* readApplicationTables;
  const indexes = yield* readApplicationIndexes;
  const actualVersion = applied.at(-1)?.version ?? null;
  const appliedVersions = new Set(applied.map((row) => row.version));
  const unknownVersions = applied
    .map((row) => row.version)
    .filter((version) => !migrationByVersion.has(version));
  const checksumMismatches = applied
    .filter((row) => {
      const migration = migrationByVersion.get(row.version);
      return (
        migration !== undefined &&
        migration.checksumSha256 !== row.checksum_sha256
      );
    })
    .map((row) => row.version);
  const pending = MIGRATIONS.filter(
    (migration) => !appliedVersions.has(migration.version),
  );
  const tableSet = new Set(tables);
  const indexSet = new Set(indexes);
  const missingApplicationTables = APPLICATION_TABLE_NAMES.filter(
    (tableName) => !tableSet.has(tableName),
  );
  const missingApplicationIndexes = APPLICATION_INDEX_NAMES.filter(
    (indexName) => !indexSet.has(indexName),
  );

  let failureCode: string | null = null;
  try {
    validateAppliedLedger(applied, "exact");
    if (
      missingApplicationTables.length > 0 ||
      missingApplicationIndexes.length > 0
    ) {
      failureCode = "schema_drift_detected";
    }
  } catch (error) {
    failureCode =
      error instanceof MigrationError ? error.code : "schema_status_failed";
  }
  if (applied.length === 0 && tables.length === 0) {
    failureCode = "schema_not_migrated";
  } else if (applied.length === 0 && tables.length > 0) {
    failureCode = "schema_unversioned_database";
  }

  return {
    expectedVersion: EXPECTED_SCHEMA_VERSION,
    actualVersion,
    manifestHash: MIGRATION_MANIFEST_HASH,
    applied,
    pending,
    unknownVersions,
    checksumMismatches,
    applicationTablesPresent: tables,
    missingApplicationTables,
    missingApplicationIndexes,
    compatible: failureCode === null,
    failureCode,
  };
});

export const getStatus: Effect.Effect<
  MigrationStatus,
  MigrationError,
  Database
> = setSessionOptions("verify").pipe(
  Effect.andThen(ensureMetadataTables),
  Effect.andThen(getStatusUnsafe),
);

export const assertCompatible: Effect.Effect<void, MigrationError, Database> =
  setSessionOptions("verify").pipe(
    Effect.andThen(
      withMigrationLock(
        Effect.gen(function* () {
          yield* ensureMetadataTables;
          const applied = yield* readAppliedMigrations;
          const existingTables = yield* readApplicationTables;
          if (applied.length === 0 && existingTables.length === 0) {
            return yield* Effect.fail(
              migrationError(
                "schema_not_migrated",
                "Database has no applied migrations; run `midgard-node db:migrate` before starting the node",
              ),
            );
          }
          if (applied.length === 0 && existingTables.length > 0) {
            return yield* Effect.fail(
              migrationError(
                "schema_unversioned_database",
                `Database contains unversioned application tables: ${existingTables.join(",")}`,
              ),
            );
          }
          yield* validateAppliedLedgerEffect(applied, "exact");
          yield* verifyApplicationShape;
          yield* Effect.logInfo(
            `schema compatibility verified: expected_version=${EXPECTED_SCHEMA_VERSION}, actual_version=${applied.at(-1)?.version}, manifest_hash=${MIGRATION_MANIFEST_HASH}`,
          );
        }),
      ),
    ),
  );

export const verify = assertCompatible;

export const formatStatus = (status: MigrationStatus): string =>
  JSON.stringify(
    {
      expectedVersion: status.expectedVersion,
      actualVersion: status.actualVersion,
      manifestHash: status.manifestHash,
      compatible: status.compatible,
      failureCode: status.failureCode,
      applied: status.applied.map((row) => ({
        version: row.version,
        name: row.name,
        checksumSha256: row.checksum_sha256,
        appliedAt: row.applied_at.toISOString(),
      })),
      pending: status.pending.map((migration) => ({
        version: migration.version,
        name: migration.name,
        checksumSha256: migration.checksumSha256,
      })),
      unknownVersions: status.unknownVersions,
      checksumMismatches: status.checksumMismatches,
      applicationTablesPresent: status.applicationTablesPresent,
      missingApplicationTables: status.missingApplicationTables,
      missingApplicationIndexes: status.missingApplicationIndexes,
    },
    null,
    2,
  );

export const formatChecksum = (): string =>
  JSON.stringify(
    {
      expectedVersion: EXPECTED_SCHEMA_VERSION,
      manifestHash: MIGRATION_MANIFEST_HASH,
      migrations: MIGRATIONS.map((migration) => ({
        version: migration.version,
        name: migration.name,
        checksumSha256: migration.checksumSha256,
      })),
    },
    null,
    2,
  );
