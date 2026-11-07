import { performance } from "node:perf_hooks";

import { SqlClient, SqlError } from "@effect/sql";
import { Effect } from "effect";

import { Database } from "../../services/database.js";
import { Migration, MIGRATION_MANIFEST_HASH } from "./index.js";
import {
  dollarQuoteAt,
  singleQuoteUsesBackslashEscapes,
} from "./runner.validate-applied-migration-ledger.js";
import {
  insertMigrationEvent,
  MigrationError,
  migrationError,
  migrationExecutionMs,
} from "./runner.with-migration-transaction.js";

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

/**
 * The SQLSTATE a migration raises to refuse a database it cannot upgrade in
 * place; the operator must start from a fresh, empty database instead.
 */
export const FRESH_DATABASE_REQUIRED_SQLSTATE = "MGF29";

/** The PostgreSQL error inside `error` when it carries `sqlState`. */
const postgresErrorWithState = (
  error: SqlError.SqlError,
  sqlState: string,
): { readonly message?: unknown; readonly hint?: unknown } | undefined => {
  const cause: unknown = error.cause;
  return typeof cause === "object" &&
    cause !== null &&
    (cause as { readonly code?: unknown }).code === sqlState
    ? (cause as { readonly message?: unknown; readonly hint?: unknown })
    : undefined;
};

const sqlFailure = (migration: Migration, error: SqlError.SqlError) => {
  const refusal = postgresErrorWithState(
    error,
    FRESH_DATABASE_REQUIRED_SQLSTATE,
  );
  if (refusal === undefined)
    return migrationError(
      "schema_migration_sql_failed",
      `Failed to execute migration ${migration.version}_${migration.name}`,
      error,
    );
  const detail = [refusal.message, refusal.hint]
    .filter((part): part is string => typeof part === "string")
    .join(". ");
  return migrationError(
    "schema_migration_requires_fresh_database",
    `Migration ${migration.version}_${migration.name} refused this database: ${detail}`,
    error,
  );
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
      error instanceof SqlError.SqlError ? sqlFailure(migration, error) : error,
    ),
  );

export const applyMigration = ({
  migration,
  appVersion,
  actor,
}: {
  readonly migration: Migration;
  readonly appVersion: string;
  readonly actor: string;
}): Effect.Effect<void, MigrationError, Database> =>
  Effect.gen(function* () {
    const startedAt = performance.now();
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
      const executionMs = migrationExecutionMs(startedAt);
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
