import { Effect } from "effect";

import { Database } from "../../services/database.js";
import {
  APPLICATION_INDEX_NAMES,
  APPLICATION_TABLE_NAMES,
  EXPECTED_SCHEMA_VERSION,
  MIGRATION_MANIFEST_HASH,
  migrationByVersion,
} from "./index.js";
import {
  type AppliedMigrationRow,
  MigrationError,
  migrationError,
  readApplicationIndexes,
  readApplicationTables,
} from "./runner.with-migration-transaction.js";

export const validateAppliedMigrationLedger = (
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
    if (row.name !== migration.name) {
      throw migrationError(
        "schema_name_mismatch",
        `Name mismatch for schema version ${row.version}`,
      );
    }
    if (row.checksum_sha256 !== migration.checksumSha256) {
      throw migrationError(
        "schema_checksum_mismatch",
        `Checksum mismatch for schema version ${row.version}`,
      );
    }
    if (row.manifest_hash_sha256 !== MIGRATION_MANIFEST_HASH) {
      throw migrationError(
        "schema_manifest_hash_mismatch",
        `Manifest hash mismatch for schema version ${row.version}`,
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

export const validateAppliedLedgerEffect = (
  applied: readonly AppliedMigrationRow[],
  mode: "exact" | "allowBehind",
): Effect.Effect<void, MigrationError> =>
  Effect.try({
    try: () => validateAppliedMigrationLedger(applied, mode),
    catch: (cause) =>
      cause instanceof MigrationError
        ? cause
        : migrationError(
            "schema_ledger_validation_failed",
            "Failed to validate schema migration ledger",
            cause,
          ),
  });

export const verifyApplicationShape: Effect.Effect<
  void,
  MigrationError,
  Database
> = Effect.gen(function* () {
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

export const dollarQuoteAt = (
  sqlText: string,
  index: number,
): string | null => {
  if (sqlText[index] !== "$") {
    return null;
  }
  const remainder = sqlText.slice(index);
  const match = /^\$[A-Za-z_][A-Za-z0-9_]*\$|^\$\$/.exec(remainder);
  return match?.[0] ?? null;
};

const isIdentifierChar = (value: string | undefined): boolean =>
  value !== undefined && /[A-Za-z0-9_]/.test(value);

export const singleQuoteUsesBackslashEscapes = (
  sqlText: string,
  quoteIndex: number,
): boolean => {
  const prefixIndex = quoteIndex - 1;
  if (!/[eE]/.test(sqlText[prefixIndex] ?? "")) {
    return false;
  }
  return !isIdentifierChar(sqlText[prefixIndex - 1]);
};
