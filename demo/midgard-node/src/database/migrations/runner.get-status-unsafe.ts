import { Effect } from "effect";

import { Database } from "../../services/database.js";
import { installFollowerSchema } from "../follower-schema.js";
import {
  APPLICATION_INDEX_NAMES,
  APPLICATION_TABLE_NAMES,
  EXPECTED_SCHEMA_VERSION,
  MIGRATION_MANIFEST_HASH,
  migrationByVersion,
  MIGRATIONS,
} from "./index.js";
import { applyMigration } from "./runner.split-sql-statements.js";
import {
  validateAppliedLedgerEffect,
  validateAppliedMigrationLedger,
  verifyApplicationShape,
} from "./runner.validate-applied-migration-ledger.js";
import {
  ensureMetadataTables,
  MigrationError,
  migrationError,
  type MigrationStatus,
  readApplicationIndexes,
  readApplicationTables,
  readAppliedMigrations,
  withMigrationTransaction,
} from "./runner.with-migration-transaction.js";

export const migrate = ({
  appVersion = "unknown",
  actor = "midgard-node",
}: {
  readonly appVersion?: string;
  readonly actor?: string;
} = {}): Effect.Effect<MigrationStatus, MigrationError, Database> =>
  withMigrationTransaction({
    mode: "migrate",
    lock: true,
    effect: Effect.gen(function* () {
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
        const result = yield* Effect.either(
          applyMigration({ migration, appVersion, actor }),
        );
        if (result._tag === "Left") {
          // Commit the per-migration `started`/`failed` audit events and all
          // earlier successful migrations before surfacing this failure. The
          // failing migration itself was rolled back to its nested savepoint.
          return { _tag: "Failure", error: result.left } as const;
        }
      }
      // The node's SQL joins the follower's admission identity, so its
      // tables are part of every migrated node database.
      yield* installFollowerSchema.pipe(
        Effect.mapError((cause) =>
          migrationError(
            "follower_schema_failed",
            "Failed to install the L1 follower schema",
            cause,
          ),
        ),
      );
      const finalApplied = yield* readAppliedMigrations;
      yield* validateAppliedLedgerEffect(finalApplied, "exact");
      yield* verifyApplicationShape;
      return {
        _tag: "Success",
        status: yield* getStatusUnsafe,
      } as const;
    }),
  }).pipe(
    Effect.flatMap((result) =>
      result._tag === "Failure"
        ? Effect.fail(result.error)
        : Effect.succeed(result.status),
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
    validateAppliedMigrationLedger(applied, "exact");
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
> = withMigrationTransaction({
  mode: "verify",
  lock: false,
  effect: ensureMetadataTables.pipe(Effect.andThen(getStatusUnsafe)),
});

export const assertCompatible: Effect.Effect<void, MigrationError, Database> =
  withMigrationTransaction({
    mode: "verify",
    lock: true,
    effect: Effect.gen(function* () {
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
  });

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
