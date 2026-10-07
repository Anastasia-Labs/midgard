import "./utils.js";

import { createHash } from "node:crypto";

import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  MIGRATION_MANIFEST_HASH,
  MIGRATIONS,
} from "../src/database/migrations/index.js";
import {
  type AppliedMigrationRow,
  MigrationError,
  migrationExecutionMs,
  splitSqlStatements,
  validateAppliedMigrationLedger,
} from "../src/database/migrations/runner.js";
import * as MigrationRunner from "../src/database/migrations/runner.js";
import { BatchSql } from "../src/services/database.js";
import { provideDatabaseLayers } from "./utils.js";

describe("migrationExecutionMs", () => {
  it("never returns a negative duration", () => {
    expect(migrationExecutionMs(200, 22)).toEqual(0);
  });

  it("rounds monotonic elapsed time to integer milliseconds", () => {
    expect(migrationExecutionMs(100, 112.6)).toEqual(13);
  });
});

describe("splitSqlStatements", () => {
  it("does not split semicolons inside quoted text or comments", () => {
    const statements = splitSqlStatements(`
      SELECT 'state; restore', 'escaped '' ; still string';
      SELECT "semi;colon";
      -- comment with a semicolon;
      SELECT 1;
      /* block comment with a semicolon; */
      SELECT 2;
    `);

    expect(statements).toEqual([
      "SELECT 'state; restore', 'escaped '' ; still string'",
      'SELECT "semi;colon"',
      "-- comment with a semicolon;\n      SELECT 1",
      "/* block comment with a semicolon; */\n      SELECT 2",
    ]);
  });

  it("keeps dollar-quoted function bodies intact", () => {
    const statements = splitSqlStatements(`
      CREATE FUNCTION demo_notice() RETURNS void AS $body$
      BEGIN
        RAISE NOTICE 'inside; body';
      END;
      $body$ LANGUAGE plpgsql;
      SELECT 1;
    `);

    expect(statements).toHaveLength(2);
    expect(statements[0]).toContain("RAISE NOTICE 'inside; body';");
    expect(statements[0]).toContain("END;");
    expect(statements[1]).toBe("SELECT 1");
  });

  it("keeps v1 as the single fresh-install baseline with ordered transactional successors", () => {
    expect(
      MIGRATIONS.map(({ version, name, transactional }) => ({
        version,
        name,
        transactional,
      })),
    ).toEqual([
      { version: 1, name: "initial_schema", transactional: true },
      { version: 2, name: "automatic_settlement", transactional: true },
      { version: 3, name: "operator_membership", transactional: true },
      { version: 4, name: "retained_script_material", transactional: true },
      { version: 5, name: "foreign_event_census", transactional: true },
      { version: 6, name: "foreign_native_adoption", transactional: true },
      {
        version: 7,
        name: "drop_foreign_tip_reconciliations",
        transactional: true,
      },
    ]);
  });

  it("splits the real baseline into statements that re-split identically", () => {
    // Idempotence on the 1600-line production dump is a property, not a pin:
    // a splitter that swallowed a dollar-quoted body, merged two statements,
    // or emitted a fragment would re-split into a different list. The dump is
    // the only input in the tree that exercises every lexer state at once.
    const statements = splitSqlStatements(MIGRATIONS[0]!.sql);
    expect(statements.length).toBeGreaterThan(100);
    expect(statements.every((statement) => statement.trim().length > 0)).toBe(
      true,
    );
    expect(splitSqlStatements(statements.join(";\n") + ";")).toEqual(
      statements,
    );
  });

  it("accepts the exact ledger of every migration", () => {
    expect(() =>
      validateAppliedMigrationLedger(
        MIGRATIONS.map((migration) => appliedMigrationRow(migration)),
        "exact",
      ),
    ).not.toThrow();
  });

  it("lets a baseline-only ledger stamped by the baseline-era manifest migrate forward but not serve", () => {
    const baselineOnly = [
      {
        ...appliedMigrationRow(),
        manifest_hash_sha256: manifestHashThrough(1),
      },
    ];
    expect(() =>
      validateAppliedMigrationLedger(baselineOnly, "allowBehind"),
    ).not.toThrow();
    expect(() => validateAppliedMigrationLedger(baselineOnly, "exact")).toThrow(
      expect.objectContaining({ code: "schema_version_behind" }),
    );
  });

  it("accepts each row stamped by the manifest it was applied under, and no older one", () => {
    const [first, second] = MIGRATIONS;
    const stamped = (
      migration: (typeof MIGRATIONS)[number],
      through: number,
    ): AppliedMigrationRow => ({
      ...appliedMigrationRow(migration),
      manifest_hash_sha256: manifestHashThrough(through),
    });
    expect(() =>
      validateAppliedMigrationLedger(
        MIGRATIONS.map((migration) => stamped(migration, migration.version)),
        "exact",
      ),
    ).not.toThrow();
    expect(() =>
      validateAppliedMigrationLedger(
        [stamped(first!, 1), stamped(second!, 1)],
        "allowBehind",
      ),
    ).toThrow(
      expect.objectContaining({ code: "schema_manifest_hash_mismatch" }),
    );
  });

  it("rejects adjacent, renamed, checksum-drifted, and manifest-drifted ledgers", () => {
    const exact = appliedMigrationRow();
    const cases: readonly [
      AppliedMigrationRow,
      (
        | "schema_version_behind"
        | "schema_name_mismatch"
        | "schema_checksum_mismatch"
        | "schema_manifest_hash_mismatch"
      ),
    ][] = [
      [
        {
          ...exact,
          name: "historical_initial_schema",
        },
        "schema_name_mismatch",
      ],
      [
        {
          ...exact,
          checksum_sha256: "00".repeat(32),
        },
        "schema_checksum_mismatch",
      ],
      [
        {
          ...exact,
          manifest_hash_sha256: "11".repeat(32),
        },
        "schema_manifest_hash_mismatch",
      ],
    ];
    expect(() => validateAppliedMigrationLedger([], "exact")).toThrow(
      expect.objectContaining({ code: "schema_version_behind" }),
    );
    for (const [rows, code] of cases.map(
      ([row, code]) =>
        [
          [
            row,
            ...MIGRATIONS.slice(1).map((migration) => ({
              ...appliedMigrationRow(),
              version: migration.version,
              name: migration.name,
              checksum_sha256: migration.checksumSha256,
            })),
          ] as const,
          code,
        ] as const,
    )) {
      expect(() => validateAppliedMigrationLedger(rows, "exact")).toThrow(
        expect.objectContaining({ code }),
      );
    }
  });

  it("fails closed on unterminated quoted SQL", () => {
    expect(() => splitSqlStatements("SELECT 'unterminated;")).toThrow(
      MigrationError,
    );
  });
});

/** Recomputed here, not imported, so the test pins the stamping format. */
const manifestHashThrough = (version: number): string =>
  createHash("sha256")
    .update(
      MIGRATIONS.filter((migration) => migration.version <= version)
        .map((m) => `${m.version}:${m.name}:${m.checksumSha256}`)
        .join("\n"),
    )
    .digest("hex");

const appliedMigrationRow = (
  migration: (typeof MIGRATIONS)[number] = MIGRATIONS[0]!,
): AppliedMigrationRow => ({
  version: migration.version,
  name: migration.name,
  checksum_sha256: migration.checksumSha256,
  manifest_hash_sha256: MIGRATION_MANIFEST_HASH,
  applied_at: new Date("2026-07-27T00:00:00.000Z"),
  app_version: "test",
  execution_ms: 1,
  applied_by: "test",
});

/**
 * The applied schema, not the dump text, is the oracle here.
 *
 * These assertions read `pg_catalog` after the baseline has actually been
 * executed, so Postgres has parsed and re-rendered every CHECK expression.
 * Reformatting `0001_initial_schema.sql` — whitespace, column order, a pg_dump
 * re-emit — cannot move them; deleting a constraint or switching a durable
 * table to UNLOGGED does.
 */
describe("applied fresh-install schema", () => {
  /** Reviewed contract: only the rebuildable delta cache may lose its WAL. */
  const UNLOGGED_TABLES = ["mempool_tx_deltas"] as const;

  /**
   * Reviewed contract: each replay/deployment discriminator the node relies on
   * when reloading a journal must be enforced by the database, not only by the
   * writer. The fragments are matched against Postgres's own normalized
   * rendering of the constraint.
   */
  const REQUIRED_CHECKS: readonly {
    readonly table: string;
    readonly constraint: string;
    readonly fragments: readonly string[];
  }[] = [
    {
      table: "event_history_replay_receipts",
      constraint: "event_history_replay_receipts_frontier_check",
      fragments: [
        "blocks_replayed = 1",
        "predecessor_hash IS NULL",
        "blocks_replayed > 1",
        "predecessor_hash IS NOT NULL",
        "predecessor_hash = parent_hash",
      ],
    },
    {
      table: "pending_block_finalizations",
      constraint: "pending_block_finalizations_format_version_check",
      fragments: ["format_version = 1"],
    },
    {
      table: "pending_block_finalizations",
      constraint: "pending_block_finalizations_replay_kind_check",
      fragments: ["ledger_delta_v1", "ledger_delta_native_mpf_v1"],
    },
    {
      table: "pending_block_finalizations",
      constraint: "pending_block_finalizations_mpf_replay_all_or_none_check",
      fragments: [
        "mpf_owner_schema = 1",
        "octet_length(mpf_owner_binary_sha256) = 32",
        "octet_length(mpf_replay_event_log) >= 92",
        "octet_length(mpf_replay_event_roots) = (mpf_replay_event_count * 32)",
        "encode(mpf_replay_base_root, 'hex'::text) = base_utxos_root",
        "encode(mpf_replay_candidate_root, 'hex'::text) = expected_utxos_root",
      ],
    },
    {
      table: "pending_block_finalizations",
      constraint: "pending_block_finalizations_deployment_marker_schema_check",
      fragments: ["'midgard-deployment-marker-v1'"],
    },
    {
      table: "pending_block_finalizations",
      constraint: "pending_block_finalizations_deployment_manifest_id_check",
      fragments: ["'^[0-9a-f]{64}$'"],
    },
  ];

  it("enforces the replay, deployment and durability contracts in the database", async () => {
    await Effect.runPromise(
      provideDatabaseLayers(
        Effect.gen(function* () {
          const sql = yield* BatchSql;
          yield* sql.unsafe("DROP SCHEMA public CASCADE; CREATE SCHEMA public");
          const status = yield* MigrationRunner.migrate({
            appVersion: "migration-runner-test",
            actor: "schema-contract",
          }).pipe(Effect.provideService(SqlClient.SqlClient, sql));
          expect(status.manifestHash).toBe(MIGRATION_MANIFEST_HASH);

          const unlogged = yield* sql<{
            readonly relname: string;
          }>`SELECT relname
               FROM pg_class
               WHERE relnamespace = 'public'::regnamespace
                 AND relkind = 'r'
                 AND relpersistence = 'u'
               ORDER BY relname`;
          expect(unlogged.map((row) => row.relname)).toEqual([
            ...UNLOGGED_TABLES,
          ]);

          // Version 7 drops the speculative foreign-tip table (#752).
          const dropped = yield* sql<{
            readonly relname: string;
          }>`SELECT relname
               FROM pg_class
               WHERE relnamespace = 'public'::regnamespace
                 AND relname = 'foreign_tip_reconciliations'`;
          expect(dropped).toEqual([]);

          const constraints = yield* sql<{
            readonly relname: string;
            readonly conname: string;
            readonly definition: string;
          }>`SELECT c.relname,
                    con.conname,
                    pg_get_constraintdef(con.oid) AS definition
               FROM pg_constraint AS con
               JOIN pg_class AS c ON c.oid = con.conrelid
               WHERE c.relnamespace = 'public'::regnamespace
                 AND con.contype = 'c'`;
          const definitionByKey = new Map(
            constraints.map((row) => [
              `${row.relname}.${row.conname}`,
              row.definition,
            ]),
          );

          const missing = REQUIRED_CHECKS.filter(
            (required) =>
              !definitionByKey.has(`${required.table}.${required.constraint}`),
          ).map((required) => `${required.table}.${required.constraint}`);
          expect(missing).toEqual([]);

          const unsatisfied = REQUIRED_CHECKS.flatMap((required) => {
            const definition =
              definitionByKey.get(`${required.table}.${required.constraint}`) ??
              "";
            return required.fragments
              .filter((fragment) => !definition.includes(fragment))
              .map(
                (fragment) =>
                  `${required.table}.${required.constraint}: ${fragment}`,
              );
          });
          expect(unsatisfied).toEqual([]);
        }),
      ),
    );
  });
});

describe("upgrading a database migrated by an earlier release", () => {
  // Version 1 is the baseline-only release; version 3 is the last migration
  // a release shipped before retained script material, the foreign event
  // census and foreign native adoption (4-6) were added.
  it.each([1, 3])(
    "migrates a database a release installed through v%i and serves it",
    async (through) => {
      const applied = MIGRATIONS.filter(
        (migration) => migration.version <= through,
      );
      expect(applied.at(-1)?.version).toBe(through);
      const releaseManifest = manifestHashThrough(through);
      await Effect.runPromise(
        provideDatabaseLayers(
          Effect.gen(function* () {
            const sql = yield* BatchSql;
            yield* sql.unsafe(
              "DROP SCHEMA public CASCADE; CREATE SCHEMA public",
            );
            const withSql = <A, E>(
              effect: Effect.Effect<A, E, SqlClient.SqlClient>,
            ) => effect.pipe(Effect.provideService(SqlClient.SqlClient, sql));
            // The state that release left: its ledger tables, its schema, and
            // one row per migration stamped with its own manifest.
            yield* withSql(MigrationRunner.getStatus);
            yield* sql.withTransaction(
              Effect.gen(function* () {
                for (const migration of applied) {
                  for (const statement of splitSqlStatements(migration.sql)) {
                    yield* sql.unsafe(statement);
                  }
                  yield* sql`INSERT INTO schema_migrations
                    (version, name, checksum_sha256, manifest_hash_sha256,
                     app_version, execution_ms, applied_by)
                    VALUES (${migration.version}, ${migration.name},
                            ${migration.checksumSha256}, ${releaseManifest},
                            'earlier-release', 1, 'earlier-release')`;
                }
              }),
            );

            const status = yield* withSql(
              MigrationRunner.migrate({
                appVersion: "migration-runner-test",
                actor: "upgrade",
              }),
            );
            expect(status.actualVersion).toBe(MIGRATIONS.at(-1)!.version);
            yield* withSql(MigrationRunner.assertCompatible);
            const rows = yield* sql<{
              readonly version: number;
              readonly manifest_hash_sha256: string;
            }>`SELECT version, manifest_hash_sha256
                 FROM schema_migrations ORDER BY version`;
            expect(rows).toEqual(
              MIGRATIONS.map((migration) => ({
                version: migration.version,
                manifest_hash_sha256:
                  migration.version <= through
                    ? releaseManifest
                    : MIGRATION_MANIFEST_HASH,
              })),
            );
          }),
        ),
      );
    },
  );
});
