import "./utils.js";

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

  it("exposes one fresh-install baseline with no historical migrations", () => {
    expect(MIGRATIONS).toHaveLength(1);
    expect(MIGRATIONS[0]).toMatchObject({
      version: 1,
      name: "initial_schema",
      transactional: true,
    });
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

  it("accepts the exact fresh baseline ledger row", () => {
    expect(() =>
      validateAppliedMigrationLedger([appliedMigrationRow()], "exact"),
    ).not.toThrow();
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
      ([row, code]) => [[row] as const, code] as const,
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

const appliedMigrationRow = (): AppliedMigrationRow => ({
  version: 1,
  name: "initial_schema",
  checksum_sha256: MIGRATIONS[0]!.checksumSha256,
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
    {
      table: "foreign_tip_reconciliations",
      constraint: "foreign_tip_reconciliations_format_version_check",
      fragments: ["format_version = 1"],
    },
    {
      table: "foreign_tip_reconciliations",
      constraint: "foreign_tip_reconciliations_evidence_kind_check",
      fragments: ["pending_v1", "verified_empty_v1", "verified_da_v1"],
    },
    {
      table: "foreign_tip_reconciliations",
      constraint: "foreign_tip_reconciliations_resolved_evidence_check",
      fragments: ["status <> 'resolved'", "evidence_kind <> 'pending_v1'"],
    },
    {
      table: "foreign_tip_reconciliations",
      constraint: "foreign_tip_reconciliations_verified_da_nonempty_check",
      fragments: ["evidence_kind <> 'verified_da_v1'"],
    },
    {
      table: "foreign_tip_reconciliations",
      constraint: "foreign_tip_reconciliations_deployment_manifest_id_check",
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
