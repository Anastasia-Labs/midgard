import { createHash } from "node:crypto";

import { describe, expect, it } from "vitest";

import {
  MIGRATION_MANIFEST_HASH,
  MIGRATIONS,
} from "@/database/migrations/index.js";
import {
  type AppliedMigrationRow,
  MigrationError,
  migrationExecutionMs,
  splitSqlStatements,
  validateAppliedMigrationLedger,
} from "@/database/migrations/runner.js";

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
    expect(splitSqlStatements(MIGRATIONS[0]!.sql).length).toBeGreaterThan(100);
  });

  it("binds the exact fresh baseline bytes, name, checksum, and manifest hash", () => {
    const migration = MIGRATIONS[0]!;
    expect(
      createHash("sha256").update(migration.sql, "utf8").digest("hex"),
    ).toBe(migration.checksumSha256);
    expect(
      createHash("sha256")
        .update(
          `${migration.version.toString()}:${migration.name}:${migration.checksumSha256}`,
          "utf8",
        )
        .digest("hex"),
    ).toBe(MIGRATION_MANIFEST_HASH);
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

  it("keeps Architecture G replay fields all-or-none and length bound", () => {
    const sql = MIGRATIONS[0]!.sql;
    expect(sql).toContain(
      "pending_block_finalizations_mpf_replay_all_or_none_check",
    );
    expect(sql).toContain("mpf_owner_schema = 1");
    expect(sql).toContain("octet_length(mpf_owner_binary_sha256) = 32");
    expect(sql).toContain("octet_length(mpf_replay_event_log) >= 92");
    expect(sql).toMatch(
      /octet_length\(mpf_replay_event_roots\) = \(?mpf_replay_event_count \* 32\)?/,
    );
  });

  it("binds every pending MPF journal to the exact final deployment marker", () => {
    const sql = MIGRATIONS[0]!.sql;
    expect(sql).toContain("deployment_marker_schema_version text NOT NULL");
    expect(sql).toContain("deployment_manifest_id text NOT NULL");
    expect(sql).toContain(
      "deployment_marker_schema_version = 'midgard-deployment-marker-v1'",
    );
    expect(sql).toContain("deployment_manifest_id ~ '^[0-9a-f]{64}$'");
  });

  it("persists only exact PendingBlockFinalizationV1 replay discriminators", () => {
    const sql = MIGRATIONS[0]!.sql;
    const table = sql.match(
      /CREATE TABLE public\.pending_block_finalizations \(([\s\S]+?)\n\);/u,
    )?.[1];
    expect(table).toBeDefined();
    expect(table).toContain("format_version smallint NOT NULL");
    expect(table).toContain("replay_kind text NOT NULL");
    expect(table).toContain("format_version = 1");
    expect(table).toContain(
      "ARRAY['ledger_delta_v1'::text, 'ledger_delta_native_mpf_v1'::text]",
    );
    expect(table).toContain(
      "(replay_kind = 'ledger_delta_v1'::text) AND (mpf_owner_schema IS NULL)",
    );
    expect(table).toContain(
      "(replay_kind = 'ledger_delta_native_mpf_v1'::text) AND (mpf_owner_schema = 1)",
    );
    expect(table).toContain(
      "encode(mpf_replay_base_root, 'hex'::text) = base_utxos_root",
    );
    expect(table).toContain(
      "encode(mpf_replay_candidate_root, 'hex'::text) = expected_utxos_root",
    );
    expect(table).not.toMatch(/consensus_profile_id text DEFAULT/iu);
    expect(table).not.toMatch(
      /expected_validation_(?:traces_root|trace_count)[^\n]*DEFAULT/iu,
    );
  });

  it("persists exact ForeignTipReconciliationV1 deployment and DA identity", () => {
    const sql = MIGRATIONS[0]!.sql;
    const table = sql.match(
      /CREATE TABLE public\.foreign_tip_reconciliations \(([\s\S]+?)\n\);/u,
    )?.[1];
    expect(table).toBeDefined();
    expect(table).toContain("format_version smallint NOT NULL");
    expect(table).toContain("deployment_marker_schema_version text NOT NULL");
    expect(table).toContain("deployment_manifest_id text NOT NULL");
    expect(table).toContain("consensus_profile_id text NOT NULL");
    expect(table).toContain("verified_da_payload_sha256 bytea");
    expect(table).toContain("evidence_kind text NOT NULL");
    expect(table).toContain("format_version = 1");
    expect(table).toContain(
      "ARRAY['pending_v1'::text, 'verified_empty_v1'::text, 'verified_da_v1'::text]",
    );
    expect(table).toContain(
      "(status <> 'resolved'::text) OR (evidence_kind <> 'pending_v1'::text)",
    );
    expect(table).toContain("octet_length(verified_da_payload_sha256) = 32");
    expect(table).toContain("octet_length(verified_da_payload_cbor) > 0");
    expect(table).toContain(
      "foreign_tip_reconciliations_verified_da_nonempty_check",
    );
    expect(table).not.toMatch(/status text DEFAULT/iu);
    expect(table).not.toMatch(/consensus_profile_id text DEFAULT/iu);
  });

  it("keeps active lease point lookups separate from expiry recovery", () => {
    const sql = MIGRATIONS[0]!.sql;
    expect(sql).toContain("idx_tx_admissions_active_lease");
    expect(sql).toContain("(lease_owner, tx_id)");
    expect(sql).toMatch(/WHERE \(status = 'validating'::/i);
    expect(sql).toContain("idx_tx_admissions_lease");
  });

  it("makes only the rebuildable transaction-delta cache unlogged", () => {
    const sql = MIGRATIONS[0]!.sql;
    expect(sql).toMatch(/CREATE UNLOGGED TABLE public\.mempool_tx_deltas/i);
    expect(sql).not.toMatch(/CREATE UNLOGGED TABLE public\.tx_admissions/i);
    expect(sql).not.toMatch(
      /CREATE UNLOGGED TABLE public\.tx_admission_payloads/i,
    );
  });

  it("defines final inline payload constraints without transitional DML", () => {
    const sql = MIGRATIONS[0]!.sql;
    expect(sql).toMatch(
      /CREATE TABLE public\.mempool \([\s\S]+?tx bytea NOT NULL,/i,
    );
    expect(sql).toMatch(
      /CREATE TABLE public\.tx_admission_payloads \([\s\S]+?tx_canonical_cbor bytea NOT NULL,[\s\S]+?tx_full_hash_v1 bytea NOT NULL,/i,
    );
    expect(sql).not.toContain("tx_canonical_cbor_sha256");
    expect(sql).toMatch(
      /CREATE TABLE public\.tx_admission_payloads \([\s\S]+?cek_program_material_sidecar_cbor bytea NOT NULL,[\s\S]+?cek_program_material_sidecar_sha256 bytea NOT NULL,/i,
    );
    expect(sql).not.toContain("idx_tx_admission_payloads_tx_id_hash");
    expect(sql).not.toMatch(/UPDATE mempool AS membership/i);
    expect(sql).not.toMatch(/DROP INDEX/i);
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
