import { describe, expect, it } from "vitest";

import { MIGRATIONS } from "@/database/migrations/index.js";
import {
  MigrationError,
  migrationExecutionMs,
  splitSqlStatements,
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
      /CREATE TABLE public\.tx_admission_payloads \([\s\S]+?tx_canonical_cbor bytea NOT NULL,/i,
    );
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
