import { describe, expect, it } from "vitest";
import { MIGRATIONS } from "@/database/migrations/index.js";
import {
  MigrationError,
  splitSqlStatements,
} from "@/database/migrations/runner.js";

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

  it("keeps the durable-admission guard statement intact", () => {
    const migration = MIGRATIONS.find(({ version }) => version === 2);
    expect(migration).toBeDefined();

    const statements = splitSqlStatements(migration!.sql);

    expect(statements).toHaveLength(7);
    expect(statements[0]).toContain(
      "existing untracked tx state; restore a clean snapshot",
    );
    expect(statements[0]).toContain("END $$");
  });

  it("fails closed on unterminated quoted SQL", () => {
    expect(() => splitSqlStatements("SELECT 'unterminated;")).toThrow(
      MigrationError,
    );
  });
});
