import { readFileSync } from "node:fs";

import { describe, expect, it } from "vitest";

import {
  applyMigrations,
  createTemporalRegistry,
  FOLLOWER_BOOKKEEPING_DDL,
  followerMigrations,
  openSqliteBackend,
} from "../src/index.js";
import {
  declaredTables,
  lintDeterminismSource,
  lintSchema,
} from "../src/lint/index.js";
import { FIXTURE_TABLES, fixtureMigrations } from "./support/fixture.js";
import { matching } from "./support/matchers.js";

const registry = createTemporalRegistry(FIXTURE_TABLES);

describe("schema lint (class and retention)", () => {
  it("passes the follower schema on both dialects, the bookkeeping and the fixture D-t tables", () => {
    for (const dialect of ["postgres", "sqlite"] as const)
      expect(
        lintSchema(
          [
            followerMigrations(dialect),
            {
              namespace: "bookkeeping",
              migrations: [
                { id: "bookkeeping", sql: FOLLOWER_BOOKKEEPING_DDL },
              ],
            },
            fixtureMigrations(dialect),
          ],
          registry,
        ),
      ).toEqual([]);
    const declared = declaredTables([followerMigrations("postgres")]);
    expect(declared.map((table) => table.table).sort()).toEqual([
      "l1_blocks",
      "l1_event_keys",
      "l1_follower_cursor",
      "l1_output_assets",
      "l1_outputs",
      "l1_rollbacks",
      "l1_scripts",
      "l1_tx_mint_policies",
      "l1_txs",
    ]);
    expect(
      declared.find((table) => table.table === "l1_scripts")?.tableClass,
    ).toBe("C");
  });

  it("fails a fixture migration that has no class", () => {
    const problems = lintSchema([
      {
        namespace: "role",
        migrations: [
          {
            id: "0001",
            sql: `
CREATE TABLE role_unclassified (id integer PRIMARY KEY);

-- retention: forever
CREATE TABLE role_retention_only (id integer PRIMARY KEY);
`,
          },
        ],
      },
    ]);
    expect(problems.map((problem) => problem.table)).toEqual([
      "role_unclassified",
      "role_retention_only",
    ]);
    expect(problems[0]?.message).toMatch(/missing `-- class:/u);
  });

  it("fails an unknown class, an empty retention and an unregistered D-t table", () => {
    const problems = lintSchema(
      [
        {
          namespace: "role",
          migrations: [
            {
              id: "0001",
              sql: `-- class: D-c; retention: never stored
CREATE TABLE role_cache (id integer);
-- class: A; retention:
CREATE TABLE role_no_rule (id integer);
-- class: D-t; retention: closed rows once k deep
CREATE TABLE IF NOT EXISTS role_unregistered (from_slot bigint, to_slot bigint);
`,
            },
          ],
        },
      ],
      registry,
    );
    expect(problems.map((problem) => [problem.table, problem.message])).toEqual(
      [
        ["role_cache", matching(/not one of/u)],
        ["role_no_rule", matching(/retention rule is empty/u)],
        ["role_unregistered", matching(/not registered/u)],
        ["fixture_address_live_count", matching(/not declared D-t/u)],
        ["fixture_block_marks", matching(/not declared D-t/u)],
        ["fixture_spend_log", matching(/not declared D-t/u)],
      ],
    );
  });
});

describe("schema lint (row pins)", () => {
  it("flags a row pin naming a table no migration declares", () => {
    const pinned = createTemporalRegistry([
      {
        name: "role_log",
        shape: "append_only",
        slotColumn: "slot",
        retention: { kind: "created_k_deep" },
        pinnedBy: [
          { column: "id", table: "role_hold", tableColumn: "id" },
          { column: "id", table: "role_missing", tableColumn: "id" },
        ],
      },
    ]);
    const problems = lintSchema(
      [
        {
          namespace: "role",
          migrations: [
            {
              id: "0001",
              sql: `-- class: B; retention: the owner deletes a row when its hold ends
CREATE TABLE role_hold (id integer);
-- class: D-t; retention: rows once slot is k deep, unless held
CREATE TABLE role_log (id integer, slot bigint);
`,
            },
          ],
        },
      ],
      pinned,
    );
    expect(problems.map((problem) => [problem.table, problem.message])).toEqual(
      [["role_log", "pinning table role_missing is declared by no migration"]],
    );
  });
});

describe("schema lint (class B foreign keys)", () => {
  const roleSet = (sql: string) => ({
    namespace: "role",
    migrations: [{ id: "0001", sql }],
  });

  it("flags a class B table that references a non-B table, inline or by ALTER TABLE", () => {
    const problems = lintSchema([
      followerMigrations("postgres"),
      roleSet(`
-- class: C; retention: while referenced
CREATE TABLE role_projection (id integer PRIMARY KEY, bytes bytea);

-- class: B; retention: terminal and k deep
CREATE TABLE role_signed_spend (
  id integer PRIMARY KEY,
  tx_hash bytea NOT NULL,
  output_index integer NOT NULL,
  FOREIGN KEY (tx_hash, output_index) REFERENCES l1_outputs ON DELETE CASCADE
);

-- class: B; retention: terminal and k deep
CREATE TABLE role_signed_note (
  id integer PRIMARY KEY,
  projection integer,
  legacy integer REFERENCES role_legacy_table(id)
);
ALTER TABLE role_signed_note ADD FOREIGN KEY (projection) REFERENCES role_projection (id);

-- class: B; retention: terminal and k deep
CREATE TABLE role_signed_child (
  id integer PRIMARY KEY,
  parent integer NOT NULL REFERENCES "role_signed_spend"(id)
);
`),
    ]);
    expect(problems.map((problem) => [problem.table, problem.message])).toEqual(
      [
        ["role_signed_spend", matching(/references l1_outputs \(class A\)/u)],
        [
          "role_signed_note",
          matching(
            /references role_legacy_table \(declared by no migration\)/u,
          ),
        ],
        [
          "role_signed_note",
          matching(/references role_projection \(class C\)/u),
        ],
      ],
    );
  });
});

describe("migration runner (catalog)", () => {
  it("records each table's class, and refuses a table without its header", async () => {
    const backend = openSqliteBackend(":memory:");
    try {
      await applyMigrations(backend, [
        followerMigrations("sqlite"),
        {
          namespace: "role",
          migrations: [
            {
              id: "0001",
              sql: "-- class: B; retention: forever\nCREATE TABLE role_signed (id integer PRIMARY KEY);",
            },
          ],
        },
      ]);
      const catalog = await backend.transaction("read", (tx) =>
        tx.query(
          "SELECT table_name, table_class FROM l1_follower_tables ORDER BY table_name",
        ),
      );
      expect(
        Object.fromEntries(
          catalog.map((row) => [row.table_name, row.table_class]),
        ),
      ).toMatchObject({ l1_outputs: "A", l1_scripts: "C", role_signed: "B" });
      await expect(
        applyMigrations(backend, [
          {
            namespace: "role",
            migrations: [
              {
                id: "0002",
                sql: "CREATE TABLE role_unclassified (id integer);",
              },
            ],
          },
        ]),
      ).rejects.toThrow(
        /role\/0002 table role_unclassified: missing `-- class:/u,
      );
    } finally {
      await backend.close();
    }
  });
});

describe("determinism lint (S3 modules)", () => {
  const source = (name: string): string =>
    readFileSync(new URL(`./fixtures/s3/${name}`, import.meta.url), "utf8");

  it("passes a pure derivation", () => {
    expect(
      lintDeterminismSource(
        "good-derivation.ts",
        source("good-derivation.ts.txt"),
      ),
    ).toEqual([]);
  });

  it("flags clocks, randomness, network and sidecar imports", () => {
    const problems = lintDeterminismSource(
      "bad-derivation.ts",
      source("bad-derivation.ts.txt"),
    );
    expect(
      problems.map((problem) => `${problem.rule}:${problem.line}`),
    ).toEqual([
      "randomness:1",
      "network_import:2",
      "network_import:3",
      "clock:6",
      "clock:7",
      "randomness:8",
      "network_global:9",
      "network_import:10",
      "clock:11",
    ]);
  });

  it("refuses extra modules a role names", () => {
    expect(
      lintDeterminismSource(
        "x.ts",
        'import { wallet } from "@al-ft/role-wallet/live";',
        {
          bannedModules: ["@al-ft/role-wallet"],
        },
      ).map((problem) => problem.rule),
    ).toEqual(["network_import"]);
  });
});
