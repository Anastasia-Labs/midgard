import { readFileSync } from "node:fs";

import { describe, expect, it } from "vitest";

import {
  createTemporalRegistry,
  followerMigrations,
  MIGRATION_LEDGER_DDL,
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
  it("passes the follower schema on both dialects, the migration ledger and the fixture D-t tables", () => {
    for (const dialect of ["postgres", "sqlite"] as const)
      expect(
        lintSchema(
          [
            followerMigrations(dialect),
            {
              namespace: "ledger",
              migrations: [{ id: "ledger", sql: MIGRATION_LEDGER_DDL }],
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
