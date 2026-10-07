import { readFileSync } from "node:fs";

import {
  createTemporalRegistry,
  followerMigrations,
} from "@al-ft/midgard-l1-follower";
import {
  declaredTables,
  lintDeterminism,
  lintSchema,
} from "@al-ft/midgard-l1-follower/lint";
import { describe, expect, it } from "vitest";

import {
  COMMITTEE_QUEUE_TABLE,
  COMMITTEE_QUEUE_TABLE_SPEC,
  committeeMigrations,
} from "../../src/l1/follower/queue-table.js";

/** Every committee derivation over the facts (plan §6: pure functions of them). */
const DERIVATIONS = [
  "queue-table.ts",
  "queue-derivation.ts",
  "landed-queue.ts",
  "obligations.ts",
  "projection.ts",
].map((name) => {
  const path = new URL(`../../src/l1/follower/${name}`, import.meta.url);
  return { path: path.pathname, source: readFileSync(path, "utf8") };
});

describe("committee follower lints", () => {
  it("passes the schema lint on both dialects, every table classed", () => {
    const registry = createTemporalRegistry([COMMITTEE_QUEUE_TABLE_SPEC]);
    for (const dialect of ["postgres", "sqlite"] as const) {
      expect(
        lintSchema(
          [followerMigrations(dialect), committeeMigrations(dialect)],
          registry,
        ),
      ).toEqual([]);
      expect(
        declaredTables([committeeMigrations(dialect)]).map((table) => [
          table.table,
          table.tableClass,
        ]),
      ).toEqual([[COMMITTEE_QUEUE_TABLE, "D-t"]]);
    }
  });

  it("passes the determinism lint on every derivation", () => {
    expect(lintDeterminism(DERIVATIONS)).toEqual([]);
  });

  it("would catch a clock read in a derivation", () => {
    const [first] = DERIVATIONS;
    expect(
      lintDeterminism([
        {
          path: first!.path,
          source: `${first!.source}\nexport const stamp = () => Date.now();\n`,
        },
      ]).length,
    ).toBeGreaterThan(0);
  });
});
