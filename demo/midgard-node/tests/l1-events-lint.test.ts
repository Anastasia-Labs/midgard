import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  createTemporalRegistry,
  followerMigrations,
} from "@al-ft/midgard-l1-follower";
import {
  declaredTables,
  lintDeterminism,
  lintDeterminismModules,
  lintSchema,
} from "@al-ft/midgard-l1-follower/lint";
import { describe, expect, it } from "vitest";

import { EVENT_TABLES, eventMigrations } from "../src/l1-events/index.js";

/**
 * The node event projection's modules (what runs in the writer transaction,
 * and the driver and reads beside it) and every module they import.
 */
const LINTED = lintDeterminismModules({
  root: fileURLToPath(new URL("..", import.meta.url)),
  include: ["src/l1-events/*.ts"],
});

/** The S3 modules the lint once named by hand: it must still reach each. */
const DERIVATION_MODULES = [
  "src/l1-events/config.ts",
  "src/l1-events/schema.ts",
  "src/l1-events/derive.ts",
  "src/l1-events/projection.ts",
];

const read = (path: string) => ({
  path,
  source: readFileSync(new URL(`../${path}`, import.meta.url), "utf8"),
});

describe("node event projection lints", () => {
  it("declares a class and retention for every table, and registers its D-t tables, on both dialects", () => {
    const registry = createTemporalRegistry([...EVENT_TABLES]);
    for (const dialect of ["postgres", "sqlite"] as const)
      expect(
        lintSchema(
          [followerMigrations(dialect), eventMigrations(dialect)],
          registry,
        ),
      ).toEqual([]);
    expect(
      declaredTables([eventMigrations("postgres")]).map((table) => [
        table.table,
        table.tableClass,
      ]),
    ).toEqual([
      ["node_l1_events", "D-t"],
      ["node_l1_event_retirements", "D-t"],
      ["node_l1_event_refusals", "D-t"],
    ]);
  });

  it("keeps the modules and their imports free of clocks, randomness, the network and the host", () => {
    expect(LINTED.problems).toEqual([]);
    expect(LINTED.files).toEqual(
      expect.arrayContaining(DERIVATION_MODULES) as unknown,
    );
    // The lint is live on these files: a clock read in the derivation is caught.
    const derive = read("src/l1-events/derive.ts");
    expect(
      lintDeterminism([
        {
          ...derive,
          source: `${derive.source}\nexport const t = Date.now();\n`,
        },
      ]).map((problem) => problem.rule),
    ).toEqual(["clock"]);
  });
});
