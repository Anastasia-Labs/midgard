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

import { EVENT_TABLES, eventMigrations } from "../src/l1-events/index.js";

/** The node event projection's S3 modules: what runs in the writer transaction. */
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

  it("keeps the derivation modules free of clocks, randomness and the network", () => {
    const files = DERIVATION_MODULES.map(read);
    expect(lintDeterminism(files)).toEqual([]);
    // The lint is live on these files: a clock read in the derivation is caught.
    const [derive] = files.filter((file) => file.path.endsWith("derive.ts"));
    expect(
      lintDeterminism([
        {
          ...derive!,
          source: `${derive!.source}\nexport const t = Date.now();\n`,
        },
      ]).map((problem) => problem.rule),
    ).toEqual(["clock"]);
  });
});
