/**
 * The queue-terminal projection's lints (plan §5.5 P8, §7.4, N4): its table
 * declares a class and a retention and is registered as D-t on both
 * dialects, and the modules its S3 derivation runs read no clock,
 * randomness or network.
 */
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

import {
  QUEUE_TERMINAL_TABLES,
  queueTerminalMigrations,
} from "../src/l1-queue-terminals/index.js";

/**
 * What runs in the writer transaction (`queueTerminalDerivation`) and every
 * module it imports: each queue-terminal module except the node-side reads
 * and the barrel, which run outside the writer transaction against the
 * node's database.
 */
const LINTED = lintDeterminismModules({
  root: fileURLToPath(new URL("..", import.meta.url)),
  include: ["src/l1-queue-terminals/*.ts"],
  exclude: [
    "src/l1-queue-terminals/index.ts",
    "src/l1-queue-terminals/reads.ts",
  ],
});

const DERIVATION_MODULES = [
  "src/l1-queue-terminals/derive.ts",
  "src/l1-queue-terminals/projection.ts",
  "src/l1-queue-terminals/schema.ts",
];

describe("queue-terminal projection lints", () => {
  it("declares a class and retention for its table, and registers it D-t, on both dialects", () => {
    const registry = createTemporalRegistry([...QUEUE_TERMINAL_TABLES]);
    for (const dialect of ["postgres", "sqlite"] as const)
      expect(
        lintSchema(
          [followerMigrations(dialect), queueTerminalMigrations(dialect)],
          registry,
        ),
      ).toEqual([]);
    expect(
      declaredTables([queueTerminalMigrations("postgres")]).map((table) => [
        table.table,
        table.tableClass,
      ]),
    ).toEqual([["node_l1_queue_terminals", "D-t"]]);
    // The lint is live: the same table unregistered is reported.
    expect(
      lintSchema(
        [followerMigrations("postgres"), queueTerminalMigrations("postgres")],
        createTemporalRegistry([]),
      ),
    ).not.toEqual([]);
  });

  it("keeps the derivation and its imports free of clocks, randomness, the network and the host", () => {
    expect(LINTED.problems).toEqual([]);
    expect(LINTED.files).toEqual(
      expect.arrayContaining(DERIVATION_MODULES) as unknown,
    );
    const path = "src/l1-queue-terminals/derive.ts";
    const source = readFileSync(new URL(`../${path}`, import.meta.url), "utf8");
    expect(
      lintDeterminism([
        { path, source: `${source}\nexport const t = Date.now();\n` },
      ]).map((problem) => problem.rule),
    ).toEqual(["clock"]);
  });
});
