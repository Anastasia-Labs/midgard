/**
 * The forced-order projection's lints (plan §5.5, §12): its table declares
 * a class and a retention and is registered as D-t on both dialects, and
 * the modules its S3 derivation runs read no clock, randomness or network.
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
  FORCED_ORDER_TABLES,
  forcedOrderMigrations,
} from "../src/forced-orders/index.js";

/**
 * What runs in the writer transaction (`forcedOrderDerivation`) and every
 * module it imports: each forced-order module except the driver side (the
 * ingest hook, the horizon, the reads, the row builder and the barrel),
 * which runs outside the writer transaction against the node's database.
 */
const LINTED = lintDeterminismModules({
  root: fileURLToPath(new URL("..", import.meta.url)),
  include: ["src/forced-orders/*.ts"],
  exclude: [
    "src/forced-orders/entry.ts",
    "src/forced-orders/horizon.ts",
    "src/forced-orders/index.ts",
    "src/forced-orders/ingest.ts",
    "src/forced-orders/reads.ts",
  ],
});

/** The modules the lint once named by hand: it must still reach each. */
const DERIVATION_MODULES = [
  "src/forced-orders/carriage.ts",
  "src/forced-orders/config.ts",
  "src/forced-orders/derive.ts",
  "src/forced-orders/projection.ts",
  "src/forced-orders/schema.ts",
];

const read = (path: string) => ({
  path,
  source: readFileSync(new URL(`../${path}`, import.meta.url), "utf8"),
});

describe("forced-order projection lints", () => {
  it("declares a class and retention for its table, and registers it D-t, on both dialects", () => {
    const registry = createTemporalRegistry([...FORCED_ORDER_TABLES]);
    for (const dialect of ["postgres", "sqlite"] as const)
      expect(
        lintSchema(
          [followerMigrations(dialect), forcedOrderMigrations(dialect)],
          registry,
        ),
      ).toEqual([]);
    expect(
      declaredTables([forcedOrderMigrations("postgres")]).map((table) => [
        table.table,
        table.tableClass,
      ]),
    ).toEqual([["node_l1_forced_order_fields", "D-t"]]);
    // The lint is live: the same table unregistered is reported.
    expect(
      lintSchema(
        [followerMigrations("postgres"), forcedOrderMigrations("postgres")],
        createTemporalRegistry([]),
      ),
    ).not.toEqual([]);
  });

  it("keeps the derivation and its imports free of clocks, randomness, the network and the host", () => {
    expect(LINTED.problems).toEqual([]);
    expect(LINTED.files).toEqual(
      expect.arrayContaining(DERIVATION_MODULES) as unknown,
    );
    const derive = read("src/forced-orders/derive.ts");
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
