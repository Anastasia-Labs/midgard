/**
 * The forced-order projection's lints (plan §5.5, §12): its table declares
 * a class and a retention and is registered as D-t on both dialects, and
 * the modules its S3 derivation runs read no clock, randomness or network.
 */
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
  FORCED_ORDER_TABLES,
  forcedOrderMigrations,
} from "../src/forced-orders/index.js";

/** What runs in the writer transaction (`forcedOrderDerivation` and its imports). */
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

  it("keeps the derivation modules free of clocks, randomness and the network", () => {
    const files = DERIVATION_MODULES.map(read);
    expect(lintDeterminism(files)).toEqual([]);
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
