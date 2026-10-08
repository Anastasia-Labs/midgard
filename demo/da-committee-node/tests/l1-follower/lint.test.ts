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
  COMMITTEE_PINNED_BLOCKS_TABLE,
  COMMITTEE_PINNED_HEADERS_TABLE,
  COMMITTEE_PINNED_TXS_TABLE,
  COMMITTEE_QUEUE_TABLE,
  COMMITTEE_QUEUE_TABLE_SPEC,
  committeeMigrations,
} from "../../src/l1/follower/queue-table.js";

/**
 * Every committee derivation over the facts (plan §6: pure functions of
 * them) and every module it imports: each follower module except the
 * process that runs the follower and its configuration.
 */
const LINTED = lintDeterminismModules({
  root: fileURLToPath(new URL("../..", import.meta.url)),
  include: ["src/l1/follower/*.ts"],
  exclude: [
    "src/l1/follower/l1-follower.ts",
    "src/l1/follower/committee-follower-config.ts",
  ],
});

/** The derivations the lint once named by hand: it must still reach each. */
const DERIVATIONS = [
  "queue-table.ts",
  "queue-derivation.ts",
  "landed-queue.ts",
  "obligations.ts",
  "projection.ts",
].map((name) => `src/l1/follower/${name}`);

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
      ).toEqual([
        [COMMITTEE_QUEUE_TABLE, "D-t"],
        // The committee's retention pins: its own records, never rewound.
        [COMMITTEE_PINNED_BLOCKS_TABLE, "B"],
        [COMMITTEE_PINNED_TXS_TABLE, "B"],
        [COMMITTEE_PINNED_HEADERS_TABLE, "B"],
      ]);
    }
  });

  it("passes the determinism lint on every derivation and its imports", () => {
    expect(LINTED.problems).toEqual([]);
    expect(LINTED.files).toEqual(
      expect.arrayContaining(DERIVATIONS) as unknown,
    );
  });

  it("would catch a clock read in a derivation", () => {
    const path = new URL(
      "../../src/l1/follower/projection.ts",
      import.meta.url,
    );
    expect(
      lintDeterminism([
        {
          path: path.pathname,
          source: `${readFileSync(path, "utf8")}\nexport const stamp = () => Date.now();\n`,
        },
      ]).length,
    ).toBeGreaterThan(0);
  });
});
