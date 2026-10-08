import { readFileSync } from "node:fs";
import { fileURLToPath } from "node:url";

import {
  lintDeterminism,
  lintDeterminismModules,
} from "@al-ft/midgard-l1-follower/lint";
import { describe, expect, it } from "vitest";

/**
 * The node's event driver and ingestion entries (beside the shared event
 * projection, which the follower package lints) and every module they
 * import.
 */
const LINTED = lintDeterminismModules({
  root: fileURLToPath(new URL("..", import.meta.url)),
  include: ["src/l1-events/*.ts"],
});

const NODE_MODULES = ["src/l1-events/driver.ts", "src/l1-events/entries.ts"];

const read = (path: string) => ({
  path,
  source: readFileSync(new URL(`../${path}`, import.meta.url), "utf8"),
});

describe("node event driver lints", () => {
  it("keeps the modules and their imports free of clocks, randomness, the network and the host", () => {
    expect(LINTED.problems).toEqual([]);
    expect(LINTED.files).toEqual(
      expect.arrayContaining(NODE_MODULES) as unknown,
    );
    // The lint is live on these files: a clock read in the driver is caught.
    const driver = read("src/l1-events/driver.ts");
    expect(
      lintDeterminism([
        {
          ...driver,
          source: `${driver.source}\nexport const t = Date.now();\n`,
        },
      ]).map((problem) => problem.rule),
    ).toEqual(["clock"]);
  });
});
