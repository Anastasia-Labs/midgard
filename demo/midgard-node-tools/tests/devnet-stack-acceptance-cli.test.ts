import { spawnSync } from "node:child_process";
import { existsSync } from "node:fs";
import { join, resolve } from "node:path";

import { afterEach, expect, it } from "vitest";

import {
  fakeContext,
  removeFakeContexts,
} from "./devnet-stack-journey.fixtures.js";

afterEach(removeFakeContexts);

const run = (...args: string[]) =>
  spawnSync(process.execPath, [resolve("dist/devnet-stack.js"), ...args], {
    encoding: "utf8",
    timeout: 20_000,
  });

it.each(["0", "Infinity", "2147484"])(
  "refuses deadline %s before wiring any run locks or resources",
  (deadline) => {
    const context = fakeContext();
    const runDir = join(context.layout.nodeRoot, "absent-run");
    const result = run(
      "acceptance",
      "--run-dir",
      runDir,
      "--deadline",
      deadline,
    );
    expect(result.status).toBe(1);
    expect(result.stderr).toContain(
      "--deadline must be a positive whole number within Node's timer range",
    );
    expect(existsSync(runDir)).toBe(false);
  },
);
