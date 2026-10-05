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

it("declares the finite acceptance action on its own compiled tools entry", () => {
  const result = run("acceptance", "--help");
  expect(result.status).toBe(0);
  expect(result.stdout).toContain("--deadline <seconds>");
  expect(result.stdout.replace(/\s+/gu, " ")).toContain(
    "exact canonical payout verification remains required",
  );
  expect(result.stdout).not.toContain("--rounds");
  expect(result.stdout).not.toContain("--stable");
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

it("refuses a timeout-relaxation option before reaching the run", () => {
  const context = fakeContext();
  const runDir = join(context.layout.nodeRoot, "absent-run");
  const result = run(
    "acceptance",
    "--run-dir",
    runDir,
    "--deadline",
    "3600",
    "--stable",
    "1",
  );
  expect(result.status).toBe(1);
  expect(result.stderr).toContain("unknown option '--stable'");
  expect(existsSync(runDir)).toBe(false);
});
