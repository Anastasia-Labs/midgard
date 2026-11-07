import { readFileSync } from "node:fs";
import { join } from "node:path";

import { afterEach, expect, it } from "vitest";

import { acceptanceExec } from "../src/devnet-stack/acceptance-process.js";
import {
  fakeContext,
  removeFakeContexts,
} from "./devnet-stack-journey.fixtures.js";

afterEach(removeFakeContexts);

it("preserves a real child failure code and joins its transcript", async () => {
  const context = fakeContext();
  const result = await acceptanceExec(
    process.execPath,
    ["-e", "process.stderr.write('exact child failure');process.exitCode=7"],
    {
      cwd: context.layout.nodeRoot,
      env: {},
      logDir: join(context.layout.nodeRoot, "logs"),
      label: "failure",
      signal: new AbortController().signal,
      timeoutMs: 5000,
    },
  );
  expect(result.code).toBe(7);
  expect(result.stderr).toBe("exact child failure");
  expect(readFileSync(result.log, "utf8")).toContain("exit code=7");
});

it("refuses a timed-out child even when its SIGTERM handler exits zero", async () => {
  const context = fakeContext();
  const pidFile = join(context.layout.nodeRoot, "pid");
  const task = acceptanceExec(
    process.execPath,
    [
      "-e",
      "process.on('SIGTERM',()=>process.exit(0));require('fs').writeFileSync(process.argv[1],String(process.pid));setInterval(()=>{},1000)",
      pidFile,
    ],
    {
      cwd: context.layout.nodeRoot,
      env: {},
      logDir: join(context.layout.nodeRoot, "logs"),
      label: "timeout",
      signal: new AbortController().signal,
      timeoutMs: 300,
    },
  );
  await expect(task).rejects.toThrow(/command timed out/);
  const pid = Number(readFileSync(pidFile, "utf8"));
  expect(pid).toBeGreaterThan(0);
  expect(() => process.kill(pid, 0)).toThrow();
});
