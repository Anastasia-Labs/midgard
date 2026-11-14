import { execFile } from "node:child_process";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

import { Cause, Exit, FiberId } from "effect";
import { describe, expect, it } from "vitest";

import { cliExitCode } from "../src/commands/cli-runtime.js";

describe("CLI exit status", () => {
  it("reports success only for a completed command", () => {
    expect(cliExitCode(Exit.succeed(undefined), undefined)).toBe(0);
    expect(cliExitCode(Exit.fail(new Error("refused")), undefined)).toBe(1);
  });

  it("reports a signal-interrupted command as failed with 128 + signal", () => {
    const interrupted = Exit.interrupt(FiberId.none);
    expect(cliExitCode(interrupted, "SIGTERM")).toBe(143);
    expect(cliExitCode(interrupted, "SIGINT")).toBe(130);
  });

  it("never reports an interruption without a signal as success", () => {
    expect(cliExitCode(Exit.interrupt(FiberId.none), undefined)).toBe(1);
    expect(
      cliExitCode(Exit.failCause(Cause.die(new Error("defect"))), "SIGTERM"),
    ).toBe(1);
  });
});

const execFileAsync = promisify(execFile);
const NODE_DIST = fileURLToPath(new URL("../dist/index.js", import.meta.url));

describe("CLI log lines", () => {
  // Worker threads log through Effect's logfmt logger; the main fiber must
  // match it, a UTC timestamp with its date, so one node log lines up with
  // journey logs and L1 slots (#816). They go to stderr, so a command's
  // stdout carries only its JSON result.
  it("logs the main fiber's lines to stderr, in UTC with a date, whatever TZ is", async () => {
    const failed = await execFileAsync(
      process.execPath,
      [NODE_DIST, "db:status"],
      {
        env: { PATH: process.env.PATH ?? "", TZ: "America/New_York" },
        encoding: "utf8",
      },
    ).then(
      () => undefined,
      (error: { code: number; stdout: string; stderr: string }) => error,
    );
    expect(failed?.code).toBe(1);
    expect(failed?.stdout).toBe("");
    const lines = (failed?.stderr ?? "").split("\n");
    expect(lines).toContainEqual(
      expect.stringMatching(
        /^timestamp=\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d{3}Z level=ERROR .*L1_OPERATOR_SEED_PHRASE/u,
      ),
    );
    expect(lines.filter((line) => /^\[\d{2}:\d{2}:/u.test(line))).toEqual([]);
  });
});
