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
