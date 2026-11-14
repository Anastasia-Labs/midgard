/**
 * A watcher on its way to a non-zero exit has the roles' shared exit
 * deadline to close its runtime: a close that hangs past it exits the
 * process anyway (exit code 70), naming the deadline. Its signal listeners
 * are gone once the runtime stops, so a later SIGTERM is not swallowed.
 */
import { describe, expect, it, vi } from "vitest";

import {
  unsafeRunWatcherCommandForTest,
  waitForShutdown,
} from "../../src/runtime/scaffold.js";

const runtime = (
  input: Readonly<{ done: Promise<void>; close: () => Promise<void> }>,
) => ({
  done: input.done,
  caughtUp: new Promise<void>(() => undefined),
  faultProofReadiness: [{ ready: true as const, category: "doubleSpend" }],
  recoveredFaultProofWorkflowCount: 0,
  faultProofSupervisor: {
    status: () => ({
      phase: "accepting" as const,
      recovered: true,
      deadlineHealth: "safe" as const,
    }),
  },
  close: input.close,
});

const stopped = () => Promise.reject(new Error("liveness ended"));

describe("the watcher's exit deadline", () => {
  it("exits 70 at the deadline when the runtime stops and its close hangs", async () => {
    const codes: number[] = [];
    const errors: string[] = [];
    void unsafeRunWatcherCommandForTest(
      "start",
      "/etc/watcher.json",
      { writeOutput: () => undefined, writeError: (text) => errors.push(text) },
      {
        runWatcher: async () =>
          runtime({
            done: stopped(),
            close: () => new Promise<void>(() => undefined),
          }),
        waitForShutdown: () => new Promise<"SIGTERM">(() => undefined),
        exit: (code) => codes.push(code),
        exitDeadlineMs: 20,
      },
    ).catch(() => undefined);
    await vi.waitFor(() => expect(codes).toEqual([70]), { timeout: 2_000 });
    expect(errors.map((line) => JSON.parse(line) as unknown)).toEqual([
      expect.objectContaining({
        command: "start",
        state: "close_deadline_exceeded",
        productionReady: false,
        deadlineMs: 20,
      }),
    ]);
  });

  it("does not exit when the close finishes in time, and rethrows the failure", async () => {
    const codes: number[] = [];
    await expect(
      unsafeRunWatcherCommandForTest(
        "start",
        "/etc/watcher.json",
        { writeOutput: () => undefined, writeError: () => undefined },
        {
          runWatcher: async () =>
            runtime({ done: stopped(), close: async () => undefined }),
          waitForShutdown: () => new Promise<"SIGTERM">(() => undefined),
          exit: (code) => codes.push(code),
          exitDeadlineMs: 20,
        },
      ),
    ).rejects.toThrow("liveness ended");
    await new Promise((resolve) => setTimeout(resolve, 60));
    expect(codes).toEqual([]);
  });

  it("removes its signal listeners once the runtime stops", async () => {
    const sigterm = process.listenerCount("SIGTERM");
    const sigint = process.listenerCount("SIGINT");
    await expect(
      unsafeRunWatcherCommandForTest(
        "start",
        "/etc/watcher.json",
        { writeOutput: () => undefined, writeError: () => undefined },
        {
          runWatcher: async () =>
            runtime({ done: stopped(), close: async () => undefined }),
          waitForShutdown,
        },
      ),
    ).rejects.toThrow("liveness ended");
    expect(process.listenerCount("SIGTERM")).toBe(sigterm);
    expect(process.listenerCount("SIGINT")).toBe(sigint);
  });
});
