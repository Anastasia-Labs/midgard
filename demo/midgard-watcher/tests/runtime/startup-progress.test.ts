import { afterEach, expect, it, vi } from "vitest";

import { watcherFailureCauses } from "../../src/cli.js";
import { unsafeRunWatcherCommandForTest } from "../../src/runtime/scaffold.js";
import { createWatcherStartupProgress } from "../../src/runtime/startup-progress.js";

afterEach(() => vi.useRealTimers());

it("retains original and diagnostic failures once in bounded CLI cause details", () => {
  const original = new Error("classifier failed");
  const diagnostic = new Error("diagnostics failed");
  const failure = new AggregateError([original, diagnostic], "startup failed", {
    cause: original,
  });
  original.cause = failure;
  expect(watcherFailureCauses(failure).map(({ error }) => error)).toEqual([
    original.message,
    diagnostic.message,
  ]);
  expect(
    watcherFailureCauses(
      new AggregateError(
        Array.from({ length: 20 }, (_, index) => new Error(`failure ${index}`)),
      ),
    ),
  ).toHaveLength(8);
});

it("reports elapsed pending work and preserves the original startup failure", async () => {
  vi.useFakeTimers();
  const report = vi.fn();
  const progress = createWatcherStartupProgress(report);
  const failure = new Error("DA source unavailable");
  let reject!: (error: Error) => void;
  const pending = progress(
    "header_classification",
    () =>
      new Promise<never>((_, fail) => {
        reject = fail;
      }),
  );
  const rejected = expect(pending).rejects.toBe(failure);
  await vi.advanceTimersByTimeAsync(30_000);
  expect(report.mock.calls.map(([event]) => event.outcome)).toEqual([
    "started",
    "pending",
  ]);
  reject(failure);
  await rejected;
  expect(report).toHaveBeenLastCalledWith(
    expect.objectContaining({
      stage: "header_classification",
      outcome: "failed",
      error: failure.message,
    }),
  );
  await vi.advanceTimersByTimeAsync(60_000);
  expect(report).toHaveBeenCalledTimes(3);
});

it("returns completed work and stops its pending timer", async () => {
  vi.useFakeTimers();
  const report = vi.fn();
  expect(
    await createWatcherStartupProgress(report)(
      "user_event_catchup",
      async () => 42,
    ),
  ).toBe(42);
  await vi.advanceTimersByTimeAsync(60_000);
  expect(report.mock.calls.map(([event]) => event.outcome)).toEqual([
    "started",
    "completed",
  ]);
});

it("writes startup progress before runtime construction fails without claiming readiness", async () => {
  const output = vi.fn();
  const errors = vi.fn();
  const failure = new Error("classification failed");
  await expect(
    unsafeRunWatcherCommandForTest(
      "start",
      "/etc/watcher.json",
      {
        writeOutput: output,
        writeError: errors,
      },
      {
        runAuthority: async () => {
          throw new Error("Unexpected authority start");
        },
        runWatcher: async (_path, report) => {
          report({
            stage: "header_classification",
            outcome: "failed",
            elapsedMs: 1250,
            observedAt: "2026-09-11T00:00:00.000Z",
            error: failure.message,
          });
          throw failure;
        },
        waitForShutdown: async () => "SIGTERM",
      },
    ),
  ).rejects.toBe(failure);
  expect(output).not.toHaveBeenCalled();
  expect(JSON.parse(errors.mock.calls[0]![0])).toMatchObject({
    state: "starting",
    productionReady: false,
    stage: "header_classification",
    outcome: "failed",
    elapsedMs: 1250,
  });
});
