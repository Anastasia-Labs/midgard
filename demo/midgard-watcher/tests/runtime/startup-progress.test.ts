import {
  SidecarExitedError,
  TransportRequestError,
} from "@al-ft/l1-node-transport";
import { FraudProofL1UnavailableError } from "@al-ft/midgard-fault-proofs";
import { afterEach, expect, it, vi } from "vitest";

import { watcherFailureCauses } from "../../src/cli.js";
import { unsafeRunWatcherCommandForTest } from "../../src/runtime/scaffold.js";
import {
  createWatcherStartupProgress,
  WatcherStartupStageHeld,
} from "../../src/runtime/startup-progress.js";

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

it("runs a held stage again, reporting the wait, and completes once", async () => {
  const report = vi.fn();
  const action = vi
    .fn<() => Promise<string>>()
    .mockRejectedValueOnce(
      new WatcherStartupStageHeld("waiting for the follower to reach the tip"),
    )
    .mockResolvedValueOnce("recovered");
  await expect(
    createWatcherStartupProgress(report, () => 1)("follower_ready", action),
  ).resolves.toBe("recovered");
  expect(action).toHaveBeenCalledTimes(2);
  expect(report.mock.calls.map(([event]) => event)).toEqual([
    expect.objectContaining({ stage: "follower_ready", outcome: "started" }),
    expect.objectContaining({
      stage: "follower_ready",
      outcome: "pending",
      error: "waiting for the follower to reach the tip",
      retryAfterMs: 1,
    }),
    expect.objectContaining({ stage: "follower_ready", outcome: "completed" }),
  ]);
});

it("runs a held stage again without a progress reporter too", async () => {
  const action = vi
    .fn<() => Promise<number>>()
    .mockRejectedValueOnce(new WatcherStartupStageHeld("held"))
    .mockResolvedValueOnce(7);
  await expect(
    createWatcherStartupProgress(undefined, () => 1)("follower_ready", action),
  ).resolves.toBe(7);
  expect(action).toHaveBeenCalledTimes(2);
});

it.each([
  [
    "a genuine refusal",
    "header_classification",
    new Error("deployment reference script differs from the verified manifest"),
  ],
  [
    "an L1 transient in an identity stage",
    "deployment_authority",
    new FraudProofL1UnavailableError("down"),
  ],
  [
    "an L1 transient a stage that keeps what it allocates raised outside its retried reads",
    "workflow_readiness",
    new FraudProofL1UnavailableError("down"),
  ],
])("still fails startup on %s, at once", async (_label, stage, failure) => {
  const report = vi.fn();
  const action = vi.fn(async () => {
    throw failure;
  });
  await expect(
    createWatcherStartupProgress(report, () => 1)(stage, action),
  ).rejects.toBe(failure);
  expect(action).toHaveBeenCalledOnce();
  expect(report.mock.calls.map(([event]) => event.outcome)).toEqual([
    "started",
    "failed",
  ]);
});

it("repeats only a stage's L1 read through a transient, never what the stage allocated, and completes once", async () => {
  const report = vi.fn();
  let allocations = 0;
  const read = vi
    .fn<() => Promise<string>>()
    .mockRejectedValueOnce(
      new FraudProofL1UnavailableError("Kupo is re-indexing"),
    )
    .mockRejectedValueOnce(
      Object.assign(new TypeError("fetch failed"), {
        cause: Object.assign(new Error("connect ECONNREFUSED"), {
          code: "ECONNREFUSED",
        }),
      }),
    )
    .mockResolvedValueOnce("ready");
  await expect(
    createWatcherStartupProgress(report, () => 1)(
      "workflow_readiness",
      async ({ retryL1Read }) => {
        allocations += 1;
        return await retryL1Read(read);
      },
    ),
  ).resolves.toBe("ready");
  expect(allocations).toBe(1);
  expect(read).toHaveBeenCalledTimes(3);
  expect(report.mock.calls.map(([event]) => event.outcome)).toEqual([
    "started",
    "pending",
    "pending",
    "completed",
  ]);
  expect(report.mock.calls[1]![0]).toMatchObject({
    stage: "workflow_readiness",
    error: "Kupo is re-indexing",
    retryAfterMs: 1,
  });
});

it("fails a stage's retried L1 read at once on a genuine refusal", async () => {
  const report = vi.fn();
  const refusal = new Error(
    "deployment reference script differs from the verified manifest",
  );
  const read = vi.fn(async () => {
    throw refusal;
  });
  await expect(
    createWatcherStartupProgress(report, () => 1)(
      "workflow_readiness",
      async ({ retryL1Read }) => await retryL1Read(read),
    ),
  ).rejects.toBe(refusal);
  expect(read).toHaveBeenCalledOnce();
  expect(report.mock.calls.map(([event]) => event.outcome)).toEqual([
    "started",
    "failed",
  ]);
});

it("waits out a sidecar exit and a node timeout on the protocol parameters read, and completes without failing startup", async () => {
  const report = vi.fn();
  const read = vi
    .fn<() => Promise<string>>()
    .mockRejectedValueOnce(
      new SidecarExitedError({
        code: 1,
        signal: null,
        fatal: null,
        diagnostics: "",
      }),
    )
    .mockRejectedValueOnce(
      new TransportRequestError(
        "node_timeout",
        "the node did not answer the query in time",
      ),
    )
    .mockResolvedValueOnce("parameters");
  await expect(
    createWatcherStartupProgress(report, () => 1)(
      "protocol_parameters",
      async ({ retryL1Read }) => await retryL1Read(read),
    ),
  ).resolves.toBe("parameters");
  expect(read).toHaveBeenCalledTimes(3);
  expect(report.mock.calls.map(([event]) => event.outcome)).toEqual([
    "started",
    "pending",
    "pending",
    "completed",
  ]);
});
