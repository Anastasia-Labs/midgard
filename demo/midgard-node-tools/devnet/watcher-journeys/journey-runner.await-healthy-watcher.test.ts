import { describe, expect, it } from "vitest";

import {
  awaitHealthyWatcher,
  type JourneyHealthyWatcherStatus,
} from "./journey-runner.await-healthy-watcher.js";

/** A stack watcher's `/v1/status`, ready, with these alerts active. */
const ready = (activeAlerts: unknown[] = []): JourneyHealthyWatcherStatus => ({
  liveness: "live",
  readiness: "ready",
  readinessReasons: [],
  activeAlerts,
  launchScope: { complete: true },
});
/** The alert a stack watcher raises on an L1 rewind until it decides past it. */
const rollback = {
  code: "chain_rollback",
  subjectDigest: "ab".repeat(32),
  active: true,
  observedAtMs: "1794350000000",
};

/** Answers each read with the next status; the last one repeats. */
const watcher = (statuses: JourneyHealthyWatcherStatus[]) => {
  let reads = 0;
  return {
    status: () =>
      Promise.resolve(statuses[Math.min(reads++, statuses.length - 1)]!),
    reads: () => reads,
  };
};

/** The runner's progress-budgeted wait, with a stall of `attempts` reads. */
const pollsOf = (attempts: number) => ({
  pollWhileReplaying: async <T>(
    name: string,
    action: () => Promise<T | undefined>,
  ): Promise<T> => {
    for (let attempt = 0; attempt < attempts; attempt += 1) {
      const result = await action();
      if (result !== undefined) return result;
    }
    throw new Error(`Timed out waiting for ${name}`);
  },
});

const run = (stack: ReturnType<typeof watcher>, attempts: number) =>
  awaitHealthyWatcher({
    name: "healthy processing after correction",
    observe: () => Promise.resolve("healthy decision"),
    status: stack.status,
    polls: pollsOf(attempts),
    progress: () => Promise.resolve(0),
    stallTimeoutMs: 1_800_000,
  });

describe("healthy processing after correction", () => {
  it("reads a ready watcher again while its rollback alert is active, and records the read that cleared", async () => {
    const stack = watcher([ready([rollback]), ready()]);
    await expect(run(stack, 5)).resolves.toEqual({
      decision: "healthy decision",
      status: ready(),
    });
    expect(stack.reads()).toBe(2);
  });

  it("fails naming the alerts that never cleared within the allowance", async () => {
    const stack = watcher([ready([rollback])]);
    await expect(run(stack, 3)).rejects.toThrow(
      /still active.*chain_rollback/su,
    );
    expect(stack.reads()).toBe(3);
  });

  it("fails at once on a ready watcher that is not live", async () => {
    const stack = watcher([{ ...ready(), liveness: "degraded" }]);
    await expect(run(stack, 3)).rejects.toThrow();
    expect(stack.reads()).toBe(1);
  });
});
