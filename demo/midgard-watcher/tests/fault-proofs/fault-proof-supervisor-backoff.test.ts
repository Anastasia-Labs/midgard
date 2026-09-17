import { mkdtemp, rm } from "node:fs/promises";
import { setTimeout as waitForDisk } from "node:timers/promises";

import { afterEach, describe, expect, it, vi } from "vitest";

import { unsafeCreateWatcherFaultProofSupervisorForTest } from "../../src/fault-proofs/fault-proof-supervisor.js";

const paths: string[] = [];
afterEach(async () => {
  vi.useRealTimers();
  await Promise.all(
    paths.splice(0).map((path) => rm(path, { recursive: true, force: true })),
  );
});
const waitFor = async (predicate: () => boolean) => {
  for (let attempt = 0; attempt < 1_000; attempt++) {
    if (predicate()) return;
    await waitForDisk(5);
  }
  throw new Error("supervisor did not reach its expected state");
};
const job = {
  mode: "run" as const,
  category: "doubleSpend" as const,
  headerHash: "ab".repeat(28),
  decisionDigest: "cd".repeat(32),
  rollbackGeneration: "0",
  observationRevision: "first",
};
const retryable = {
  kind: "retryable",
  resume: "backoff",
  reason: "controlled transport loss",
  retryAfterMs: 1_000,
};

describe("objective progress transport backoff", () => {
  it("coalesces observations during backoff and cancels its timer on close", async () => {
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout"] });
    const root = await mkdtemp("/var/tmp/midgard-objective-backoff-");
    paths.push(root);
    const run = vi.fn(async () => retryable);
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: root,
      deploymentFingerprint: "dd".repeat(32),
      run,
    });
    let failure: unknown;
    void supervisor.done.catch((error: unknown) => {
      failure = error;
    });
    await supervisor.recoverExisting(null);
    await supervisor.unsafeScheduleForTest(job);
    await waitFor(
      () =>
        run.mock.calls.length === 1 && supervisor.status().activeJob === null,
    );
    for (let index = 0; index < 20; index++)
      await supervisor.unsafeScheduleForTest({
        ...job,
        observationRevision: "newer",
      });
    expect(run).toHaveBeenCalledTimes(1);
    expect(vi.getTimerCount()).toBe(1);
    await vi.advanceTimersByTimeAsync(1_000);
    await waitFor(
      () =>
        failure !== undefined ||
        (run.mock.calls.length === 2 && supervisor.status().activeJob === null),
    );
    if (failure !== undefined) throw failure;
    expect(vi.getTimerCount()).toBe(1);
    await vi.advanceTimersByTimeAsync(1_000);
    expect(run).toHaveBeenCalledTimes(2);
    await supervisor.close();
    expect(vi.getTimerCount()).toBe(0);
  });

  it("does not extend the authenticated deadline when observations arrive during retry", async () => {
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout"] });
    const root = await mkdtemp("/var/tmp/midgard-objective-deadline-");
    paths.push(root);
    let now = 302_399_500;
    const run = vi.fn(async () => retryable);
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: root,
      deploymentFingerprint: "dd".repeat(32),
      run,
      unsafeNowMsForTest: () => now,
    });
    await supervisor.recoverExisting(null);
    await supervisor.unsafeScheduleForTest(job);
    await waitFor(
      () =>
        run.mock.calls.length === 1 && supervisor.status().activeJob === null,
    );
    await supervisor.unsafeScheduleForTest({
      ...job,
      observationRevision: "newer",
    });
    const stopped = expect(supervisor.done).rejects.toThrow(
      "deadline is unsafe",
    );
    now += 1_000;
    await vi.advanceTimersByTimeAsync(1_000);
    await stopped;
    expect(run).toHaveBeenCalledTimes(1);
    await supervisor.close();
  });
});
