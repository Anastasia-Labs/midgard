import { mkdtemp, rm } from "node:fs/promises";
import { setTimeout as waitForDisk } from "node:timers/promises";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
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

  it("holds the objective by name, never extending its authenticated deadline, when observations arrive during retry", async () => {
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout"] });
    const root = await mkdtemp("/var/tmp/midgard-objective-deadline-");
    paths.push(root);
    let now =
      MIDGARD_RETENTION_WINDOW.maturityMs -
      MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs -
      500;
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
    let settled = false;
    void supervisor.done.then(
      () => (settled = true),
      () => (settled = true),
    );
    now += 1_000;
    await vi.advanceTimersByTimeAsync(1_000);
    await waitFor(
      () =>
        supervisor.status().journalDecisionMissing.length === 1 &&
        supervisor.status().activeJob === null,
    );
    expect(supervisor.status()).toMatchObject({
      phase: "accepting",
      blockedJob: null,
      journalDecisionMissing: [
        {
          kind: "objective",
          category: "doubleSpend",
          headerHash: job.headerHash,
          detail: `doubleSpend/${job.headerHash}`,
          readiness: "fault_proof_start_deadline_passed",
        },
      ],
    });
    expect(settled).toBe(false);
    expect(run).toHaveBeenCalledTimes(1);
    await supervisor.close();
  });
});
