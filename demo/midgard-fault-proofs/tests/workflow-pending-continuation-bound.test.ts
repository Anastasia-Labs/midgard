import { mkdtemp, rm } from "node:fs/promises";
import { setTimeout as realDelay } from "node:timers/promises";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { unsafeCreateWatcherFaultProofSupervisorForTest } from "midgard-watcher";
import { afterEach, describe, expect, it, vi } from "vitest";

import { FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED } from "../src/workflow/orchestrator.js";
import {
  WORKFLOW_CONTINUATION_QUIET_BOUND_MS,
  WORKFLOW_CONTINUATION_QUIET_BOUND_POLLS,
} from "../src/workflow/pending-continuation.js";
import { createManifestBoundWorkflowRunner } from "../src/workflow/runtime.js";
import {
  admittedActuation,
  DEPLOYMENT,
} from "./workflow-runtime.admitted-actuation.js";
import {
  doubleSpendRecord,
  loadedRuntime,
} from "./workflow-runtime.slash-funding-fixture.js";

const paths: string[] = [];
afterEach(async () => {
  vi.useRealTimers();
  for (const path of paths.splice(0))
    await rm(path, { recursive: true, force: true });
});

const scratch = async (): Promise<string> => {
  const path = await mkdtemp("/var/tmp/midgard-pending-bound-");
  paths.push(path);
  return path;
};

/** Waits on the real clock while the fake one stands still. */
const waitFor = async (predicate: () => boolean): Promise<void> => {
  for (let attempt = 0; attempt < 400; attempt++) {
    if (predicate()) return;
    await realDelay(5);
  }
  throw new Error("the expected state was not reached");
};

type Entry = Readonly<{ event: Readonly<Record<string, unknown>> }>;

/**
 * A record session's journal as the orchestrator writes it: every poll of an
 * unconfirmed transaction appends a pending reconciliation; a confirmation is
 * progress.
 */
const recordJournal = () => {
  const entries: Entry[] = [
    { event: { kind: "submitted", actionId: "walk", txHash: "aa" } },
  ];
  return {
    entries,
    pollPending: () => {
      entries.push({
        event: { kind: "reconciled", actionId: "walk", outcome: "pending" },
      });
      return {
        kind: "pending",
        reason: "transaction for walk is pending",
        entries: [...entries],
      };
    },
    confirm: (actionId: string) => {
      entries.push({ event: { kind: "confirmed", actionId, txHash: "bb" } });
      return {
        kind: "pending",
        reason: `transaction for ${actionId} is pending`,
        entries: [...entries],
      };
    },
  };
};

/**
 * A manifest-bound runner for one record. Each call is one admitted session
 * with its own permits, as the watcher mints them for every run.
 */
const runnerFor = async (execute: () => Promise<unknown>) => {
  let actuation = await admittedActuation();
  const close = vi.fn(async () => undefined);
  const runner = createManifestBoundWorkflowRunner({
    record: doubleSpendRecord({
      bindConfig: () => ({}),
      constructWorkflow: async () => ({
        binding: {
          deploymentFingerprint: DEPLOYMENT,
          definition: {
            category: "doubleSpend" as const,
            headerHash: actuation.headerHash,
          },
        },
      }),
      execute,
    }),
    loadRuntime: async () => loadedRuntime(actuation, { close }),
  });
  const journalDirectory = await scratch();
  let sessions = 0;
  const runOrResume = async (mode: "run" | "resume") => {
    if (sessions++ > 0) actuation = await admittedActuation();
    return await runner.runOrResume({
      mode,
      category: "doubleSpend",
      deploymentFingerprint: DEPLOYMENT,
      headerHash: actuation.headerHash,
      decisionDigest: actuation.decisionDigest,
      actuationPermit: actuation.actuationPermit,
      fundingReservationPermit: actuation.fundingReservationPermit,
      journalDirectory,
      runtimeConfigPath: "/etc/midgard/runtime.json",
    });
  };
  return { runOrResume, close };
};

const settledFlag = <T>(promise: Promise<T>) => {
  const state = { settled: false, value: undefined as T | undefined };
  void promise.then((value) => {
    state.settled = true;
    state.value = value;
  });
  return state;
};

describe("bounded pending continuation", () => {
  it("hands a pending that never confirms back as a typed stall within its bound, and closes the session", async () => {
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout", "Date"] });
    const journal = recordJournal();
    const execute = vi.fn(async () => journal.pollPending());
    const runner = await runnerFor(execute);
    const running = settledFlag(runner.runOrResume("run"));
    await vi.advanceTimersByTimeAsync(0);
    expect(execute).toHaveBeenCalledOnce();
    await vi.advanceTimersByTimeAsync(WORKFLOW_CONTINUATION_QUIET_BOUND_MS);
    await waitFor(() => running.settled);
    expect(running.value).toMatchObject({
      kind: "pending",
      resumeOnObservation: true,
      reason: expect.stringMatching(
        new RegExp(
          `^${FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED}: .*last pending: transaction for walk is pending$`,
          "u",
        ),
      ),
    });
    expect(execute.mock.calls.length).toBeLessThanOrEqual(
      WORKFLOW_CONTINUATION_QUIET_BOUND_POLLS + 1,
    );
    expect(runner.close).toHaveBeenCalledOnce();
  });

  it("keeps a session that journals progress past the bound until it completes", async () => {
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout", "Date"] });
    const journal = recordJournal();
    // One confirmation every 20 polls, for three bounds' worth of time.
    const polls = (3 * WORKFLOW_CONTINUATION_QUIET_BOUND_MS) / 1_000;
    const completed = { kind: "completed", workflowId: "long-record" };
    const execute = vi.fn(async () =>
      execute.mock.calls.length > polls
        ? completed
        : execute.mock.calls.length % 20 === 0
          ? journal.confirm(`walk-${execute.mock.calls.length.toString()}`)
          : journal.pollPending(),
    );
    const runner = await runnerFor(execute);
    const running = settledFlag(runner.runOrResume("run"));
    await vi.advanceTimersByTimeAsync(polls * 1_000 + 1_000);
    await waitFor(() => running.settled);
    expect(running.value).toBe(completed);
    expect(execute).toHaveBeenCalledTimes(polls + 1);
  });

  it("lets a second objective queued behind a stalled one run to conviction, then resumes the stalled one on its next observation", async () => {
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout", "Date"] });
    const stalledHeader = "0a".repeat(28);
    const secondHeader = "fb".repeat(28);
    const offset =
      MIDGARD_RETENTION_WINDOW.maturityMs -
      MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs;
    const deadline = (headerHash: string, blockTimeMs: number) => ({
      headerHash,
      headerBlockTimeMs: blockTimeMs.toString(),
      maturityAtMs: (
        blockTimeMs + MIDGARD_RETENTION_WINDOW.maturityMs
      ).toString(),
      latestSafeStartAtMs: (blockTimeMs + offset).toString(),
    });
    const job = (headerHash: string, blockTimeMs: number, revision: string) =>
      ({
        mode: "run",
        category: "doubleSpend",
        headerHash,
        decisionDigest: "cd".repeat(32),
        rollbackGeneration: "0",
        observationRevision: revision,
        deadline: deadline(headerHash, blockTimeMs),
      }) as const;
    const journal = recordJournal();
    // The first objective's L1 never confirms until the second has convicted.
    let secondConvicted = false;
    const completed = { kind: "completed", workflowId: "first-objective" };
    const execute = vi.fn(async () =>
      secondConvicted ? completed : journal.pollPending(),
    );
    const first = await runnerFor(execute);
    const order: string[] = [];
    const outcomes: unknown[] = [];
    const run = vi.fn(async (scheduled: { headerHash: string }) => {
      if (scheduled.headerHash === secondHeader) {
        order.push("second");
        secondConvicted = true;
        return { kind: "completed", result: { kind: "completed" } };
      }
      order.push("first");
      const result = (await first.runOrResume("run")) as {
        kind: string;
        reason?: string;
      };
      outcomes.push(result);
      return result.kind === "pending"
        ? {
            kind: "pending",
            resume: "await_observation",
            reason: result.reason,
          }
        : { kind: "completed", result };
    });
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: await scratch(),
      deploymentFingerprint: "dd".repeat(32),
      run,
    });
    let failure: unknown;
    void supervisor.done.catch((error: unknown) => {
      failure = error;
    });
    await supervisor.recoverExisting(null);
    await supervisor.unsafeScheduleForTest(job(stalledHeader, 0, "first"));
    await waitFor(() => execute.mock.calls.length === 1);
    // The second fault arrives behind it, and the first is observed again
    // while its session still waits.
    await supervisor.unsafeScheduleForTest(job(secondHeader, 60_000, "first"));
    await supervisor.unsafeScheduleForTest(job(stalledHeader, 0, "second"));
    expect(order).toEqual(["first"]);
    await vi.advanceTimersByTimeAsync(WORKFLOW_CONTINUATION_QUIET_BOUND_MS);
    await waitFor(
      () =>
        failure !== undefined ||
        (order.length === 3 && supervisor.status().activeJob === null),
    );
    if (failure !== undefined) throw failure;
    expect(order).toEqual(["first", "second", "first"]);
    expect(outcomes).toEqual([
      expect.objectContaining({
        kind: "pending",
        resumeOnObservation: true,
        reason: expect.stringContaining(
          FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED,
        ),
      }),
      completed,
    ]);
    expect(run).toHaveBeenCalledTimes(3);
    await supervisor.close();
  });
});
