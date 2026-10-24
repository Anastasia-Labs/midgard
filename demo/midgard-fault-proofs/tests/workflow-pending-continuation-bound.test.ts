import { mkdtemp, rm } from "node:fs/promises";
import { setTimeout as realDelay } from "node:timers/promises";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { unsafeCreateWatcherFaultProofSupervisorForTest } from "midgard-watcher";
import { afterEach, describe, expect, it, vi } from "vitest";

import { installWorkflowApplicationRegistry } from "../src/workflow/adapters.js";
import { runFraudProofWorkflowCli } from "../src/workflow/cli.js";
import { FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED } from "../src/workflow/orchestrator.js";
import {
  WORKFLOW_CONTINUATION_QUIET_BOUND_MS,
  WORKFLOW_CONTINUATION_QUIET_BOUND_POLLS,
} from "../src/workflow/pending-continuation.js";
import {
  createManifestBoundWorkflowRunner,
  WORKFLOW_RUNNER_FACTORIES,
} from "../src/workflow/runtime.js";
import {
  admittedActuation,
  DEPLOYMENT,
  directoryJournal,
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

/** A record session's journal as the orchestrator writes it: every poll of
 * an unconfirmed transaction appends a pending reconciliation; a
 * confirmation is progress. */
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
    /** The last attempt expired unseen; the same action is submitted again. */
    resubmit: () => {
      const attempt = entries.filter(
        ({ event }) => event.kind === "submitted",
      ).length;
      const txHash = (attempt + 1).toString(16).padStart(2, "0");
      entries.push(
        {
          event: { kind: "reconciled", actionId: "walk", outcome: "not_found" },
        },
        { event: { kind: "signed_attempt_retired", txHash: "aa" } },
        { event: { kind: "preflight_passed", actionId: "walk", txHash } },
        {
          event: {
            kind: "submission_intent",
            actionId: "walk",
            attempt: attempt + 1,
            txHash,
          },
        },
        {
          event: {
            kind: "submitted",
            actionId: "walk",
            attempt: attempt + 1,
            txHash,
          },
        },
      );
      return {
        kind: "pending",
        reason: "transaction for walk is pending",
        entries: [...entries],
      };
    },
    /** The next action is preflighted, intended and submitted. */
    startAction: (actionId: string) => {
      entries.push(
        { event: { kind: "preflight_passed", actionId, txHash: "cc" } },
        {
          event: {
            kind: "submission_intent",
            actionId,
            attempt: 1,
            txHash: "cc",
          },
        },
        { event: { kind: "submitted", actionId, attempt: 1, txHash: "cc" } },
      );
      return {
        kind: "pending",
        reason: `transaction for ${actionId} is pending`,
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
const runnerFor = async (
  execute: () => Promise<unknown>,
  onProgress?: () => void,
) => {
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
      journal: directoryJournal(journalDirectory),
      runtimeConfigPath: "/etc/midgard/runtime.json",
      onProgress,
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

/**
 * A deadline fault whose L1 never confirms until a second fault queued behind
 * it has convicted, re-observed while its session waits; `later` also queues
 * a third fault with a later deadline. Resolves once every run has finished.
 */
const queueBehindStall = async (later: boolean) => {
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
  let secondConvicted = false;
  const completed = { kind: "completed", workflowId: "first-objective" };
  const execute = vi.fn(async () =>
    secondConvicted ? completed : journal.pollPending(),
  );
  const first = await runnerFor(execute);
  const order: string[] = [];
  const outcomes: unknown[] = [];
  const run = vi.fn(async (scheduled: { headerHash: string }) => {
    if (scheduled.headerHash !== stalledHeader) {
      secondConvicted ||= scheduled.headerHash === secondHeader;
      order.push(scheduled.headerHash === secondHeader ? "second" : "later");
      return { kind: "completed", result: { kind: "completed" } };
    }
    order.push("first");
    const result = (await first.runOrResume("run")) as {
      kind: string;
      reason?: string;
    };
    outcomes.push(result);
    return result.kind === "pending"
      ? { kind: "pending", resume: "await_observation", reason: result.reason }
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
  if (later)
    await supervisor.unsafeScheduleForTest(
      job("fc".repeat(28), 120_000, "first"),
    );
  expect(order).toEqual(["first"]);
  await vi.advanceTimersByTimeAsync(WORKFLOW_CONTINUATION_QUIET_BOUND_MS);
  const runs = later ? 4 : 3;
  await waitFor(
    () =>
      failure !== undefined ||
      (order.length === runs && supervisor.status().activeJob === null),
  );
  if (failure !== undefined) throw failure;
  await supervisor.close();
  return { order, outcomes, completed, run };
};

describe("bounded pending continuation", () => {
  it("hands a pending that never confirms back as a typed stall within its bound, and closes the session", async () => {
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout", "Date"] });
    const journal = recordJournal();
    const execute = vi.fn(async () => journal.pollPending());
    const onProgress = vi.fn();
    const runner = await runnerFor(execute, onProgress);
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
    expect(onProgress).not.toHaveBeenCalled();
    expect(runner.close).toHaveBeenCalledOnce();
  });

  it("stalls on the clock when every poll is slow, long before the poll backstop", async () => {
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout", "Date"] });
    const journal = recordJournal();
    // Each poll's L1 reads take 9 s, so one poll ends every 10 s.
    const execute = vi.fn(async () => {
      vi.setSystemTime(Date.now() + 9_000);
      return journal.pollPending();
    });
    const runner = await runnerFor(execute);
    const running = settledFlag(runner.runOrResume("run"));
    for (let poll = 0; poll < 200 && !running.settled; poll++)
      await vi.advanceTimersByTimeAsync(1_000);
    await waitFor(() => running.settled);
    expect(running.value).toMatchObject({
      resumeOnObservation: true,
      reason: expect.stringContaining(
        FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED,
      ),
    });
    expect(execute.mock.calls.length).toBeLessThanOrEqual(
      WORKFLOW_CONTINUATION_QUIET_BOUND_MS / 10_000 + 1,
    );
  });

  it("stalls on the poll backstop when the clock stands still", async () => {
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout", "Date"] });
    const journal = recordJournal();
    const frozenAtMs = Date.now();
    const execute = vi.fn(async () => {
      vi.setSystemTime(frozenAtMs);
      return journal.pollPending();
    });
    const runner = await runnerFor(execute);
    const running = settledFlag(runner.runOrResume("run"));
    const polls = WORKFLOW_CONTINUATION_QUIET_BOUND_POLLS;
    for (let poll = 0; poll < polls + 100 && !running.settled; poll++)
      await vi.advanceTimersByTimeAsync(1_000);
    await waitFor(() => running.settled);
    expect(running.value).toMatchObject({
      resumeOnObservation: true,
      reason: expect.stringContaining(
        `no journaled progress for 0 ms over ${polls.toString()} polls`,
      ),
    });
    expect(execute).toHaveBeenCalledTimes(polls + 1);
  });

  it("stalls within its bound when the same action is accepted, expires unseen and is resubmitted every window", async () => {
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout", "Date"] });
    const journal = recordJournal();
    // Accepted at submission but never included: every 3 min validity window
    // the attempt is reconciled missing, retired and submitted again.
    const execute = vi.fn(async () =>
      execute.mock.calls.length % 180 === 0
        ? journal.resubmit()
        : journal.pollPending(),
    );
    const onProgress = vi.fn();
    const runner = await runnerFor(execute, onProgress);
    const startedAtMs = Date.now();
    const running = settledFlag(runner.runOrResume("run"));
    await vi.advanceTimersByTimeAsync(WORKFLOW_CONTINUATION_QUIET_BOUND_MS);
    await waitFor(() => running.settled);
    expect(Date.now() - startedAtMs).toBeLessThanOrEqual(
      WORKFLOW_CONTINUATION_QUIET_BOUND_MS,
    );
    expect(running.value).toMatchObject({
      kind: "pending",
      resumeOnObservation: true,
      reason: expect.stringContaining(
        FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED,
      ),
    });
    expect(
      journal.entries.filter(({ event }) => event.kind === "submitted").length,
    ).toBeGreaterThan(5);
    expect(onProgress).not.toHaveBeenCalled();
    expect(runner.close).toHaveBeenCalledOnce();
  });

  it("keeps a session that journals progress past the bound until it completes", async () => {
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout", "Date"] });
    const journal = recordJournal();
    // One confirmation every 20 polls, for three bounds' worth of time. Every
    // confirmation is progress, even one of an action confirmed before.
    const polls = (3 * WORKFLOW_CONTINUATION_QUIET_BOUND_MS) / 1_000;
    const completed = { kind: "completed", workflowId: "long-record" };
    const execute = vi.fn(async () =>
      execute.mock.calls.length > polls
        ? completed
        : execute.mock.calls.length % 20 === 0
          ? journal.confirm("walk")
          : journal.pollPending(),
    );
    const onProgress = vi.fn();
    const runner = await runnerFor(execute, onProgress);
    const running = settledFlag(runner.runOrResume("run"));
    await vi.advanceTimersByTimeAsync(polls * 1_000 + 1_000);
    await waitFor(() => running.settled);
    expect(running.value).toBe(completed);
    expect(execute).toHaveBeenCalledTimes(polls + 1);
    // Each confirmation is reported as it lands, before the session ends.
    expect(onProgress).toHaveBeenCalledTimes(polls / 20);
  });

  it("counts a new action's first submission as progress, so a session that moves on to it is not stalled", async () => {
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout", "Date"] });
    const journal = recordJournal();
    // The first action confirms at once; the next is submitted at minute 20
    // and confirms at minute 45, 25 min later but 45 min after the first.
    const minute = 60;
    const polls = 46 * minute;
    const completed = { kind: "completed", workflowId: "two-actions" };
    const execute = vi.fn(async () => {
      const poll = execute.mock.calls.length;
      if (poll > polls) return completed;
      if (poll === 2) return journal.confirm("walk");
      if (poll === 20 * minute) return journal.startAction("seal");
      if (poll === 45 * minute) return journal.confirm("seal");
      return journal.pollPending();
    });
    const onProgress = vi.fn();
    const runner = await runnerFor(execute, onProgress);
    const running = settledFlag(runner.runOrResume("run"));
    await vi.advanceTimersByTimeAsync(polls * 1_000 + 1_000);
    await waitFor(() => running.settled);
    expect(running.value).toBe(completed);
    expect(execute).toHaveBeenCalledTimes(polls + 1);
    expect(onProgress).toHaveBeenCalledTimes(3);
  });

  it("keeps its bound when the progress callback throws", async () => {
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout", "Date"] });
    const journal = recordJournal();
    const execute = vi.fn(async () =>
      execute.mock.calls.length === 2
        ? journal.confirm("walk")
        : journal.pollPending(),
    );
    const onProgress = vi.fn(() => {
      throw new Error("alarm hook failed");
    });
    const runner = await runnerFor(execute, onProgress);
    const running = settledFlag(
      runner
        .runOrResume("run")
        .catch((error: unknown) => ({ rejected: String(error) })),
    );
    await vi.advanceTimersByTimeAsync(
      WORKFLOW_CONTINUATION_QUIET_BOUND_MS + 2_000,
    );
    await waitFor(() => running.settled);
    expect(running.value).toMatchObject({
      kind: "pending",
      resumeOnObservation: true,
      reason: expect.stringContaining(
        FRAUD_PROOF_WORKFLOW_CONTINUATION_STALLED,
      ),
    });
    expect(onProgress).toHaveBeenCalledOnce();
    expect(runner.close).toHaveBeenCalledOnce();
  });

  it("hands the caller's progress callback through the CLI to the installed runner", async () => {
    const actuation = await admittedActuation();
    const onProgress = vi.fn();
    const received: unknown[] = [];
    const runner = WORKFLOW_RUNNER_FACTORIES.doubleSpend(
      async ({ invocation }) => {
        received.push(invocation);
        throw new Error("runtime loader reached");
      },
    );
    await expect(
      runFraudProofWorkflowCli({
        mode: "run",
        category: "doubleSpend",
        deploymentFingerprint: DEPLOYMENT,
        headerHash: actuation.headerHash,
        decisionDigest: actuation.decisionDigest,
        actuationPermit: actuation.actuationPermit,
        fundingReservationPermit: actuation.fundingReservationPermit,
        journal: directoryJournal(await scratch()),
        runtimeConfigPath: "/etc/midgard/runtime.json",
        onProgress,
        applicationRegistry: installWorkflowApplicationRegistry({
          deploymentFingerprint: DEPLOYMENT,
          requiredInstalledCategories: ["doubleSpend"],
          installations: [
            {
              category: "doubleSpend",
              deploymentFingerprint: DEPLOYMENT,
              runner,
            },
          ],
        }),
      }),
    ).rejects.toThrow("runtime loader reached");
    expect(received).toEqual([expect.objectContaining({ onProgress })]);
  });

  it("lets a second objective queued behind a stalled one run to conviction, then resumes the stalled one on its next observation", async () => {
    const { order, outcomes, completed, run } = await queueBehindStall(false);
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
  });

  it("yields a stalled objective for one pick only, so a later fault still waits behind it", async () => {
    const { order } = await queueBehindStall(true);
    expect(order).toEqual(["first", "second", "first", "later"]);
  });
});
