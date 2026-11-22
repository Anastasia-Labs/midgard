import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { createWorkflowActuationPermitController } from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import type { WorkflowAdapterRunnerInput } from "@al-ft/midgard-fault-proofs/workflow/adapters";
import { verifyCompletedFraudProofWorkflow } from "@al-ft/midgard-fault-proofs/workflow/completed-verification";
import { WORKFLOW_RUNNER_FACTORIES } from "@al-ft/midgard-fault-proofs/workflow/runtime";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { createWatcherFaultProofExecution } from "../../src/fault-proofs/fault-proof-execution.js";
import type { WatcherFaultProofProgressRequest } from "../../src/fault-proofs/fault-proof-progress-authority.types.js";
import { createWatcherFaultProofSupervisor } from "../../src/fault-proofs/fault-proof-supervisor.create-watcher-fault-proof-supervisor.js";
import { watcherFaultProofDeadline } from "../../src/fault-proofs/fault-proof-supervisor.validate-job.js";
import {
  closeWatcherJournalDatabase,
  openWatcherJournalDatabase,
} from "../../src/fault-proofs/watcher-journal-database.js";
import {
  cleanupClassifiedFaultFixtures,
  deploymentIdentity,
  setupClassifiedFaultFixture,
} from "../support/classified-fault-fixture.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { waitForFaultProofSupervisorIdle } from "../support/fault-proof-supervisor-idle.js";
import { landedRemoval } from "../support/landed-removal.js";
import { storelessProofRetention } from "../support/proof-retention.js";
import {
  recordObjectives,
  TEST_JOURNAL_KEY,
} from "../support/watcher-journal-fixture.js";
import {
  lockWatcherJournals,
  watcherSupervisorReadyz,
} from "../support/watcher-journal-lock.js";

/**
 * W2b-R3: SQLITE_BUSY or SQLITE_LOCKED that reaches the supervisor holds it
 * unready (`journal_busy`) and never ends the process. After the backoff the
 * work is requeued in process from durable state, and ends exactly as a
 * restart from the same durable state does. Every busy here is real: a
 * second connection holds the journals' write lock past the busy timeout.
 */

const finishControl = vi.hoisted(() => ({
  beforeDecisionRead: async (): Promise<void> => undefined,
  beforeStart: async (): Promise<void> => undefined,
  beforeFinish: async (): Promise<void> => undefined,
}));
vi.mock("../../src/fault-proofs/fault-proof-queue-journal.js", async (load) => {
  const actual =
    await load<
      typeof import("../../src/fault-proofs/fault-proof-queue-journal.js")
    >();
  return {
    ...actual,
    openWatcherFaultProofQueueJournal: async (
      input: Parameters<typeof actual.openWatcherFaultProofQueueJournal>[0],
    ) => {
      const journal = await actual.openWatcherFaultProofQueueJournal(input);
      return {
        ...journal,
        markStarted: async (
          ...args: Parameters<typeof journal.markStarted>
        ) => {
          await finishControl.beforeStart();
          await journal.markStarted(...args);
        },
        markFinished: async (
          ...args: Parameters<typeof journal.markFinished>
        ) => {
          await finishControl.beforeFinish();
          await journal.markFinished(...args);
        },
      };
    },
  };
});
vi.mock("../../src/fault-proofs/fault-decision-journal.js", async (load) => {
  const actual =
    await load<
      typeof import("../../src/fault-proofs/fault-decision-journal.js")
    >();
  return {
    ...actual,
    openWatcherFaultDecisionJournal: async (
      input: Parameters<typeof actual.openWatcherFaultDecisionJournal>[0],
    ) => {
      const journal = await actual.openWatcherFaultDecisionJournal(input);
      return {
        ...journal,
        readAll: async () => {
          await finishControl.beforeDecisionRead();
          return await journal.readAll();
        },
      };
    },
  };
});
vi.mock("../../src/fault-proofs/fault-proof-application.js", async (load) => ({
  ...(await load<
    typeof import("../../src/fault-proofs/fault-proof-application.js")
  >()),
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES: Object.freeze(["doubleSpend"]),
}));

const closers: (() => Promise<void> | void)[] = [];
beforeEach(() => {
  finishControl.beforeDecisionRead = async () => undefined;
  finishControl.beforeStart = async () => undefined;
  finishControl.beforeFinish = async () => undefined;
});
afterEach(async () => {
  for (const close of closers.splice(0).reverse()) await close();
  await cleanupClassifiedFaultFixtures();
});

const setup = async () => {
  const fixture = await setupClassifiedFaultFixture({
    headerEndTime: BigInt(Date.now()),
  });
  // Open with neither a queue row nor a decision of its own: the progress
  // authority's startup forgets it, a write.
  recordObjectives(fixture.journalRoot, [
    { category: "doubleSpend", headerHash: "ab".repeat(28) },
  ]);
  // Every run lands our removal, which the chain shows beyond k.
  const landed = await landedRemoval(fixture);
  const verifyCompleted = vi.fn(verifyCompletedFraudProofWorkflow);
  let beforeRun = async (): Promise<void> => undefined;
  const runOrResume = vi.fn(async (invocation: WorkflowAdapterRunnerInput) => {
    await beforeRun();
    return landed.completed(invocation.decisionDigest);
  });
  const runner = WORKFLOW_RUNNER_FACTORIES.doubleSpend(async () => {
    throw new Error("the busy tests never build through the runner");
  });
  const setAlert = vi.fn();

  // One watcher process: its supervisor and readiness. The
  // requeue wakes the decision driver once the hold clears.
  const start = (wake: () => void = () => undefined) => {
    const supervisor = createWatcherFaultProofSupervisor({
      proofRetention: storelessProofRetention,
      journalRoot: fixture.journalRoot,
      hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
      deadlineAlertHeadroomMs:
        MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs,
      queueAuthenticationKey: TEST_JOURNAL_KEY,
      journalBusyRequeue: { wake },
      execution: createWatcherFaultProofExecution({
        application: {
          runners: { doubleSpend: runner },
          runOrResume,
          verifyCompleted: landed.verifyCompleted(
            async () => landed.raw.snapshot,
            verifyCompleted,
          ),
        },
        runtimeConfigPath: "/unused-journal-busy-test.json",
        hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
        operationsSink: { recordProofStep: vi.fn(), setAlert },
      }),
    });
    closers.push(() => supervisor.close());
    let failed: unknown;
    supervisor.done.catch((error: unknown) => {
      failed = error;
    });
    const readyz = watcherSupervisorReadyz(supervisor);
    return { supervisor, readyz, failed: () => failed };
  };

  // The live fault the decision driver dispatches on every pass.
  const controller = createWorkflowActuationPermitController({
    decision: fixture.old,
    rollbackGeneration: "1",
  });
  const observation = progressObservation({
    hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
    header: fixture.fixture,
    revision: 1,
  });
  const request: WatcherFaultProofProgressRequest = {
    observation,
    rollbackGeneration: "1",
    fault: {
      decision: fixture.old,
      actuationPermit: controller.permit,
      deadline: watcherFaultProofDeadline(observation.finalizedHeaders[0]!),
    },
  };

  const states = (table: "fault_proof_queue" | "fault_proof_objectives") =>
    openWatcherJournalDatabase({
      journalRoot: fixture.journalRoot,
      authenticationKey: TEST_JOURNAL_KEY,
    })
      .rows(table)
      .map(({ state }) => state)
      .sort();
  const queueStates = () => states("fault_proof_queue");
  // What a resume after the busy settles to: comparable across fixtures.
  const outcome = (supervisor: ReturnType<typeof start>["supervisor"]) => ({
    runs: runOrResume.mock.calls.length,
    verifications: verifyCompleted.mock.calls.length,
    queue: queueStates(),
    objectives: states("fault_proof_objectives"),
    unfinished: supervisor.status().unfinishedObjectiveCount,
    holds: supervisor.status().journalDecisionMissing.length,
    alerts: setAlert.mock.calls.filter(
      ([alert]) =>
        (alert as { code: string }).code === "proof_submission_failure",
    ).length,
  });

  return {
    fixture,
    start,
    request,
    queueStates,
    outcome,
    runOrResume,
    setBeforeRun: (callback: () => Promise<void>) => {
      beforeRun = callback;
    },
  };
};

// Where the busy meets the work, and the queue rows it leaves durable.
const HELD_QUEUE_STATE = {
  initialize: [],
  markStarted: ["queued"],
  midRun: ["active"],
  markFinished: ["active"],
} as const;
type Busy = keyof typeof HELD_QUEUE_STATE;

/** Arms one busy at `busy`; returns the lock's release once it was taken. */
const armBusy = (
  test: Awaited<ReturnType<typeof setup>>,
  busy: Busy,
): { release: () => void } => {
  const lock = { release: () => undefined as void };
  let armed = true;
  const take = () => {
    if (!armed) return false;
    armed = false;
    lock.release = lockWatcherJournals(test.fixture.journalRoot);
    closers.push(lock.release);
    return true;
  };
  if (busy === "initialize")
    finishControl.beforeDecisionRead = async () => {
      take();
    };
  else if (busy === "markStarted")
    finishControl.beforeStart = async () => {
      take();
    };
  else if (busy === "markFinished")
    finishControl.beforeFinish = async () => {
      take();
    };
  else
    test.setBeforeRun(async () => {
      // A journal write inside the run, while another connection writes.
      if (take())
        openWatcherJournalDatabase({
          journalRoot: test.fixture.journalRoot,
          authenticationKey: TEST_JOURNAL_KEY,
        }).transaction(() => undefined);
    });
  return lock;
};

/** The busy run's process holds; returns once it reports journal_busy. */
const untilHeld = async (
  watcher: ReturnType<Awaited<ReturnType<typeof setup>>["start"]>,
) =>
  vi.waitFor(
    () => expect(watcher.supervisor.status().journalBusy).toBeTruthy(),
    {
      timeout: 20_000,
    },
  );

const runs = (test: Awaited<ReturnType<typeof setup>>) =>
  test.runOrResume.mock.calls.length;

/** In process: the hold, the requeue after its backoff, the same request again. */
const resumeInProcess = async (busy: Busy) => {
  const test = await setup();
  const lock = armBusy(test, busy);
  const wakes = vi.fn();
  const watcher = test.start(() => {
    wakes();
    void watcher.supervisor.requestProgress(test.request);
  });
  await watcher.supervisor.requestProgress(test.request);
  await untilHeld(watcher);

  // Held: unready under journal_busy, the process up, nothing committed.
  const runsWhenHeld = runs(test);
  const readiness = await watcher.readyz();
  expect(readiness.status).toBe(503);
  expect(readiness.reasons).toContain("journal_busy");
  expect(watcher.supervisor.status().phase).toBe("accepting");
  expect(watcher.failed()).toBeUndefined();
  expect(test.queueStates()).toEqual(HELD_QUEUE_STATE[busy]);
  expect(wakes).not.toHaveBeenCalled();
  // The decision driver keeps dispatching while held; each pass is a no-op.
  await watcher.supervisor.requestProgress(test.request);
  expect(watcher.supervisor.status().journalBusy).toBeTruthy();
  expect(runs(test)).toBe(runsWhenHeld);
  expect(test.queueStates()).toEqual(HELD_QUEUE_STATE[busy]);

  // Once the lock is gone, the next requeue after the backoff succeeds;
  // then the decision driver is woken and dispatches again.
  lock.release();
  await vi.waitFor(() => expect(wakes).toHaveBeenCalledOnce(), {
    timeout: 40_000,
  });
  await vi.waitFor(() => expect(test.queueStates()).toEqual(["finished"]), {
    timeout: 20_000,
  });
  await waitForFaultProofSupervisorIdle(watcher.supervisor);
  expect(watcher.supervisor.status().journalBusy).toBeNull();
  expect((await watcher.readyz()).reasons).not.toContain("journal_busy");
  expect(watcher.failed()).toBeUndefined();
  return test.outcome(watcher.supervisor);
};

/** Restart: the busy process stops at the hold; a new one starts from the
 * same durable state, with nothing in flight, as the runtime does. */
const resumeAfterRestart = async (busy: Busy) => {
  const test = await setup();
  const lock = armBusy(test, busy);
  const first = test.start();
  await first.supervisor.requestProgress(test.request);
  await untilHeld(first);
  expect(test.queueStates()).toEqual(HELD_QUEUE_STATE[busy]);
  await first.supervisor.close();
  lock.release();
  closeWatcherJournalDatabase(test.fixture.journalRoot);
  const second = test.start();
  await second.supervisor.requestProgress(test.request);
  await vi.waitFor(() => expect(test.queueStates()).toEqual(["finished"]), {
    timeout: 20_000,
  });
  await waitForFaultProofSupervisorIdle(second.supervisor);
  expect(second.failed()).toBeUndefined();
  return test.outcome(second.supervisor);
};

describe("journal_busy holds the watcher and requeues its work in process", () => {
  for (const busy of Object.keys(HELD_QUEUE_STATE) as Busy[])
    it(`a busy database at ${busy} resumes as a restart does`, async () => {
      const inProcess = await resumeInProcess(busy);
      const restart = await resumeAfterRestart(busy);
      expect(inProcess).toEqual(restart);
      expect(inProcess.queue).toEqual(["finished"]);
      expect(inProcess.alerts).toBe(0);
      // A busy startup or start runs once; a busy run runs again; a busy
      // finish takes the marker its verification wrote as final and does
      // not verify the completed run again. Each reaches the durable finish
      // and the objective marker.
      expect(inProcess).toMatchObject(
        {
          initialize: { runs: 1, verifications: 1 },
          markStarted: { runs: 1, verifications: 1 },
          midRun: { runs: 2, verifications: 1 },
          markFinished: { runs: 1, verifications: 1 },
        }[busy],
      );
      expect(inProcess.objectives).toEqual(["marked"]);
    }, 120_000);
});
