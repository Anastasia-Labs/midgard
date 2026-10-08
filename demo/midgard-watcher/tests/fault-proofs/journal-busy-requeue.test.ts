import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import {
  bindWorkflowActuationJournal,
  bindWorkflowFundingReservationJournal,
  createWorkflowActuationPermitController,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  verifyCompletedFraudProofWorkflow,
  type WorkflowAdapterRunnerInput,
} from "@al-ft/midgard-fault-proofs";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { createWatcherFaultProofExecution } from "../../src/fault-proofs/fault-proof-execution.js";
import {
  createWatcherFaultProofSupervisor,
  watcherFaultProofDeadline,
  type WatcherFaultProofProgressRequest,
} from "../../src/fault-proofs/fault-proof-supervisor.js";
import {
  closeWatcherJournalDatabase,
  openWatcherJournalDatabase,
} from "../../src/fault-proofs/watcher-journal-database.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  setupFundingRecoveryFixture,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { waitForFaultProofSupervisorIdle } from "../support/fault-proof-supervisor-idle.js";
import { completedTerminalWriter } from "../support/funding-recovery-completed-terminal.js";
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
  await cleanupFundingRecoveryFixtures();
});

const deferred = () => {
  let resolve!: () => void;
  const promise = new Promise<void>((done) => {
    resolve = done;
  });
  closers.push(resolve);
  return { promise, resolve };
};

const setup = async () => {
  const fixture = await setupFundingRecoveryFixture(
    false,
    false,
    false,
    false,
    false,
    BigInt(Date.now()),
  );
  // Open with neither a queue row nor a workflow journal: the progress
  // authority's startup forgets it, a write.
  recordObjectives(fixture.journalRoot, [
    { category: "doubleSpend", headerHash: "ab".repeat(28) },
  ]);
  const { raw, writeTerminal } = await completedTerminalWriter(fixture);
  const verifyCompleted = vi.fn(verifyCompletedFraudProofWorkflow);
  let beforeRun = async (): Promise<void> => undefined;
  const runOrResume = vi.fn(async (invocation: WorkflowAdapterRunnerInput) => {
    await beforeRun();
    const bound = bindWorkflowFundingReservationJournal({
      journal: bindWorkflowActuationJournal({
        journal: new DirectoryFraudProofWorkflowJournalStore(
          fixture.journalDirectory,
        ),
        permit: invocation.actuationPermit,
        category: "doubleSpend",
        deploymentFingerprint: deploymentIdentity.manifestId,
        headerHash: fixture.old.headerHash,
        decisionDigest: invocation.decisionDigest,
      }),
      permit: invocation.fundingReservationPermit,
    });
    await fixture.run(bound);
    await writeTerminal();
    return { kind: "completed" };
  });
  const setAlert = vi.fn();

  // One watcher process: its funding factory, supervisor and readiness. As
  // in the runtime, the requeue sweeps the same factory the runs fund from.
  const start = (
    wake: () => void = () => undefined,
    beforeSweep: () => Promise<void> = async () => undefined,
  ) => {
    const fundingFactory = fixture.fundingFactory();
    const supervisor = createWatcherFaultProofSupervisor({
      reservationDecisionHolds: fundingFactory.decisionHolds,
      proofRetention: storelessProofRetention,
      journalRoot: fixture.journalRoot,
      deploymentFingerprint: deploymentIdentity.manifestId,
      deadlineAlertHeadroomMs:
        MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs,
      queueAuthenticationKey: TEST_JOURNAL_KEY,
      journalBusyRequeue: {
        releaseUnusedFunding: async () => {
          await beforeSweep();
          await fundingFactory.releaseUnused();
        },
        wake,
      },
      execution: createWatcherFaultProofExecution({
        application: {
          runners: { doubleSpend: fixture.runner },
          runOrResume,
          verifyCompleted: async ({ entries, terminal, decisionDigest }) =>
            verifyCompleted({
              binding: raw.binding,
              entries,
              terminal,
              decisionDigest,
              authority: {
                authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
                capture: async () => raw.snapshot,
              },
            }),
        },
        fundingFactory,
        walletAddress,
        provider: {
          getUtxos: async () => fixture.walletUtxos,
          getUtxosByOutRef: async () => [],
        },
        journalRoot: fixture.journalRoot,
        runtimeConfigPath: "/unused-journal-busy-test.json",
        deploymentFingerprint: deploymentIdentity.manifestId,
        operationsSink: () => ({ recordProofStep: vi.fn(), setAlert }),
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
    deploymentFingerprint: deploymentIdentity.manifestId,
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
  const outcome = async (
    supervisor: ReturnType<typeof start>["supervisor"],
  ) => ({
    runs: runOrResume.mock.calls.map(([invocation]) => invocation.mode),
    verifications: verifyCompleted.mock.calls.length,
    queue: queueStates(),
    objectives: states("fault_proof_objectives"),
    reservations: (await fixture.records()).map(({ state }) => state).sort(),
    submits: vi.mocked(fixture.adapter.submit).mock.calls.length,
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
  const held = deferred();
  const assertionsDone = deferred();
  const wakes = vi.fn();
  const watcher = test.start(
    () => {
      wakes();
      void watcher.supervisor.requestProgress(test.request);
    },
    async () => {
      held.resolve();
      await assertionsDone.promise;
    },
  );
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

  // The backoff elapses into the requeue, which waits here until the lock
  // is gone; then the decision driver is woken and dispatches again.
  await held.promise;
  expect((await watcher.readyz()).reasons).toContain("journal_busy");
  lock.release();
  assertionsDone.resolve();
  await vi.waitFor(() => expect(wakes).toHaveBeenCalledOnce(), {
    timeout: 10_000,
  });
  await vi.waitFor(() => expect(test.queueStates()).toEqual(["finished"]), {
    timeout: 20_000,
  });
  await waitForFaultProofSupervisorIdle(watcher.supervisor);
  expect(watcher.supervisor.status().journalBusy).toBeNull();
  expect((await watcher.readyz()).reasons).not.toContain("journal_busy");
  expect(watcher.failed()).toBeUndefined();
  return await test.outcome(watcher.supervisor);
};

/** Restart: the busy process stops at the hold; a new one starts from the
 * same durable state, as the runtime does (funding sweep, then dispatch). */
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
  await test.fixture.restartStore();
  await test.fixture.fundingFactory().releaseUnused();
  const second = test.start();
  await second.supervisor.requestProgress(test.request);
  await vi.waitFor(() => expect(test.queueStates()).toEqual(["finished"]), {
    timeout: 20_000,
  });
  await waitForFaultProofSupervisorIdle(second.supervisor);
  expect(second.failed()).toBeUndefined();
  return await test.outcome(second.supervisor);
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
      // finish verifies the completed run again. Each reaches the durable
      // finish and the objective marker.
      expect(inProcess).toMatchObject(
        {
          initialize: { runs: ["resume"], verifications: 1 },
          markStarted: { runs: ["resume"], verifications: 1 },
          midRun: { runs: ["resume", "resume"], verifications: 1 },
          markFinished: { runs: ["resume"], verifications: 2 },
        }[busy],
      );
      expect(inProcess.objectives).toEqual(["marked"]);
    }, 120_000);
});
