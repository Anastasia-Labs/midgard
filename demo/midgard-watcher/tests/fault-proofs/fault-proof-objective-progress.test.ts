import { existsSync } from "node:fs";
import { rename, rm } from "node:fs/promises";
import { join } from "node:path";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import {
  admitFraudProofRawL1Snapshot,
  assertWorkflowActuationPermitIdentity,
  bindWorkflowActuationJournal,
  bindWorkflowFundingReservationJournal,
  createWorkflowActuationPermitController,
  deriveFraudProofRawL1CompletedTerminal,
  deriveFraudProofRawL1FamilyStage,
  DirectoryFraudProofWorkflowJournalStore,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
  FraudProofL1UnavailableError,
  fraudProofRawL1SnapshotRequestForFamily,
  type FraudProofWorkflowJournalEvent,
  journalJsonDigest,
  verifyCompletedFraudProofWorkflow,
  type WorkflowAdapterRunnerInput,
} from "@al-ft/midgard-fault-proofs";
import {
  fixture as terminalFixture,
  rollBackTerminalFixture,
} from "@al-ft/midgard-fault-proofs/test-support/raw-l1-terminal-fixture";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { openWatcherFaultDecisionJournal } from "../../src/fault-proofs/fault-decision-journal.js";
import { createWatcherFaultProofExecution } from "../../src/fault-proofs/fault-proof-execution.js";
import { listWatcherProofObjectives } from "../../src/fault-proofs/fault-proof-objective-table.js";
import {
  createWatcherFaultProofSupervisor,
  watcherFaultProofDeadline,
} from "../../src/fault-proofs/fault-proof-supervisor.js";
import { openWatcherJournalDatabase } from "../../src/fault-proofs/watcher-journal-database.js";
import { unsafeAdmitWatcherStateQueueObservationForReplayTest } from "../../src/indexers/authenticated-state-queue-observation.js";
import { WatcherFaultProofL1RefusedError } from "../../src/l1-follower/fault-proof-l1-source.chain.js";
import type {
  WatcherProofRetention,
  WatcherProofRetentionTarget,
} from "../../src/l1-follower/proof-retention.js";
import { watcherDeploymentReleaseEconomicsAuthority } from "../../src/runtime/deployment-identity.js";
import { handleWatcherOperationsHttpRequest } from "../../src/runtime/operations-observability.handle-http-request.js";
import { createWatcherOperationsObservability } from "../../src/runtime/operations-observability.js";
import { watcherSha256CanonicalJson } from "../../src/storage/durable-store.js";
import {
  cleanupFundingRecoveryFixtures,
  deploymentIdentity,
  finality,
  key,
  setupFundingRecoveryFixture,
  walletAddress,
} from "../support/fault-proof-funding-fixture.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { waitForFaultProofSupervisorIdle } from "../support/fault-proof-supervisor-idle.js";
import { storelessProofRetention } from "../support/proof-retention.js";
import { TEST_JOURNAL_KEY } from "../support/watcher-journal-fixture.js";
const finishControl = vi.hoisted(() => ({
  beforeFinish: async (): Promise<void> => undefined,
  beforeRegister: async (): Promise<void> => undefined,
  beforeDecisionRead: async (): Promise<void> => undefined,
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
        register: async (...args: Parameters<typeof journal.register>) => {
          await finishControl.beforeRegister();
          return await journal.register(...args);
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
const releases: (() => void)[] = [];
const deferred = () => {
  let resolve!: () => void;
  const promise = new Promise<void>((done) => {
    resolve = done;
  });
  releases.push(resolve);
  return { promise, resolve };
};
const supervisors: ReturnType<typeof createWatcherFaultProofSupervisor>[] = [];
beforeEach(() => {
  finishControl.beforeFinish = async () => undefined;
  finishControl.beforeRegister = async () => undefined;
  finishControl.beforeDecisionRead = async () => undefined;
});
afterEach(async () => {
  for (const release of releases.splice(0)) release();
  await Promise.all(
    supervisors.splice(0).map((supervisor) => supervisor.close()),
  );
  await cleanupFundingRecoveryFixtures();
});

const setup = async (
  headerEndTime = BigInt(Date.now()),
  terminalBeyondRecoveryHorizon = true,
  withoutSubmissionIntent = false,
) => {
  const fixture = await setupFundingRecoveryFixture(
    false,
    false,
    withoutSubmissionIntent,
    false,
    false,
    headerEndTime,
  );
  const getUtxos = vi.fn(async () => fixture.walletUtxos);
  const getUtxosByOutRef = vi.fn(async () => []);
  const journal = new DirectoryFraudProofWorkflowJournalStore(
    fixture.journalDirectory,
  );
  const append = async (event: FraudProofWorkflowJournalEvent) => {
    const entries = await journal.load(fixture.initial.workflowId);
    await journal.append(
      {
        schemaVersion: FRAUD_PROOF_WORKFLOW_JOURNAL_SCHEMA_VERSION,
        workflowId: fixture.initial.workflowId,
        identity: fixture.initial.identity,
        sequence: entries.length,
        recordedAt: new Date().toISOString(),
        event,
      },
      entries.length,
    );
  };
  const verifiedFinality = await finality.verifyForWorkflow({
    deploymentFingerprint: deploymentIdentity.manifestId,
  });
  const verifiedEconomics = await watcherDeploymentReleaseEconomicsAuthority(
    deploymentIdentity,
  ).verifyForWorkflow({ deploymentFingerprint: deploymentIdentity.manifestId });
  const raw = await terminalFixture({
    proofCreation: true,
    header: fixture.fixture.header,
    deploymentFingerprint: deploymentIdentity.manifestId,
    blueprintHash: verifiedFinality.blueprintHash,
    verifiedFinality,
    verifiedEconomics,
    proverCredential: key.to_public().hash().to_hex(),
    // A workflow completes only beyond the recovery horizon (k + 2); a
    // provisional terminal_included keeps the fixture's shallower depth.
    confirmationDepth: terminalBeyondRecoveryHorizon
      ? verifiedFinality.policy.automaticRecoveryMaxDepth + 2
      : undefined,
  });
  const terminal = await deriveFraudProofRawL1CompletedTerminal({
    snapshot: raw.snapshot,
    definition: raw.definition,
    releaseEconomics: verifiedEconomics,
  });
  if (terminal === null)
    throw new Error("raw fixture omitted completed correction");
  let capturedSnapshot = raw.snapshot;
  let beforeCapture = async (): Promise<void> => undefined;
  const completionVerified = deferred();
  const secondRunStarted = deferred();
  const verifyCompleted = vi.fn(
    async (input: Parameters<typeof verifyCompletedFraudProofWorkflow>[0]) => {
      const result = await verifyCompletedFraudProofWorkflow(input);
      completionVerified.resolve();
      return result;
    },
  );
  let beforeRun = async (_input: WorkflowAdapterRunnerInput): Promise<void> =>
    undefined;
  let afterRun = async (result: unknown): Promise<unknown> => result;
  const runOrResume = vi.fn(async (invocation: WorkflowAdapterRunnerInput) => {
    if (runOrResume.mock.calls.length === 2) secondRunStarted.resolve();
    await beforeRun(invocation);
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
    return afterRun(await fixture.run(bound));
  });
  const createExecution = () =>
    createWatcherFaultProofExecution({
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
              capture: async () => {
                await beforeCapture();
                return capturedSnapshot;
              },
            },
          }),
      },
      fundingFactory: fixture.fundingFactory(),
      walletAddress,
      provider: { getUtxos, getUtxosByOutRef },
      journalRoot: fixture.journalRoot,
      runtimeConfigPath: "/unused-objective-test.json",
      deploymentFingerprint: deploymentIdentity.manifestId,
      operationsSink: () => ({ recordProofStep: vi.fn(), setAlert: vi.fn() }),
    });
  let execution: ReturnType<typeof createExecution> | undefined;
  const createSupervisor = (proofRetention = storelessProofRetention) => {
    execution = createExecution();
    const supervisor = createWatcherFaultProofSupervisor({
      reservationDecisionHolds: () => [],
      proofRetention,
      journalRoot: fixture.journalRoot,
      deploymentFingerprint: deploymentIdentity.manifestId,
      deadlineAlertHeadroomMs:
        MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs,
      queueAuthenticationKey: new Uint8Array(32).fill(0xa5),
      execution,
    });
    supervisors.push(supervisor);
    return supervisor;
  };
  const request = (
    supervisor: ReturnType<typeof createSupervisor>,
    generation: number,
    decision = fixture.fresh,
    targetPresent = true,
  ) => {
    const controller = createWorkflowActuationPermitController({
      decision,
      rollbackGeneration: generation.toString(),
    });
    const observation = progressObservation({
      deploymentFingerprint: deploymentIdentity.manifestId,
      header: targetPresent ? fixture.fixture : undefined,
      revision: generation,
    });
    return {
      controller,
      accepted: supervisor.requestProgress({
        observation,
        rollbackGeneration: generation.toString(),
        ...(targetPresent
          ? {
              fault: {
                decision,
                actuationPermit: controller.permit,
                deadline: watcherFaultProofDeadline(
                  observation.finalizedHeaders[0]!,
                ),
              },
            }
          : {}),
      }),
    };
  };
  const writeTerminal = async (provisional = false) => {
    for (const txHash of [
      terminal.proofToken.createdByTxHash,
      terminal.correction.removalTxHash,
    ]) {
      await append({
        kind: "preflight_passed",
        actionId: txHash,
        txHash,
        localEvaluator: "fixture-local-uplc",
        referenceScripts: [],
      });
      await append({
        kind: "submission_intent",
        actionId: txHash,
        txHash,
        attempt: 1,
        actionInput: {},
      });
      await append({ kind: "submitted", actionId: txHash, txHash, attempt: 1 });
      await append({
        kind: "reconciled",
        actionId: txHash,
        txHash,
        outcome: "confirmed",
      });
      await append({ kind: "confirmed", actionId: txHash, txHash });
    }
    await append({
      kind: provisional ? "terminal_included" : "completed",
      terminal,
      terminalDigest: journalJsonDigest(terminal),
    });
  };
  return {
    fixture,
    journal,
    append,
    terminal,
    raw,
    createSupervisor,
    request,
    idle: waitForFaultProofSupervisorIdle,
    readiness: () => execution?.readiness() ?? [],
    writeTerminal,
    runOrResume,
    verifyCompleted,
    completionVerified: completionVerified.promise,
    secondRunStarted: secondRunStarted.promise,
    getUtxos,
    getUtxosByOutRef,
    setBeforeRun: (callback: typeof beforeRun) => {
      beforeRun = callback;
    },
    setAfterRun: (callback: typeof afterRun) => {
      afterRun = callback;
    },
    setBeforeCapture: (callback: typeof beforeCapture) => {
      beforeCapture = callback;
    },
    setSnapshot: (snapshot: typeof capturedSnapshot) => {
      capturedSnapshot = snapshot;
    },
  };
};

/** The reasons `/readyz` names for this supervisor alone. */
const readyzReasons = async (
  supervisor: ReturnType<typeof createWatcherFaultProofSupervisor>,
): Promise<readonly string[]> => {
  const operations = createWatcherOperationsObservability({
    deploymentFingerprint: deploymentIdentity.manifestId,
    supervisor,
    launchScopeStatus: () => ({
      installedCategoryCount: 1,
      requiredCategoryCount: 1,
    }),
    retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
    durableProofQueueStatus: () => supervisor.durableQueueStatus(),
    nowMs: () => 100_000n,
  });
  const response = await handleWatcherOperationsHttpRequest(
    new Request("http://127.0.0.1/readyz"),
    operations.api,
  );
  return ((await response.json()) as { reasons: readonly string[] }).reasons;
};

/** A retention that records which objectives' L1 history it holds. */
const recordingRetention = () => {
  const pinned = new Set<string>();
  const name = (target: WatcherProofRetentionTarget) =>
    `${target.category}/${target.headerHash}`;
  const retention: WatcherProofRetention = {
    ...storelessProofRetention,
    pin: async (target) => (pinned.add(name(target)), { kind: "pinned" }),
    release: async (target) => void pinned.delete(name(target)),
  };
  return { pinned, retention };
};

/** The recorded objective rows, as `<category>/<headerHash>`. */
const objectiveRows = (journalRoot: string): readonly string[] =>
  listWatcherProofObjectives(
    openWatcherJournalDatabase({
      journalRoot,
      authenticationKey: TEST_JOURNAL_KEY,
    }),
    ["doubleSpend"],
  ).map(({ objective }) => `${objective.category}/${objective.headerHash}`);

/** The states of a journal's rows. */
const journalStates = (
  journalRoot: string,
  journal: "fault_proof_queue" | "fault_decisions",
): readonly string[] =>
  openWatcherJournalDatabase({
    journalRoot,
    authenticationKey: TEST_JOURNAL_KEY,
  })
    .rows(journal)
    .map(({ state }) => state);

describe("proof objective progress with durable funding and journals", () => {
  it("holds an objective past its latest safe start with no signed attempt by name, again after a restart, until its header leaves the queue", async () => {
    const test = await setup(20n, true, true);
    const supervisor = test.createSupervisor();
    // Whether the running supervisor's liveness ended.
    let settled = false;
    let watched: Promise<void> | undefined;
    const watch = (done: Promise<void>) => {
      watched = done;
      settled = false;
      const end = () => {
        if (watched === done) settled = true;
      };
      void done.then(end, end);
    };
    watch(supervisor.done);
    const hold = {
      kind: "objective",
      category: "doubleSpend",
      headerHash: test.fixture.fresh.headerHash,
      decisionDigest: test.fixture.fresh.decisionDigest,
      detail: `doubleSpend/${test.fixture.fresh.headerHash}`,
      readiness: "fault_proof_start_deadline_passed",
    } as const;
    const expectHeld = async (
      held: ReturnType<typeof test.createSupervisor>,
    ) => {
      expect(held.status()).toMatchObject({
        phase: "accepting",
        blockedJob: null,
        unfinishedObjectiveCount: 1,
        journalDecisionMissing: [hold],
      });
      expect(await readyzReasons(held)).toContain(
        "fault_proof_start_deadline_passed",
      );
      expect(settled).toBe(false);
    };
    await test.request(supervisor, 2).accepted;
    await test.idle(supervisor);
    await expectHeld(supervisor);
    // Held work takes no new run while its header stays queued.
    await test.request(supervisor, 3).accepted;
    await test.idle(supervisor);
    await expectHeld(supervisor);
    await supervisor.close();
    // A restart over the same stores meets it again and holds it again.
    await test.fixture.restartStore();
    const restarted = test.createSupervisor();
    watch(restarted.done);
    await test.request(restarted, 4).accepted;
    await test.idle(restarted);
    await expectHeld(restarted);
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(test.getUtxos).not.toHaveBeenCalled();
    // The header left the finalized queue: the hold clears.
    await test.request(restarted, 5, test.fixture.fresh, false).accepted;
    await test.idle(restarted);
    expect(restarted.status()).toMatchObject({
      phase: "accepting",
      unfinishedObjectiveCount: 0,
      journalDecisionMissing: [],
    });
    expect(await readyzReasons(restarted)).not.toContain(
      "fault_proof_start_deadline_passed",
    );
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(settled).toBe(false);
  });

  it("runs an objective with no signed attempt inside its safe-start window", async () => {
    const test = await setup(BigInt(Date.now()), true, true);
    const supervisor = test.createSupervisor();
    await test.request(supervisor, 2).accepted;
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledOnce();
    expect(supervisor.status()).toMatchObject({
      phase: "accepting",
      blockedJob: null,
      journalDecisionMissing: [],
    });
  });

  it("drops a refusal hold once the deadline holds the objective under its own name", async () => {
    vi.useFakeTimers({ toFake: ["Date"] });
    try {
      const startedAt = Date.now();
      const latestSafeStartOffsetMs =
        MIDGARD_RETENTION_WINDOW.maturityMs -
        MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs;
      // Its latest safe start is a minute away.
      const test = await setup(
        BigInt(startedAt - latestSafeStartOffsetMs + 60_000),
        true,
        true,
      );
      let refuse = true;
      test.setBeforeRun(async () => {
        if (!refuse) return;
        refuse = false;
        throw new WatcherFaultProofL1RefusedError(
          "untracked_address",
          "addr_test1 is not tracked",
        );
      });
      const supervisor = test.createSupervisor();
      await test.request(supervisor, 1).accepted;
      await test.idle(supervisor);
      expect(test.readiness().map(({ reason }) => reason)).toEqual([
        "fault_proof_l1_refused:untracked_address",
      ]);
      vi.setSystemTime(startedAt + 120_000);
      await test.request(supervisor, 2).accepted;
      await test.idle(supervisor);
      // Never run again, so no run would clear the refusal: the hold does.
      expect(supervisor.status().journalDecisionMissing).toMatchObject([
        { readiness: "fault_proof_start_deadline_passed" },
      ]);
      expect(test.readiness()).toEqual([]);
      expect(test.runOrResume).toHaveBeenCalledOnce();
    } finally {
      vi.useRealTimers();
    }
  });

  it("drops a refusal hold once its header leaves the queue with no signed attempt to reconcile", async () => {
    const test = await setup(BigInt(Date.now()), true, true);
    let refuse = true;
    test.setBeforeRun(async () => {
      if (!refuse) return;
      refuse = false;
      throw new WatcherFaultProofL1RefusedError(
        "untracked_address",
        "addr_test1 is not tracked",
      );
    });
    const supervisor = test.createSupervisor();
    await test.request(supervisor, 1).accepted;
    await test.idle(supervisor);
    expect(test.readiness().map(({ reason }) => reason)).toEqual([
      "fault_proof_l1_refused:untracked_address",
    ]);
    await test.request(supervisor, 2, test.fixture.fresh, false).accepted;
    await test.idle(supervisor);
    expect(test.readiness()).toEqual([]);
    expect(test.runOrResume).toHaveBeenCalledOnce();
    expect(supervisor.status().phase).toBe("accepting");
  });

  it("releases an unheld objective with no signed attempt once its header leaves the queue, with its workflow directory, and admits it afresh if the header returns", async () => {
    const test = await setup(BigInt(Date.now()), true, true);
    const { pinned, retention } = recordingRetention();
    const objective = `doubleSpend/${test.fixture.fresh.headerHash}`;
    const directory = test.fixture.journalDirectory;
    const expectOpen = (
      supervisor: ReturnType<typeof test.createSupervisor>,
    ) => {
      expect(supervisor.status()).toMatchObject({
        phase: "accepting",
        unfinishedObjectiveCount: 1,
        journalDecisionMissing: [],
      });
      expect([...pinned]).toEqual([objective]);
      expect(objectiveRows(test.fixture.journalRoot)).toEqual([objective]);
      expect(existsSync(directory)).toBe(true);
    };
    const expectReleased = (
      supervisor: ReturnType<typeof test.createSupervisor>,
    ) => {
      expect(supervisor.status()).toMatchObject({
        phase: "accepting",
        unfinishedObjectiveCount: 0,
        journalDecisionMissing: [],
        objectiveCleanupFailures: [],
      });
      expect([...pinned]).toEqual([]);
      expect(objectiveRows(test.fixture.journalRoot)).toEqual([]);
      expect(existsSync(directory)).toBe(false);
    };
    const lastMode = () => test.runOrResume.mock.calls.at(-1)![0].mode;
    const supervisor = test.createSupervisor(retention);
    // It runs inside its window, signs nothing and is not held.
    await test.request(supervisor, 2).accepted;
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledOnce();
    expect(lastMode()).toBe("resume");
    expectOpen(supervisor);
    // A new observation that still queues its header keeps it.
    await test.request(supervisor, 3).accepted;
    await test.idle(supervisor);
    expectOpen(supervisor);
    // Its header left the finalized queue: nothing drives it again, so its
    // row, its workflow directory, its L1 history pin and its unfinished
    // count go.
    await test.request(supervisor, 4, test.fixture.fresh, false).accepted;
    await test.idle(supervisor);
    expectReleased(supervisor);
    // A rollback that brings the header back admits it as a fresh objective
    // in the same process: a new execution, not the departed one.
    let runs = test.runOrResume.mock.calls.length;
    await test.request(supervisor, 5).accepted;
    await test.idle(supervisor);
    expectOpen(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(runs + 1);
    expect(lastMode()).toBe("run");
    await test.request(supervisor, 6, test.fixture.fresh, false).accepted;
    await test.idle(supervisor);
    expectReleased(supervisor);
    runs = test.runOrResume.mock.calls.length;
    await supervisor.close();
    // A restart over the same stores does not adopt it again.
    await test.fixture.restartStore();
    const restarted = test.createSupervisor(retention);
    await test.request(restarted, 7, test.fixture.fresh, false).accepted;
    await test.idle(restarted);
    expectReleased(restarted);
    expect(test.runOrResume).toHaveBeenCalledTimes(runs);
    // Nor does a returning header after the restart resume a departed one.
    await test.request(restarted, 8).accepted;
    await test.idle(restarted);
    expectOpen(restarted);
    expect(test.runOrResume).toHaveBeenCalledTimes(runs + 1);
    expect(lastMode()).toBe("run");
  });

  // A crash during a job leaves its queue row active; nothing in a later
  // process finishes it.
  const crashMidJob = async (test: Awaited<ReturnType<typeof setup>>) => {
    const { pinned, retention } = recordingRetention();
    test.setBeforeRun(async () => {
      throw new Error("the process died mid-job");
    });
    const crashed = test.createSupervisor(retention);
    await test.request(crashed, 2).accepted;
    await expect(crashed.done).rejects.toThrow("the process died mid-job");
    await crashed.close();
    expect(
      journalStates(test.fixture.journalRoot, "fault_proof_queue"),
    ).toEqual(["active"]);
    test.setBeforeRun(async () => undefined);
    await test.fixture.restartStore();
    return { pinned, restarted: test.createSupervisor(retention) };
  };

  it("settles the job a crash left active once its header leaves the queue with nothing signed, and releases the objective", async () => {
    const test = await setup(BigInt(Date.now()), true, true);
    const { pinned, restarted } = await crashMidJob(test);
    await test.request(restarted, 3, test.fixture.fresh, false).accepted;
    await test.idle(restarted);
    expect(restarted.status()).toMatchObject({
      phase: "accepting",
      unfinishedObjectiveCount: 0,
      objectiveCleanupFailures: [],
    });
    expect([...pinned]).toEqual([]);
    expect(objectiveRows(test.fixture.journalRoot)).toEqual([]);
    expect(
      journalStates(test.fixture.journalRoot, "fault_proof_queue"),
    ).toEqual([]);
    expect(existsSync(test.fixture.journalDirectory)).toBe(false);
    expect(test.runOrResume).toHaveBeenCalledOnce();
  });

  it("forgets a job a crash left active before its execution was written once its header leaves the queue", async () => {
    const test = await setup(BigInt(Date.now()), true, true);
    const { pinned, restarted } = await crashMidJob(test);
    await rm(test.fixture.journalDirectory, { recursive: true });
    // While its header is queued the row stays: a live fault requeues it.
    await restarted.requestProgress({
      observation: progressObservation({
        deploymentFingerprint: deploymentIdentity.manifestId,
        header: test.fixture.fixture,
        revision: 3,
      }),
      rollbackGeneration: "3",
    });
    await test.idle(restarted);
    expect(objectiveRows(test.fixture.journalRoot)).toEqual([
      `doubleSpend/${test.fixture.fresh.headerHash}`,
    ]);
    expect(
      journalStates(test.fixture.journalRoot, "fault_proof_queue"),
    ).toEqual(["active"]);
    await test.request(restarted, 4, test.fixture.fresh, false).accepted;
    await test.idle(restarted);
    expect(restarted.status()).toMatchObject({
      phase: "accepting",
      unfinishedObjectiveCount: 0,
      objectiveCleanupFailures: [],
    });
    expect([...pinned]).toEqual([]);
    expect(objectiveRows(test.fixture.journalRoot)).toEqual([]);
    expect(
      journalStates(test.fixture.journalRoot, "fault_proof_queue"),
    ).toEqual([]);
  });

  it("keeps reconciling a job a crash left active with a signed attempt after its header leaves the queue", async () => {
    const test = await setup(BigInt(Date.now()));
    const { pinned, restarted } = await crashMidJob(test);
    test.setBeforeRun(async (invocation) => {
      expect(
        assertWorkflowActuationPermitIdentity({
          permit: invocation.actuationPermit,
          category: "doubleSpend",
          rollbackGeneration: "3",
        }).authority,
      ).toBe("reconciliation");
    });
    await test.request(restarted, 3, test.fixture.fresh, false).accepted;
    await test.idle(restarted);
    expect(test.runOrResume).toHaveBeenCalledTimes(2);
    expect(restarted.status()).toMatchObject({
      phase: "accepting",
      unfinishedObjectiveCount: 1,
    });
    const objective = `doubleSpend/${test.fixture.fresh.headerHash}`;
    expect([...pinned]).toEqual([objective]);
    expect(objectiveRows(test.fixture.journalRoot)).toEqual([objective]);
    expect(
      journalStates(test.fixture.journalRoot, "fault_proof_queue"),
    ).toEqual(["finished"]);
    expect(existsSync(test.fixture.journalDirectory)).toBe(true);
  });

  it.each([
    ["marked beyond k is pruned in process", 2_160, true],
    ["not yet k deep keeps its rows and directory", 1_000_000_000, false],
  ])(
    "a completion %s once its job has finished",
    async (_name, securityParameter, pruned) => {
      const test = await setup();
      test.setAfterRun(async () => {
        await test.writeTerminal();
        return { kind: "completed" };
      });
      const supervisor = test.createSupervisor({
        ...storelessProofRetention,
        securityParameter,
      });
      await test.request(supervisor, 1, test.fixture.old).accepted;
      await test.idle(supervisor);
      // Kept while an observation still queues its header.
      await supervisor.requestProgress({
        observation: progressObservation({
          deploymentFingerprint: deploymentIdentity.manifestId,
          header: test.fixture.fixture,
          revision: 2,
        }),
        rollbackGeneration: "2",
      });
      await test.idle(supervisor);
      expect(existsSync(test.fixture.journalDirectory)).toBe(true);
      // The next pass that does not prunes a final completion, as the next
      // start would.
      await test.request(supervisor, 3, test.fixture.fresh, false).accepted;
      await test.idle(supervisor);
      expect(supervisor.status()).toMatchObject({
        phase: "accepting",
        unfinishedObjectiveCount: 0,
        objectiveCleanupFailures: [],
      });
      const root = test.fixture.journalRoot;
      expect(existsSync(test.fixture.journalDirectory)).toBe(!pruned);
      expect(objectiveRows(root)).toEqual(
        pruned ? [] : [`doubleSpend/${test.fixture.fresh.headerHash}`],
      );
      expect(journalStates(root, "fault_proof_queue")).toEqual(
        pruned ? [] : ["finished"],
      );
      expect(journalStates(root, "fault_decisions")).toHaveLength(
        pruned ? 0 : 2,
      );
      expect(test.runOrResume).toHaveBeenCalledOnce();
    },
  );

  it("leaves a final completion's directory to the job handed over after its finish", async () => {
    const test = await setup();
    const entered = deferred(),
      release = deferred();
    test.setBeforeRun(async () => {
      entered.resolve();
      await release.promise;
    });
    test.setAfterRun(async () => {
      await test.writeTerminal();
      return { kind: "completed" };
    });
    const supervisor = test.createSupervisor();
    // A pass runs after the first job's durable finish, while the update that
    // arrived during its run is being registered.
    let registrations = 0;
    finishControl.beforeRegister = async () => {
      if (++registrations !== 2) return;
      await test.request(supervisor, 3, test.fixture.fresh, false).accepted;
    };
    await test.request(supervisor, 1, test.fixture.old).accepted;
    await Promise.race([entered.promise, supervisor.done]);
    await test.request(supervisor, 2).accepted;
    release.resolve();
    await Promise.race([test.completionVerified, supervisor.done]);
    await test.idle(supervisor);
    expect(registrations).toBe(2);
    expect(supervisor.status().phase).toBe("accepting");
    // The handed-over job met the completion, not a fresh objective.
    expect(test.runOrResume).toHaveBeenCalledOnce();
    expect(test.getUtxos).not.toHaveBeenCalled();
    await test.request(supervisor, 4, test.fixture.fresh, false).accepted;
    await test.idle(supervisor);
    expect(existsSync(test.fixture.journalDirectory)).toBe(false);
    expect(objectiveRows(test.fixture.journalRoot)).toEqual([]);
  });

  it("keeps reconciling an objective with a signed attempt after its header leaves the queue", async () => {
    const test = await setup(BigInt(Date.now()));
    const { pinned, retention } = recordingRetention();
    const objective = `doubleSpend/${test.fixture.fresh.headerHash}`;
    const supervisor = test.createSupervisor(retention);
    await test.request(supervisor, 2).accepted;
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledOnce();
    test.setBeforeRun(async (invocation) => {
      expect(
        assertWorkflowActuationPermitIdentity({
          permit: invocation.actuationPermit,
          category: "doubleSpend",
          rollbackGeneration: "3",
        }).authority,
      ).toBe("reconciliation");
    });
    await test.request(supervisor, 3, test.fixture.fresh, false).accepted;
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(2);
    expect(supervisor.status()).toMatchObject({
      phase: "accepting",
      unfinishedObjectiveCount: 1,
      journalDecisionMissing: [],
    });
    expect([...pinned]).toEqual([objective]);
    expect(objectiveRows(test.fixture.journalRoot)).toEqual([objective]);
    expect(existsSync(test.fixture.journalDirectory)).toBe(true);
  });

  it.each([
    ["the run", "directly"],
    ["the run", "wrapped"],
    ["the completion check", "directly"],
  ] as const)(
    "a refused L1 read in %s (%s) holds the objective by name instead of failing the process",
    async (where, how) => {
      const test = await setup();
      const refusal = new WatcherFaultProofL1RefusedError(
        "untracked_address",
        "addr_test1 is not tracked",
      );
      let refuse = true;
      const refuseOnce = async () => {
        if (!refuse) return;
        refuse = false;
        throw how === "wrapped"
          ? new Error("the workflow could not observe", { cause: refusal })
          : refusal;
      };
      if (where === "the run") test.setBeforeRun(refuseOnce);
      else test.setBeforeCapture(refuseOnce);
      test.setAfterRun(async () => {
        await test.writeTerminal();
        return { kind: "completed" };
      });
      const supervisor = test.createSupervisor();
      let settled = false;
      supervisor.done.then(
        () => (settled = true),
        () => (settled = true),
      );
      await test.request(supervisor, 1).accepted;
      await test.idle(supervisor);
      // Held and named: the supervisor keeps running, its liveness does not
      // end, and /readyz names the refusal.
      expect(supervisor.status().phase).toBe("accepting");
      expect(supervisor.status().unfinishedObjectiveCount).toBe(1);
      expect(settled).toBe(false);
      expect(test.readiness()).toEqual([
        {
          reason: "fault_proof_l1_refused:untracked_address",
          detail: `doubleSpend/${test.fixture.fresh.headerHash}: addr_test1 is not tracked`,
        },
      ]);
      // The next observation runs it again, and a read the source answers
      // ends the hold.
      await test.request(supervisor, 2).accepted;
      await test.idle(supervisor);
      expect(supervisor.status().unfinishedObjectiveCount).toBe(0);
      expect(test.readiness()).toEqual([]);
      expect(test.runOrResume).toHaveBeenCalledTimes(
        where === "the run" ? 2 : 1,
      );
      expect(settled).toBe(false);
    },
  );

  it("coalesces two generations and authenticates completion before another funding admission", async () => {
    const test = await setup();
    const entered = deferred(),
      release = deferred();
    test.setBeforeRun(async () => {
      entered.resolve();
      await release.promise;
    });
    test.setAfterRun(async () => {
      await test.writeTerminal();
      return { kind: "completed" };
    });
    const supervisor = test.createSupervisor();
    await test.request(supervisor, 1, test.fixture.old).accepted;
    await Promise.race([entered.promise, supervisor.done]);
    await test.request(supervisor, 2).accepted;
    release.resolve();
    await Promise.race([test.completionVerified, supervisor.done]);
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(1);
    expect([1, 2]).toContain(test.verifyCompleted.mock.calls.length);
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toHaveLength(1);
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
  });

  it.each([
    ["marked beyond k is final", 2_160, 1],
    ["not yet k deep is verified again", 1_000_000_000, 2],
  ])(
    "a completion %s for a job queued at a later generation",
    async (_name, securityParameter, verifications) => {
      const test = await setup();
      const entered = deferred(),
        release = deferred();
      test.setBeforeRun(async () => {
        entered.resolve();
        await release.promise;
      });
      test.setAfterRun(async () => {
        await test.writeTerminal();
        return { kind: "completed" };
      });
      // Past the completion the follower prunes the released history.
      const holds: string[] = [];
      let captures = 0;
      test.setBeforeCapture(async () => {
        if (++captures > 1 && verifications === 1)
          throw new WatcherFaultProofL1RefusedError(
            "beyond_retention",
            "pruned",
          );
      });
      const supervisor = test.createSupervisor({
        ...storelessProofRetention,
        securityParameter,
        pin: async () => (holds.push("pin"), { kind: "pinned" }),
        release: async () => void holds.push("release"),
      });
      await test.request(supervisor, 1, test.fixture.old).accepted;
      await Promise.race([entered.promise, supervisor.done]);
      await test.request(supervisor, 2).accepted;
      release.resolve();
      await Promise.race([test.completionVerified, supervisor.done]);
      await test.idle(supervisor);
      expect(supervisor.status().phase).not.toBe("blocked");
      expect(supervisor.status().unfinishedObjectiveCount).toBe(0);
      expect(test.verifyCompleted).toHaveBeenCalledTimes(verifications);
      expect(test.runOrResume).toHaveBeenCalledTimes(1);
      if (verifications === 1) expect(holds.at(-1)).toBe("release");
      else expect(holds).not.toContain("release");
    },
  );

  it("retains completed authority for decisions published by another handle after supervisor startup", async () => {
    const test = await setup();
    // Stage the real signed fixture outside discovery, then publish its
    // evidence only after the supervisor has opened an empty decision reader.
    const proofs = join(test.fixture.journalRoot, "fault-proofs");
    const stagedProofs = join(test.fixture.journalRoot, "staged-proof-fixture");
    await rename(proofs, stagedProofs);
    openWatcherJournalDatabase({
      journalRoot: test.fixture.journalRoot,
      authenticationKey: TEST_JOURNAL_KEY,
    }).transaction((tx) => {
      for (const row of tx.rows("fault_decisions"))
        tx.delete("fault_decisions", row.key);
    });
    const initializedReader = vi.fn(async () => undefined);
    finishControl.beforeDecisionRead = initializedReader;
    const supervisor = test.createSupervisor();
    await supervisor.requestProgress({
      observation: progressObservation({
        deploymentFingerprint: deploymentIdentity.manifestId,
      }),
      rollbackGeneration: "0",
    });
    await test.idle(supervisor);
    expect(initializedReader).toHaveBeenCalledOnce();
    const writer = await openWatcherFaultDecisionJournal({
      directory: test.fixture.journalRoot,
      deploymentFingerprint: deploymentIdentity.manifestId,
      launchScope: test.fixture.old.launchScope,
      authenticationKey: TEST_JOURNAL_KEY,
    });
    await writer.appendLiveDecision(test.fixture.old);
    await writer.appendLiveDecision(test.fixture.fresh);
    await rename(stagedProofs, proofs);
    const entered = deferred(),
      release = deferred();
    test.setBeforeRun(async () => {
      entered.resolve();
      await release.promise;
    });
    test.setAfterRun(async () => {
      await test.writeTerminal();
      return { kind: "completed" };
    });
    await test.request(supervisor, 1, test.fixture.old).accepted;
    await Promise.race([entered.promise, supervisor.done]);
    await test.request(supervisor, 2).accepted;
    release.resolve();
    await Promise.race([test.completionVerified, supervisor.done]);
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(1);
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.fixture.adapter.preflight).not.toHaveBeenCalled();
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toHaveLength(1);
  });

  it("hands unfinished work to pending valid authority after the active permit is revoked", async () => {
    const test = await setup();
    const entered = deferred(),
      release = deferred();
    test.setBeforeRun(async () => {
      if (test.runOrResume.mock.calls.length === 1) {
        entered.resolve();
        await release.promise;
      }
    });
    const supervisor = test.createSupervisor();
    const first = test.request(supervisor, 1, test.fixture.old);
    await first.accepted;
    await Promise.race([entered.promise, supervisor.done]);
    first.controller.revoke("native_chain_rollback");
    await test.request(supervisor, 2).accepted;
    release.resolve();
    await Promise.race([test.secondRunStarted, supervisor.done]);
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(2);
    expect(test.runOrResume.mock.calls[1]![0].decisionDigest).toBe(
      test.fixture.fresh.decisionDigest,
    );
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toHaveLength(1);
  });

  it("retains a new observation arriving while the finished queue record is being persisted", async () => {
    const test = await setup();
    const finishing = deferred(),
      release = deferred();
    let held = false;
    finishControl.beforeFinish = async () => {
      if (!held) {
        held = true;
        finishing.resolve();
        await release.promise;
      }
    };
    const supervisor = test.createSupervisor();
    await test.request(supervisor, 1, test.fixture.old).accepted;
    await Promise.race([finishing.promise, supervisor.done]);
    await test.request(supervisor, 2).accepted;
    release.resolve();
    await Promise.race([test.secondRunStarted, supervisor.done]);
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(2);
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
  });

  it("drains a public request accepted before close while admission is still reading its journal", async () => {
    const test = await setup();
    const admitting = deferred(),
      release = deferred();
    finishControl.beforeDecisionRead = async () => {
      admitting.resolve();
      await release.promise;
    };
    const supervisor = test.createSupervisor();
    const request = test.request(supervisor, 2);
    await Promise.race([admitting.promise, supervisor.done]);
    const closing = supervisor.close();
    expect(supervisor.status().phase).toBe("closing");
    release.resolve();
    await request.accepted;
    await closing;
    expect(supervisor.status().phase).toBe("closed");
    expect(test.runOrResume).toHaveBeenCalledOnce();
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
    expect(test.getUtxos).not.toHaveBeenCalled();
  });

  it("reconciles a retained signed attempt after the new-start deadline with read-only authority", async () => {
    const test = await setup(20n);
    test.setBeforeRun(async (invocation) => {
      const authority = assertWorkflowActuationPermitIdentity({
        permit: invocation.actuationPermit,
        category: "doubleSpend",
        rollbackGeneration: "2",
      });
      expect(authority.authority).toBe("reconciliation");
    });
    const supervisor = test.createSupervisor();
    await test.request(supervisor, 2).accepted;
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledOnce();
    expect(test.fixture.adapter.reconcile).toHaveBeenCalledWith(
      expect.objectContaining({ txHash: test.fixture.transactionHash }),
    );
    expect(test.fixture.adapter.preflight).not.toHaveBeenCalled();
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toHaveLength(1);
    expect(supervisor.status().journalDecisionMissing).toEqual([]);
  });

  it("does not spin or repeat funding admission for identical pending observations", async () => {
    const test = await setup();
    const supervisor = test.createSupervisor();
    await test.request(supervisor, 2).accepted;
    await test.idle(supervisor);
    for (let repeat = 0; repeat < 10; repeat += 1)
      await test.request(supervisor, 2).accepted;
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(1);
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
  });

  it("reconciles once when the queue observation advances to a quiet native block", async () => {
    const test = await setup();
    vi.mocked(test.fixture.adapter.reconcile).mockResolvedValueOnce({
      kind: "pending",
      txHash: test.fixture.transactionHash,
    });
    test.setBeforeRun(async (invocation) => {
      expect(
        assertWorkflowActuationPermitIdentity({
          permit: invocation.actuationPermit,
          category: "doubleSpend",
          rollbackGeneration: "2",
        }).authority,
      ).toBe("reconciliation");
    });
    const supervisor = test.createSupervisor();
    const observation = progressObservation({
      deploymentFingerprint: deploymentIdentity.manifestId,
    });
    const request = { observation, rollbackGeneration: "2" };
    await supervisor.requestProgress(request);
    await test.idle(supervisor);
    expect(test.fixture.adapter.reconcile).toHaveBeenCalledTimes(1);
    const { observationDigest: _digest, ...prior } = observation;
    const advanced = {
      ...prior,
      nativePoint: {
        ...prior.nativePoint,
        blockNo: (BigInt(prior.nativePoint.blockNo) + 1n).toString(),
        slot: (BigInt(prior.nativePoint.slot) + 20n).toString(),
        blockHash:
          "27807a70215e3e018eec9be8c619c692e06a78ebcb63daf90d7abe823f3bbf47",
      },
    };
    const quietRequest = {
      ...request,
      observation: unsafeAdmitWatcherStateQueueObservationForReplayTest({
        ...advanced,
        observationDigest: watcherSha256CanonicalJson(advanced),
      }),
    };
    await supervisor.requestProgress(quietRequest);
    await test.idle(supervisor);
    expect(test.fixture.adapter.reconcile).toHaveBeenCalledTimes(2);
    const entries = await test.journal.load(test.fixture.initial.workflowId);
    for (let repeat = 0; repeat < 5; repeat += 1)
      await supervisor.requestProgress(quietRequest);
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(2);
    expect(test.fixture.adapter.reconcile).toHaveBeenCalledTimes(2);
    expect(await test.journal.load(test.fixture.initial.workflowId)).toEqual(
      entries,
    );
    expect(await test.fixture.records()).toHaveLength(1);
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(test.fixture.adapter.preflight).not.toHaveBeenCalled();
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
  });

  it("reconstructs a submitted attempt after restart without submitting or allocating again", async () => {
    const test = await setup();
    const signed = test.fixture.signedTransactionCborHex;
    const supervisor = test.createSupervisor();
    await test.request(supervisor, 1, test.fixture.old).accepted;
    await test.idle(supervisor);
    await supervisor.close();
    await test.fixture.restartStore();
    const restarted = test.createSupervisor();
    await test.request(restarted, 2).accepted;
    await test.idle(restarted);
    expect(test.fixture.adapter.reconcile).toHaveBeenCalledTimes(1);
    expect(test.fixture.adapter.reconcile).toHaveBeenCalledWith(
      expect.objectContaining({ txHash: test.fixture.transactionHash }),
    );
    expect(test.fixture.signedTransactionCborHex).toBe(signed);
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toHaveLength(1);
  });

  it.each([false, true])(
    "authenticates removed completed execution after restart and pending canonical verification (%s)",
    async (pending) => {
      const test = await setup();
      await test.fixture.run(await test.fixture.recover());
      await test.writeTerminal();
      const before = await test.fixture.records();
      const supervisor = test.createSupervisor();
      if (pending)
        test.verifyCompleted.mockResolvedValueOnce({
          kind: "pending",
          reason: "checkpoint_changed",
        });
      await test.request(supervisor, 2, test.fixture.fresh, false).accepted;
      await test.idle(supervisor);
      if (pending) {
        expect(supervisor.status().unfinishedObjectiveCount).toBe(1);
        await test.request(supervisor, 3, test.fixture.fresh, false).accepted;
        await test.idle(supervisor);
      }
      expect(test.verifyCompleted).toHaveBeenCalledTimes(pending ? 2 : 1);
      expect(supervisor.status().unfinishedObjectiveCount).toBe(0);
      expect(test.runOrResume).not.toHaveBeenCalled();
      expect(test.getUtxos).not.toHaveBeenCalled();
      expect(test.getUtxosByOutRef).not.toHaveBeenCalled();
      expect(await test.fixture.records()).toEqual(before);
    },
  );

  it("retries a completed objective's unavailable raw source without funding or another execution", async () => {
    const test = await setup();
    await test.fixture.run(await test.fixture.recover());
    await test.writeTerminal();
    const records = await test.fixture.records();
    let captures = 0;
    test.setBeforeCapture(async () => {
      if (++captures === 1)
        throw new FraudProofL1UnavailableError("canonical provider HTTP 503");
    });
    const supervisor = test.createSupervisor();
    await test.request(supervisor, 2).accepted;
    await vi.waitFor(
      async () => {
        if (supervisor.status().phase === "blocked") await supervisor.done;
        expect(test.verifyCompleted).toHaveBeenCalledTimes(2);
      },
      { timeout: 3_000 },
    );
    await test.idle(supervisor);
    expect(test.runOrResume).not.toHaveBeenCalled();
    expect(test.getUtxos).not.toHaveBeenCalled();
    expect(await test.fixture.records()).toEqual(records);
  });

  it("resumes a provisionally completed objective under fresh rollback authority", async () => {
    const test = await setup(undefined, false);
    await test.fixture.run(await test.fixture.recover());
    await test.writeTerminal(true);
    const rollback = rollBackTerminalFixture(test.raw);
    const observed = vi.fn(async () => {
      const snapshot = admitFraudProofRawL1Snapshot({
        value: rollback,
        request: fraudProofRawL1SnapshotRequestForFamily({
          definition: test.raw.definition,
          releaseFinality: test.raw.binding.releaseFinality,
        }),
        releaseFinality: test.raw.binding.releaseFinality,
        observationDepth: "inclusion",
      });
      const stage = await deriveFraudProofRawL1FamilyStage({
        snapshot,
        definition: test.raw.definition,
        releaseEconomics: test.raw.binding.releaseEconomics,
      });
      expect(stage.kind).toBe("proof_token");
      return {
        kind: "pending" as const,
        reason: "authenticated rollback restored correction target",
      };
    });
    vi.mocked(test.fixture.adapter.observe).mockImplementation(observed);
    const supervisor = test.createSupervisor();
    supervisor.revokeAuthority("native_chain_rollback");
    await test.request(supervisor, 3).accepted;
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(1);
    expect(test.runOrResume.mock.calls[0]![0].mode).toBe("resume");
    expect(observed).toHaveBeenCalledOnce();
    expect(
      (await test.journal.load(test.fixture.initial.workflowId)).at(-1)?.event
        .kind,
    ).not.toBe("completed");
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
    expect(test.getUtxos).not.toHaveBeenCalled();
  });
});
