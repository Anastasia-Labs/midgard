import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import {
  assertWorkflowActuationPermitIdentity,
  createWorkflowActuationPermitController,
  deriveFraudProofRawL1CompletedTerminal,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  FraudProofL1UnavailableError,
  verifyCompletedFraudProofWorkflow,
  type WorkflowAdapterRunnerInput,
} from "@al-ft/midgard-fault-proofs";
import {
  fixture as terminalFixture,
  rollBackTerminalFixture,
} from "@al-ft/midgard-fault-proofs/test-support/raw-l1-terminal-fixture";
import {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
} from "@al-ft/midgard-fault-proofs/workflow/identity";
import { WORKFLOW_RUNNER_FACTORIES } from "@al-ft/midgard-fault-proofs/workflow/runtime";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { openWatcherFaultDecisionJournal } from "../../src/fault-proofs/fault-decision-journal.js";
import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.production-dependencies.js";
import { createWatcherFaultProofExecution } from "../../src/fault-proofs/fault-proof-execution.js";
import { listWatcherProofObjectives } from "../../src/fault-proofs/fault-proof-objective-table.js";
import { createWatcherFaultProofSupervisor } from "../../src/fault-proofs/fault-proof-supervisor.create-watcher-fault-proof-supervisor.js";
import type { WatcherProofMaterialsSweep } from "../../src/fault-proofs/fault-proof-supervisor.proof-materials-sweep.js";
import { watcherFaultProofDeadline } from "../../src/fault-proofs/fault-proof-supervisor.validate-job.js";
import { openWatcherJournalDatabase } from "../../src/fault-proofs/watcher-journal-database.js";
import { WatcherJournalUnavailableError } from "../../src/fault-proofs/watcher-journal-database.types.js";
import { WatcherFaultProofL1RefusedError } from "../../src/l1-follower/fault-proof-l1-source.chain.js";
import { watcherDeploymentReleaseEconomicsAuthority } from "../../src/runtime/deployment-identity.watcher-deployment-availability-challenge-authority.js";
import type { WatcherOperationsSink } from "../../src/runtime/operations-observability.js";
import {
  cleanupClassifiedFaultFixtures,
  deploymentIdentity,
  finality,
  key,
  setupClassifiedFaultFixture,
} from "../support/classified-fault-fixture.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { readyzReasons } from "../support/fault-proof-readyz-reasons.js";
import { waitForFaultProofSupervisorIdle } from "../support/fault-proof-supervisor-idle.js";
import {
  recordingProofRetention as recordingRetention,
  storelessProofRetention,
} from "../support/proof-retention.js";
import { TEST_JOURNAL_KEY } from "../support/watcher-journal-fixture.js";
import { registerLateStartTests } from "./fault-proof-objective-progress.late-start.js";
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
  await cleanupClassifiedFaultFixtures();
});

/** The pending result of a run whose transaction awaits inclusion. */
const AWAITING_INCLUSION = Object.freeze({
  kind: "pending",
  resumeOnObservation: true,
  reason: "signed transaction awaiting inclusion",
});

/**
 * One classified doubleSpend fault under a fresh journal root, run by a
 * mocked runner, with its completion checked by the real verification
 * against a raw L1 view the test sets: by default the target is still live;
 * `landRemoval` shows our removal. Each `createSupervisor` is one watcher
 * process, as after a restart.
 */
export const setup = async (
  headerEndTime = BigInt(Date.now()),
  operationsSink: Pick<
    WatcherOperationsSink,
    "recordProofStep" | "setAlert"
  > = { recordProofStep: vi.fn(), setAlert: vi.fn() },
) => {
  const fixture = await setupClassifiedFaultFixture({ headerEndTime });
  const verifiedFinality = await finality.verifyForWorkflow({
    hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
  });
  const verifiedEconomics = await watcherDeploymentReleaseEconomicsAuthority(
    deploymentIdentity,
  ).verifyForWorkflow({
    hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
  });
  const raw = await terminalFixture({
    proofCreation: true,
    header: fixture.fixture.header,
    hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
    blueprintHash: verifiedFinality.blueprintHash,
    verifiedFinality,
    verifiedEconomics,
    proverCredential: key.to_public().hash().to_hex(),
    // A workflow completes only beyond the recovery horizon (k + 2).
    confirmationDepth: verifiedFinality.policy.automaticRecoveryMaxDepth + 2,
  });
  const derived = await deriveFraudProofRawL1CompletedTerminal({
    snapshot: raw.snapshot,
    definition: raw.definition,
    releaseEconomics: verifiedEconomics,
  });
  if (derived.kind !== "removed")
    throw new Error("raw fixture omitted completed correction");
  const terminal = derived.terminal;
  // The view the chain shows: the target live until the removal lands.
  let capturedSnapshot = rollBackTerminalFixture(raw);
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
  const workflowId = computeFraudProofWorkflowId({
    schemaVersion: FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
    hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
    category: "doubleSpend",
    target: { kind: "state_queue_header", headerHash: fixture.old.headerHash },
    decisionDigest: fixture.fresh.decisionDigest,
  });
  /** A run's report that the removal landed. */
  const completed = () =>
    Object.freeze({ kind: "completed", workflowId, terminal });
  let beforeRun = async (_input: WorkflowAdapterRunnerInput): Promise<void> =>
    undefined;
  let afterRun = async (result: unknown): Promise<unknown> => result;
  const runOrResume = vi.fn(async (invocation: WorkflowAdapterRunnerInput) => {
    if (runOrResume.mock.calls.length === 2) secondRunStarted.resolve();
    await beforeRun(invocation);
    return afterRun(AWAITING_INCLUSION);
  });
  const runner = WORKFLOW_RUNNER_FACTORIES.doubleSpend(async () => {
    throw new Error("the objective tests never build through the runner");
  });
  const sweepProofMaterials = vi.fn<WatcherProofMaterialsSweep>(async () => 0);
  let execution:
    | ReturnType<typeof createWatcherFaultProofExecution>
    | undefined;
  const createSupervisor = (
    proofRetention = storelessProofRetention,
    more: Partial<Parameters<typeof createWatcherFaultProofSupervisor>[0]> = {},
  ) => {
    execution = createWatcherFaultProofExecution({
      application: {
        runners: { doubleSpend: runner },
        runOrResume,
        verifyCompleted: async ({ terminal, decisionDigest }) =>
          verifyCompleted({
            binding: raw.binding,
            ...(terminal === undefined ? {} : { terminal }),
            decisionDigest,
            authority: {
              authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
              capture: async () => {
                await beforeCapture();
                return { snapshot: capturedSnapshot, pending: [] };
              },
            },
          }),
      },
      runtimeConfigPath: "/unused-objective-test.json",
      hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
      operationsSink,
    });
    const supervisor = createWatcherFaultProofSupervisor({
      proofRetention,
      journalRoot: fixture.journalRoot,
      hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
      deadlineAlertHeadroomMs:
        MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs,
      queueAuthenticationKey: new Uint8Array(32).fill(0xa5),
      execution,
      sweepProofMaterials,
      ...more,
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
      hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
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
  return {
    fixture,
    terminal,
    raw,
    completed,
    createSupervisor,
    request,
    idle: waitForFaultProofSupervisorIdle,
    readiness: () => execution?.readiness() ?? [],
    /** Our removal lands: the chain now shows the completed correction. */
    landRemoval: () => {
      capturedSnapshot = raw.snapshot;
    },
    /** Our removal is rolled back: the target is live again. */
    rollBackRemoval: () => {
      capturedSnapshot = rollBackTerminalFixture(raw);
    },
    /** Our removal lands `confirmationDepth` deep: short of final below
     * k + 2. */
    landRemovalAt: async (confirmationDepth: number) => {
      capturedSnapshot = (
        await terminalFixture({
          proofCreation: true,
          header: fixture.fixture.header,
          hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
          blueprintHash: verifiedFinality.blueprintHash,
          verifiedFinality,
          verifiedEconomics,
          proverCredential: key.to_public().hash().to_hex(),
          confirmationDepth,
        })
      ).snapshot;
    },
    runOrResume,
    verifyCompleted,
    sweepProofMaterials,
    completionVerified: completionVerified.promise,
    secondRunStarted: secondRunStarted.promise,
    setBeforeRun: (callback: typeof beforeRun) => {
      beforeRun = callback;
    },
    setAfterRun: (callback: typeof afterRun) => {
      afterRun = callback;
    },
    setBeforeCapture: (callback: typeof beforeCapture) => {
      beforeCapture = callback;
    },
  };
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

describe("proof objective progress with journals", () => {
  it("holds an objective past its maturity with no removal on chain by name, again after a restart, until its header leaves the queue", async () => {
    const test = await setup(20n);
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
    // The chain was asked whether a removal landed: none did.
    expect(test.verifyCompleted).toHaveBeenCalledOnce();
    // Held work takes no new run while its header stays queued.
    await test.request(supervisor, 3).accepted;
    await test.idle(supervisor);
    await expectHeld(supervisor);
    await supervisor.close();
    // A restart over the same stores meets it again and holds it again.
    const restarted = test.createSupervisor();
    watch(restarted.done);
    await test.request(restarted, 4).accepted;
    await test.idle(restarted);
    await expectHeld(restarted);
    expect(test.runOrResume).not.toHaveBeenCalled();
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

  it("completes an objective whose removal landed just before a restart, past maturity, without running it again", async () => {
    vi.useFakeTimers({ toFake: ["Date"] });
    try {
      const startedAt = Date.now();
      const test = await setup(BigInt(startedAt));
      const first = test.createSupervisor();
      await test.request(first, 2).accepted;
      await test.idle(first);
      expect(test.runOrResume).toHaveBeenCalledOnce();
      // Its removal lands, the process stops, and it restarts a minute past
      // maturity.
      test.landRemoval();
      await first.close();
      vi.setSystemTime(
        startedAt + MIDGARD_RETENTION_WINDOW.maturityMs + 60_000,
      );
      const restarted = test.createSupervisor();
      await test.request(restarted, 3).accepted;
      await test.idle(restarted);
      // The chain shows the removal: completed, not held.
      expect(restarted.status()).toMatchObject({
        phase: "accepting",
        blockedJob: null,
        unfinishedObjectiveCount: 0,
        journalDecisionMissing: [],
      });
      expect(await readyzReasons(restarted)).not.toContain(
        "fault_proof_start_deadline_passed",
      );
      expect(test.verifyCompleted).toHaveBeenCalledOnce();
      expect(test.verifyCompleted.mock.calls[0]![0].terminal).toBeUndefined();
      // Marked final: a later observation neither runs nor verifies it.
      await test.request(restarted, 4).accepted;
      await test.idle(restarted);
      expect(test.runOrResume).toHaveBeenCalledOnce();
      expect(test.verifyCompleted).toHaveBeenCalledOnce();
      // Its header leaves the queue: its rows go.
      await test.request(restarted, 5, test.fixture.fresh, false).accepted;
      await test.idle(restarted);
      expect(restarted.status()).toMatchObject({
        unfinishedObjectiveCount: 0,
        objectiveCleanupFailures: [],
      });
      expect(objectiveRows(test.fixture.journalRoot)).toEqual([]);
    } finally {
      vi.useRealTimers();
    }
  });

  it("keeps a departed objective whose removal landed across a restart before final, and completes it from the chain once final with no run of its family, marked against that removal", async () => {
    const recordProofStep = vi.fn();
    const test = await setup(BigInt(Date.now()), {
      recordProofStep,
      setAlert: vi.fn(),
    });
    const { pinned, retention } = recordingRetention();
    const objective = `doubleSpend/${test.fixture.fresh.headerHash}`;
    const root = test.fixture.journalRoot;
    const steps = () =>
      recordProofStep.mock.calls.map(([step]) => {
        const { stage, status } = step as { stage: string; status: string };
        return `${stage}:${status}`;
      });
    const first = test.createSupervisor(retention);
    await test.request(first, 2).accepted;
    await test.idle(first);
    expect(test.runOrResume).toHaveBeenCalledOnce();
    // Our removal lands one block deep, and the process stops before it is
    // final.
    await test.landRemovalAt(1);
    await first.close();
    const ran = steps().length;
    const restarted = test.createSupervisor(retention);
    // Its header has left the queue: the chain shows our removal landed, so
    // it is kept and its proof step is confirmed.
    await test.request(restarted, 3, test.fixture.fresh, false).accepted;
    await test.idle(restarted);
    expect(test.verifyCompleted).toHaveBeenCalledOnce();
    expect(restarted.status()).toMatchObject({
      phase: "accepting",
      unfinishedObjectiveCount: 1,
      journalDecisionMissing: [],
    });
    expect([...pinned]).toEqual([objective]);
    expect(objectiveRows(root)).toEqual([objective]);
    // Still short of final: asked again.
    await test.request(restarted, 4, test.fixture.fresh, false).accepted;
    await test.idle(restarted);
    expect(test.verifyCompleted).toHaveBeenCalledTimes(2);
    // Final: completed from the chain alone, marked against our removal and
    // released. Its one sweep-only run, under reconciliation authority, fails
    // unfunded and leaves it completed.
    test.sweepProofMaterials.mockRejectedValueOnce(new Error("unfunded"));
    test.landRemoval();
    await test.request(restarted, 5, test.fixture.fresh, false).accepted;
    await test.idle(restarted);
    expect(test.verifyCompleted).toHaveBeenCalledTimes(3);
    await vi.waitFor(() => expect(test.sweepProofMaterials).toHaveBeenCalled());
    const [sweep] = test.sweepProofMaterials.mock.calls[0]!;
    expect(sweep.terminal).toStrictEqual(test.terminal);
    expect(
      assertWorkflowActuationPermitIdentity({
        permit: sweep.actuationPermit,
        category: "doubleSpend",
        rollbackGeneration: "5",
      }).authority,
    ).toBe("reconciliation");
    expect(restarted.status()).toMatchObject({
      phase: "accepting",
      unfinishedObjectiveCount: 0,
      journalDecisionMissing: [],
    });
    const rows = listWatcherProofObjectives(
      openWatcherJournalDatabase({
        journalRoot: root,
        authenticationKey: TEST_JOURNAL_KEY,
      }),
      ["doubleSpend"],
    );
    expect(rows.map(({ state }) => state)).toEqual(["marked"]);
    expect(rows[0]!.marker).toMatchObject({
      terminalTxHash: test.terminal.correction.removalTxHash,
    });
    expect([...pinned]).toEqual([]);
    // No run or prepare of its family since the restart: only the chain's
    // steps.
    expect(test.runOrResume).toHaveBeenCalledOnce();
    expect(steps().slice(ran)).toEqual([
      "terminal:confirmed",
      "terminal:completed",
    ]);
    // Marked final: a later observation neither runs nor verifies it, and
    // its rows go.
    await test.request(restarted, 6, test.fixture.fresh, false).accepted;
    await test.idle(restarted);
    expect(test.runOrResume).toHaveBeenCalledOnce();
    expect(test.verifyCompleted).toHaveBeenCalledTimes(3);
    expect(test.sweepProofMaterials).toHaveBeenCalledOnce();
    expect(objectiveRows(root)).toEqual([]);
  });

  it("admits a departed objective again when its landed removal is rolled back and its header returns, and releases it cancelled when the header stays gone under another removal", async () => {
    const recordProofStep = vi.fn();
    const test = await setup(BigInt(Date.now()), {
      recordProofStep,
      setAlert: vi.fn(),
    });
    const { pinned, retention } = recordingRetention();
    const objective = `doubleSpend/${test.fixture.fresh.headerHash}`;
    const statuses = () =>
      recordProofStep.mock.calls.map(
        ([step]) => (step as { status: string }).status,
      );
    const supervisor = test.createSupervisor(retention);
    await test.request(supervisor, 2).accepted;
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledOnce();
    // Our removal lands short of final and the header leaves the queue.
    await test.landRemovalAt(1);
    await test.request(supervisor, 3, test.fixture.fresh, false).accepted;
    await test.idle(supervisor);
    expect(supervisor.status()).toMatchObject({ unfinishedObjectiveCount: 1 });
    expect(statuses().at(-1)).toBe("confirmed");
    // Rolled back before final, the header returns: the live fault admits it
    // again and runs it under submission authority.
    test.rollBackRemoval();
    await test.request(supervisor, 4).accepted;
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(2);
    expect(
      assertWorkflowActuationPermitIdentity({
        permit: test.runOrResume.mock.calls[1]![0].actuationPermit,
        category: "doubleSpend",
        rollbackGeneration: "4",
      }).authority,
    ).toBe("submission");
    expect(supervisor.status()).toMatchObject({ unfinishedObjectiveCount: 1 });
    // It lands again and the header leaves: confirmed again.
    await test.landRemovalAt(1);
    await test.request(supervisor, 5, test.fixture.fresh, false).accepted;
    await test.idle(supervisor);
    expect(supervisor.status()).toMatchObject({ unfinishedObjectiveCount: 1 });
    expect(statuses().at(-1)).toBe("confirmed");
    // Rolled back before final, the header stays gone: another record's
    // removal stands, so it is released cancelled, with no run.
    test.verifyCompleted.mockResolvedValueOnce({
      kind: "pending",
      reason: "terminal_not_ours",
    });
    await test.request(supervisor, 6, test.fixture.fresh, false).accepted;
    await test.idle(supervisor);
    expect(supervisor.status()).toMatchObject({
      phase: "accepting",
      unfinishedObjectiveCount: 0,
      objectiveCleanupFailures: [],
    });
    expect([...pinned]).not.toContain(objective);
    expect(objectiveRows(test.fixture.journalRoot)).toEqual([]);
    expect(test.runOrResume).toHaveBeenCalledTimes(2);
    expect(statuses().at(-1)).toBe("cancelled");
    expect(statuses()).not.toContain("completed");
    expect(test.sweepProofMaterials).not.toHaveBeenCalled();
  });

  it("releases a departed objective at once when the chain shows no removal of ours", async () => {
    const test = await setup();
    const { pinned, retention } = recordingRetention();
    const supervisor = test.createSupervisor(retention);
    await test.request(supervisor, 2).accepted;
    await test.idle(supervisor);
    await test.request(supervisor, 3, test.fixture.fresh, false).accepted;
    await test.idle(supervisor);
    expect(test.verifyCompleted).toHaveBeenCalledOnce();
    expect(test.runOrResume).toHaveBeenCalledOnce();
    expect(supervisor.status()).toMatchObject({ unfinishedObjectiveCount: 0 });
    expect([...pinned]).toEqual([]);
    expect(objectiveRows(test.fixture.journalRoot)).toEqual([]);
  });

  it("takes one sweep-only run, and no run of its family, when a departure completes in process", async () => {
    const test = await setup();
    const supervisor = test.createSupervisor();
    await test.request(supervisor, 2).accepted;
    await test.idle(supervisor);
    test.landRemoval();
    await test.request(supervisor, 3, test.fixture.fresh, false).accepted;
    await supervisor.close();
    expect(
      [test.runOrResume, test.sweepProofMaterials].map(
        ({ mock }) => mock.calls.length,
      ),
    ).toEqual([1, 1]);
  });

  it("runs an objective inside its safe-start window", async () => {
    const test = await setup();
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

  it("drops a refusal hold once its header leaves the queue", async () => {
    const test = await setup();
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

  it("drops a pending transport retry once its header leaves the queue, so the retry never runs", async () => {
    const test = await setup();
    test.setBeforeRun(async () => {
      throw new FraudProofL1UnavailableError("canonical provider HTTP 503");
    });
    const supervisor = test.createSupervisor();
    await test.request(supervisor, 1).accepted;
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledOnce();
    // The bridge no longer selects it and its header left the queue; the
    // retry would otherwise fire with the permit it was first handed.
    await test.request(supervisor, 2, test.fixture.fresh, false).accepted;
    await test.idle(supervisor);
    // Past the first backoff (1 s).
    await new Promise((resolve) => setTimeout(resolve, 1_500));
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledOnce();
    expect(supervisor.status()).toMatchObject({
      phase: "accepting",
      unfinishedObjectiveCount: 0,
    });
  });

  it("releases an unheld objective once its header leaves the queue, with its rows, and admits it afresh if the header returns", async () => {
    const test = await setup();
    const { pinned, retention } = recordingRetention();
    const objective = `doubleSpend/${test.fixture.fresh.headerHash}`;
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
    };
    const supervisor = test.createSupervisor(retention);
    // It runs inside its window and is not held.
    await test.request(supervisor, 2).accepted;
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledOnce();
    expectOpen(supervisor);
    // A new observation that still queues its header keeps it.
    await test.request(supervisor, 3).accepted;
    await test.idle(supervisor);
    expectOpen(supervisor);
    // Its header left the finalized queue: nothing drives it again, so its
    // row, its L1 history pin and its unfinished count go.
    await test.request(supervisor, 4, test.fixture.fresh, false).accepted;
    await test.idle(supervisor);
    expectReleased(supervisor);
    // A rollback that brings the header back admits it as a fresh objective
    // in the same process.
    let runs = test.runOrResume.mock.calls.length;
    await test.request(supervisor, 5).accepted;
    await test.idle(supervisor);
    expectOpen(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(runs + 1);
    await test.request(supervisor, 6, test.fixture.fresh, false).accepted;
    await test.idle(supervisor);
    expectReleased(supervisor);
    runs = test.runOrResume.mock.calls.length;
    await supervisor.close();
    // A restart over the same stores does not adopt it again.
    const restarted = test.createSupervisor(retention);
    await test.request(restarted, 7, test.fixture.fresh, false).accepted;
    await test.idle(restarted);
    expectReleased(restarted);
    expect(test.runOrResume).toHaveBeenCalledTimes(runs);
    // A returning header after the restart admits it afresh.
    await test.request(restarted, 8).accepted;
    await test.idle(restarted);
    expectOpen(restarted);
    expect(test.runOrResume).toHaveBeenCalledTimes(runs + 1);
  });

  // A crash during a job (journals failing mid-run stand in for it) leaves
  // its queue row active; nothing in a later process finishes it.
  const crashMidJob = async (test: Awaited<ReturnType<typeof setup>>) => {
    const { pinned, retention } = recordingRetention({ atTip: true });
    test.setBeforeRun(async () => {
      throw new WatcherJournalUnavailableError("the process died mid-job");
    });
    const crashed = test.createSupervisor(retention);
    await test.request(crashed, 2).accepted;
    await test.idle(crashed);
    await crashed.close();
    expect(
      journalStates(test.fixture.journalRoot, "fault_proof_queue"),
    ).toEqual(["active"]);
    test.setBeforeRun(async () => undefined);
    return { pinned, restarted: test.createSupervisor(retention) };
  };

  it("settles the job a crash left active once its header leaves the queue, and releases the objective", async () => {
    const test = await setup();
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
    expect(test.runOrResume).toHaveBeenCalledOnce();
  });

  it("keeps the row of a job a crash left active while its header is queued with no fault naming it, and releases it once the header leaves", async () => {
    const test = await setup();
    const { pinned, restarted } = await crashMidJob(test);
    // While its header is queued the row stays: a live fault requeues it.
    await restarted.requestProgress({
      observation: progressObservation({
        hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
        header: test.fixture.fixture,
        revision: 3,
      }),
      rollbackGeneration: "3",
    });
    await test.idle(restarted);
    const objective = `doubleSpend/${test.fixture.fresh.headerHash}`;
    expect(objectiveRows(test.fixture.journalRoot)).toEqual([objective]);
    expect(
      journalStates(test.fixture.journalRoot, "fault_proof_queue"),
    ).toEqual(["active"]);
    // The header is still queued, so the pass at tip keeps its hold.
    expect([...pinned]).toEqual([objective]);
    await test.request(restarted, 4, test.fixture.fresh, false).accepted;
    await test.idle(restarted);
    expect(restarted.status()).toMatchObject({
      phase: "accepting",
      unfinishedObjectiveCount: 0,
      objectiveCleanupFailures: [],
    });
    // Out of the queue, nothing owns it: the pass releases it.
    expect([...pinned]).toEqual([]);
    expect(objectiveRows(test.fixture.journalRoot)).toEqual([]);
    expect(
      journalStates(test.fixture.journalRoot, "fault_proof_queue"),
    ).toEqual([]);
    expect(test.runOrResume).toHaveBeenCalledOnce();
  });

  it.each([
    ["marked beyond k is final and pruned once its header leaves", 2_160, true],
    // Depth > k is final (plan section 9): one verified at or below k is not
    // applicable, so it is never marked; it stays open and pinned and is
    // verified again on each observation until it is beyond k, its header
    // in the queue or not.
    [
      "not yet beyond k stays open while its header is queued",
      1_000_000_000,
      false,
    ],
  ])("a completion %s", async (_name, securityParameter, final) => {
    const test = await setup();
    test.setAfterRun(async () => {
      test.landRemoval();
      return test.completed();
    });
    const { pinned, retention } = recordingRetention();
    const supervisor = test.createSupervisor({
      ...retention,
      securityParameter,
    });
    const objective = `doubleSpend/${test.fixture.fresh.headerHash}`;
    const root = test.fixture.journalRoot;
    await test.request(supervisor, 1, test.fixture.old).accepted;
    await test.idle(supervisor);
    // A final completion's hold is released at once; one short of depth
    // keeps it.
    expect([...pinned]).toEqual(final ? [] : [objective]);
    // Its header stays queued. A final completion is kept; one short of
    // depth, which the fault still names, keeps its rows and is run and
    // verified again.
    if (final)
      await supervisor.requestProgress({
        observation: progressObservation({
          hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
          header: test.fixture.fixture,
          revision: 2,
        }),
        rollbackGeneration: "2",
      });
    else await test.request(supervisor, 2).accepted;
    await test.idle(supervisor);
    if (!final) {
      expect(supervisor.status()).toMatchObject({
        phase: "accepting",
        unfinishedObjectiveCount: 1,
      });
      expect(objectiveRows(root)).toEqual([objective]);
      expect([...pinned]).toEqual([objective]);
    }
    // The pass where its header has left the queue releases a final one.
    // One short of depth is our removal landed: the chain keeps it until
    // final, with no run of its family.
    await test.request(supervisor, 3, test.fixture.fresh, false).accepted;
    await test.idle(supervisor);
    if (!final) {
      expect(supervisor.status()).toMatchObject({
        phase: "accepting",
        unfinishedObjectiveCount: 1,
      });
      expect(objectiveRows(root)).toEqual([objective]);
      expect([...pinned]).toEqual([objective]);
      expect(test.runOrResume).toHaveBeenCalledTimes(2);
      expect(test.verifyCompleted).toHaveBeenCalledTimes(3);
      return;
    }
    expect(supervisor.status()).toMatchObject({
      phase: "accepting",
      unfinishedObjectiveCount: 0,
      objectiveCleanupFailures: [],
    });
    expect([...pinned]).toEqual([]);
    expect(objectiveRows(root)).toEqual([]);
    expect(journalStates(root, "fault_proof_queue")).toEqual([]);
    expect(journalStates(root, "fault_decisions")).toHaveLength(0);
    expect(test.runOrResume).toHaveBeenCalledOnce();
    expect(test.verifyCompleted).toHaveBeenCalledOnce();
  });

  it("leaves a final completion's rows to the job handed over after its finish", async () => {
    const test = await setup();
    const entered = deferred(),
      release = deferred();
    test.setBeforeRun(async () => {
      entered.resolve();
      await release.promise;
    });
    test.setAfterRun(async () => {
      test.landRemoval();
      return test.completed();
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
    await test.request(supervisor, 4, test.fixture.fresh, false).accepted;
    await test.idle(supervisor);
    expect(objectiveRows(test.fixture.journalRoot)).toEqual([]);
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
        test.landRemoval();
        return test.completed();
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
      // The next observation runs it again, which reads the landed removal
      // from the chain, and a read the source answers ends the hold.
      await test.request(supervisor, 2).accepted;
      await test.idle(supervisor);
      expect(supervisor.status().unfinishedObjectiveCount).toBe(0);
      expect(test.readiness()).toEqual([]);
      expect(test.runOrResume).toHaveBeenCalledTimes(2);
      expect(settled).toBe(false);
    },
  );

  it("coalesces two generations and verifies the completion without running again", async () => {
    const test = await setup();
    const entered = deferred(),
      release = deferred();
    test.setBeforeRun(async () => {
      entered.resolve();
      await release.promise;
    });
    test.setAfterRun(async () => {
      test.landRemoval();
      return test.completed();
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
    expect(supervisor.status().unfinishedObjectiveCount).toBe(0);
  });

  it.each([
    ["marked beyond k is final", 2_160, 1],
    ["not yet beyond k stays open and is verified again", 1_000_000_000, 2],
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
        test.landRemoval();
        return test.completed();
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
      expect(supervisor.status().unfinishedObjectiveCount).toBe(
        verifications === 1 ? 0 : 1,
      );
      expect(test.verifyCompleted).toHaveBeenCalledTimes(verifications);
      // A final completion is met by its marker; one short of depth is run
      // again, and the run reads the landed removal from the chain.
      expect(test.runOrResume).toHaveBeenCalledTimes(verifications);
      if (verifications === 1) expect(holds.at(-1)).toBe("release");
      else expect(holds).not.toContain("release");
    },
  );

  it("completes for decisions published by another handle after supervisor startup", async () => {
    const test = await setup();
    // The supervisor opens an empty decision reader; the decisions are
    // published afterwards by another handle.
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
        hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
      }),
      rollbackGeneration: "0",
    });
    await test.idle(supervisor);
    expect(initializedReader).toHaveBeenCalledOnce();
    const writer = await openWatcherFaultDecisionJournal({
      directory: test.fixture.journalRoot,
      hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
      launchScope: installedLaunchScope(test.fixture.old.launchScope),
      authenticationKey: TEST_JOURNAL_KEY,
    });
    await writer.appendLiveDecision(test.fixture.old);
    await writer.appendLiveDecision(test.fixture.fresh);
    const entered = deferred(),
      release = deferred();
    test.setBeforeRun(async () => {
      entered.resolve();
      await release.promise;
    });
    test.setAfterRun(async () => {
      test.landRemoval();
      return test.completed();
    });
    await test.request(supervisor, 1, test.fixture.old).accepted;
    await Promise.race([entered.promise, supervisor.done]);
    await test.request(supervisor, 2).accepted;
    release.resolve();
    await Promise.race([test.completionVerified, supervisor.done]);
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(1);
    expect(supervisor.status()).toMatchObject({
      phase: "accepting",
      unfinishedObjectiveCount: 0,
    });
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
  });

  registerLateStartTests(setup);
  it("does not spin or run again for identical pending observations", async () => {
    const test = await setup();
    const supervisor = test.createSupervisor();
    await test.request(supervisor, 2).accepted;
    await test.idle(supervisor);
    for (let repeat = 0; repeat < 10; repeat += 1)
      await test.request(supervisor, 2).accepted;
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(1);
  });

  it("retries the chain read of an objective past maturity whose source is unavailable, and completes it without a run", async () => {
    const test = await setup(20n);
    test.landRemoval();
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
    expect(supervisor.status()).toMatchObject({
      phase: "accepting",
      unfinishedObjectiveCount: 0,
      journalDecisionMissing: [],
    });
  });
});
