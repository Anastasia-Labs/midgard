import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import {
  authenticatedHeaderObservation,
  buildCanonicalBlockFixture,
  buildFixtureTransaction,
  outRefCbor,
} from "@al-ft/midgard-fault-proofs/test-support/canonical-block-evidence-fixture";
import { createWorkflowActuationPermitController } from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import type { WorkflowAdapterRunnerInput } from "@al-ft/midgard-fault-proofs/workflow/adapters";
import { DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY } from "@al-ft/midgard-fault-proofs/workflow/complete-replay";
import { verifyCompletedFraudProofWorkflow } from "@al-ft/midgard-fault-proofs/workflow/completed-verification";
import {
  authenticatedStateQueueObservationDigest,
  classifyHeader,
  createHeaderClassifier,
} from "@al-ft/midgard-fault-proofs/workflow/header-classifier";
import { WORKFLOW_RUNNER_FACTORIES } from "@al-ft/midgard-fault-proofs/workflow/runtime";
import { afterEach, describe, expect, it, vi } from "vitest";

import { openWatcherFaultDecisionJournal } from "../../src/fault-proofs/fault-decision-journal.create-journal.js";
import { installedLaunchScope } from "../../src/fault-proofs/fault-proof-application.production-dependencies.js";
import { createWatcherFaultProofExecution } from "../../src/fault-proofs/fault-proof-execution.js";
import { createWatcherFaultProofSupervisor } from "../../src/fault-proofs/fault-proof-supervisor.create-watcher-fault-proof-supervisor.js";
import { watcherFaultProofDeadline } from "../../src/fault-proofs/fault-proof-supervisor.validate-job.js";
import { openWatcherJournalDatabase } from "../../src/fault-proofs/watcher-journal-database.js";
import {
  cleanupClassifiedFaultFixtures,
  deploymentIdentity,
  finality,
  setupClassifiedFaultFixture,
  sourcesFor,
} from "../support/classified-fault-fixture.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { landedRemoval } from "../support/landed-removal.js";
import { storelessProofRetention } from "../support/proof-retention.js";
import { TEST_JOURNAL_KEY } from "../support/watcher-journal-fixture.js";

vi.mock("../../src/fault-proofs/fault-proof-application.js", async (load) => ({
  ...(await load<
    typeof import("../../src/fault-proofs/fault-proof-application.js")
  >()),
  WATCHER_INSTALLED_WORKFLOW_CATEGORIES: Object.freeze(["doubleSpend"]),
}));

const closers: (() => Promise<void> | void)[] = [];
afterEach(async () => {
  for (const close of closers.splice(0).reverse()) await close();
  await cleanupClassifiedFaultFixtures();
});

const QUARANTINED = "fault_proof_objective_quarantined";

/** A second double spend in its own block: another objective of the watcher. */
const secondObjective = async (
  fixture: Awaited<ReturnType<typeof setupClassifiedFaultFixture>>,
) => {
  const shared = outRefCbor(92, 0n);
  const block = await buildCanonicalBlockFixture({
    endTime: BigInt(Date.now()),
    transactions: [
      buildFixtureTransaction({ spendInputs: [shared], fee: 3n }),
      buildFixtureTransaction({ spendInputs: [shared], fee: 4n }),
    ],
  });
  const observation = {
    ...authenticatedHeaderObservation(block),
    confirmationDepth: 31,
  };
  const decision = await classifyHeader({
    classifier: await createHeaderClassifier({
      hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
      replayer: DOUBLE_SPEND_COMPLETE_CANONICAL_REPLAY,
      releaseFinalityAuthority: finality,
    }),
    observation,
    authenticatedObservationDigest:
      await authenticatedStateQueueObservationDigest({
        observation,
        minimumConfirmationDepth: 30,
      }),
    sources: sourcesFor(block.payloadEnvelopeCbor),
  });
  if (decision.decision !== "fault_detected")
    throw new Error("second block must classify a double spend");
  await (
    await openWatcherFaultDecisionJournal({
      directory: fixture.journalRoot,
      hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
      launchScope: installedLaunchScope(decision.launchScope),
      authenticationKey: TEST_JOURNAL_KEY,
    })
  ).appendLiveDecision(decision);
  return { block, decision };
};

/**
 * One watcher with two objectives: the convicted one's run lands our
 * removal, which the real verification checks against raw L1; `held` is the
 * second objective, whose run meets `cause` until the test removes it.
 */
const setup = async (cause: () => unknown) => {
  const fixture = await setupClassifiedFaultFixture({
    headerEndTime: BigInt(Date.now()),
  });
  const second = await secondObjective(fixture);
  const landed = await landedRemoval(fixture);
  const verifyCompleted = vi.fn(verifyCompletedFraudProofWorkflow);
  let present = true;
  const runOrResume = vi.fn(async (invocation: WorkflowAdapterRunnerInput) => {
    if (invocation.headerHash === second.decision.headerHash) {
      if (present) {
        const met = cause();
        if (met instanceof Error) throw met;
        return met;
      }
      return {
        kind: "pending",
        reason: "signed transaction awaiting inclusion",
      };
    }
    return landed.completed(invocation.decisionDigest);
  });
  const runner = WORKFLOW_RUNNER_FACTORIES.doubleSpend(async () => {
    throw new Error("the quarantine tests never build through the runner");
  });
  const setAlert = vi.fn();
  const recordProofStep = vi.fn();
  /** A watcher process over the fixture's journals: a later call is a
   * restart of the one before, which it closes. */
  const start = async () => {
    const execution = createWatcherFaultProofExecution({
      application: {
        runners: { doubleSpend: runner },
        runOrResume,
        verifyCompleted: landed.verifyCompleted(
          async () => landed.raw.snapshot,
          verifyCompleted,
        ),
      },
      runtimeConfigPath: "/unused-quarantine-test.json",
      hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
      operationsSink: { recordProofStep, setAlert },
    });
    const supervisor = createWatcherFaultProofSupervisor({
      proofRetention: storelessProofRetention,
      journalRoot: fixture.journalRoot,
      hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
      deadlineAlertHeadroomMs:
        MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs,
      queueAuthenticationKey: TEST_JOURNAL_KEY,
      execution,
    });
    closers.push(() => supervisor.close());
    supervisor.done.catch((error: unknown) => {
      failed = error;
    });
    return { execution, supervisor };
  };
  let failed: unknown;
  let watcher = await start();
  const observation = progressObservation({
    hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
    headers: [fixture.fixture, second.block],
    revision: 1,
  });
  const request = (
    decision: typeof second.decision,
    observed: typeof observation = observation,
  ) => ({
    observation: observed,
    rollbackGeneration: "1",
    fault: {
      decision,
      actuationPermit: createWorkflowActuationPermitController({
        decision,
        rollbackGeneration: "1",
      }).permit,
      deadline: watcherFaultProofDeadline(
        observed.finalizedHeaders.find(
          ({ headerHash }) => headerHash === decision.headerHash,
        )!,
      ),
    },
  });
  const queue = () =>
    openWatcherJournalDatabase({
      journalRoot: fixture.journalRoot,
      authenticationKey: TEST_JOURNAL_KEY,
    })
      .rows("fault_proof_objectives")
      .map(({ body, state }) => ({
        headerHash: (body as { headerHash: string }).headerHash,
        state,
      }));
  const heldRuns = () =>
    runOrResume.mock.calls.filter(
      ([invocation]) => invocation.headerHash === second.decision.headerHash,
    ).length;
  const alerts = (active: boolean) =>
    setAlert.mock.calls.filter(
      ([alert]) =>
        (alert as { code: string; subjectDigest: string; active: boolean })
          .code === "proof_submission_failure" &&
        (alert as { subjectDigest: string }).subjectDigest ===
          second.decision.decisionDigest &&
        (alert as { active: boolean }).active === active,
    ).length;
  /** The prover-step statuses the held objective recorded. */
  const heldSteps = () =>
    recordProofStep.mock.calls.flatMap(([step]) =>
      (step as { decisionDigest: string }).decisionDigest ===
      second.decision.decisionDigest
        ? [(step as { status: string }).status]
        : [],
    );
  return {
    fixture,
    second,
    get supervisor() {
      return watcher.supervisor;
    },
    get execution() {
      return watcher.execution;
    },
    failed: () => failed,
    dispatch: async () => {
      await watcher.supervisor.requestProgress(request(second.decision));
      await watcher.supervisor.requestProgress(request(fixture.old));
    },
    /** An admission naming the convicted objective alone. */
    admitOther: async () =>
      await watcher.supervisor.requestProgress(request(fixture.old)),
    /** An admission of the convicted objective from an observation the
     * held objective's header has left. */
    depart: async () =>
      await watcher.supervisor.requestProgress(
        request(
          fixture.old,
          progressObservation({
            hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
            headers: [fixture.fixture],
            revision: 2,
          }),
        ),
      ),
    restart: async () => {
      await watcher.supervisor.close();
      watcher = await start();
    },
    heldSteps,
    queue,
    heldRuns,
    alerts,
    verifyCompleted,
    removeCause: () => {
      present = false;
    },
  };
};

describe("an objective that stalls or meets its own integrity error is quarantined alone", () => {
  it.each([
    [
      "a workflow stall outside preflight",
      () => ({
        kind: "stalled",
        reason: "step_02 thread output is absent at its expected reference",
      }),
    ],
    [
      "an integrity error from its run",
      () => new Error("signed transaction hash changed between build and send"),
    ],
    [
      "its family re-classifying the admitted fault",
      () => ({ kind: "no_fault_detected", classification: {} }),
    ],
  ])(
    "holds the objective after %s while another objective convicts, and clears once the cause is gone",
    async (_name, cause) => {
      const test = await setup(cause);
      await test.dispatch();

      // The other objective runs to its verified completion.
      await vi.waitFor(
        () =>
          expect(test.queue()).toContainEqual({
            headerHash: test.fixture.old.headerHash,
            state: "marked",
          }),
        { timeout: 30_000 },
      );
      expect(test.verifyCompleted).toHaveBeenCalled();

      // The held objective is named on readiness and alarmed once, is run
      // again on its backoff, and the process never fails.
      await vi.waitFor(() => expect(test.heldRuns()).toBeGreaterThan(1), {
        timeout: 30_000,
      });
      expect(test.execution.readiness()).toEqual([
        {
          reason: QUARANTINED,
          detail: expect.stringContaining(
            `doubleSpend/${test.second.decision.headerHash}: `,
          ),
        },
      ]);
      expect(test.alerts(true)).toBe(1);
      expect(test.alerts(false)).toBe(0);
      expect(test.failed()).toBeUndefined();
      expect(test.supervisor.status().phase).toBe("accepting");

      // With the cause gone, the next re-check ends the hold and its alarm.
      test.removeCause();
      await vi.waitFor(() => expect(test.execution.readiness()).toEqual([]), {
        timeout: 60_000,
      });
      expect(test.alerts(false)).toBe(1);
      expect(test.failed()).toBeUndefined();
      expect(test.supervisor.status().phase).toBe("accepting");
    },
    120_000,
  );
});

/** The run's stall while no terminal of ours stands on a header gone. */
const goneStall = (detail: string) => ({
  kind: "stalled",
  reason: `observation failed: doubleSpend raw L1 derivation failed: ${detail}`,
});

const settledRuns = async (runs: () => number) => {
  const before = runs();
  // Past the quarantine backoff: a run still scheduled would have started.
  await new Promise((resolve) => setTimeout(resolve, 2_500));
  return { before, after: runs() };
};

describe("an objective whose header left the queue without a conviction of ours", () => {
  it("ends, never as our conviction, once its run reads the header as removed against another record, and stays ended across a restart", async () => {
    let removed = false;
    const test = await setup(() =>
      removed
        ? {
            kind: "resolved_by_another_record",
            resolution: {
              removalTxHash: "c5".repeat(32),
              referencedProofTokenOutRef: `${"c4".repeat(32)}#0`,
              confirmationDepth: 40,
            },
          }
        : goneStall("no terminal of ours stands: terminal_not_ours"),
    );
    const held = () =>
      test
        .queue()
        .filter(
          ({ headerHash }) => headerHash === test.second.decision.headerHash,
        );
    await test.dispatch();
    await vi.waitFor(() => expect(test.heldRuns()).toBeGreaterThan(1), {
      timeout: 30_000,
    });
    expect(test.execution.readiness()).toEqual([
      expect.objectContaining({ reason: QUARANTINED }),
    ]);
    expect(test.alerts(true)).toBe(1);

    // The removal against another record lands and is settled.
    removed = true;
    await vi.waitFor(() => expect(test.execution.readiness()).toEqual([]), {
      timeout: 60_000,
    });
    expect(test.alerts(false)).toBe(1);
    expect(test.heldSteps()).toContain("cancelled");
    expect(test.heldSteps()).not.toContain("completed");
    expect(held().map(({ state }) => state)).not.toContain("marked");
    // It is not run again, and the next admission forgets its rows.
    const ended = await settledRuns(test.heldRuns);
    expect(ended.after).toBe(ended.before);
    await test.admitOther();
    await vi.waitFor(() => expect(held()).toEqual([]), { timeout: 30_000 });

    await test.restart();
    await test.admitOther();
    const restarted = await settledRuns(test.heldRuns);
    expect(restarted.after).toBe(ended.after);
    expect(test.execution.readiness()).toEqual([]);
    expect(held()).toEqual([]);
    expect(test.alerts(true)).toBe(1);
    expect(test.failed()).toBeUndefined();
    expect(test.supervisor.status().phase).toBe("accepting");
  }, 120_000);

  it("clears the alarm of a quarantined objective whose header departs between its retries", async () => {
    const test = await setup(() =>
      goneStall("fraudulent header disappeared without a retained proof token"),
    );
    await test.dispatch();
    // Its second run has ended quarantined; the next waits on the backoff.
    await vi.waitFor(
      () =>
        expect(
          test.heldSteps().filter((status) => status === "failed"),
        ).toHaveLength(2),
      { timeout: 30_000 },
    );
    await new Promise((resolve) => setTimeout(resolve, 500));
    expect(test.alerts(true)).toBe(1);

    await test.depart();
    await vi.waitFor(() => expect(test.execution.readiness()).toEqual([]), {
      timeout: 30_000,
    });
    // No retry raises it again once the objective has settled.
    const ended = await settledRuns(test.heldRuns);
    expect(ended.after).toBe(ended.before);
    expect(test.execution.readiness()).toEqual([]);
    expect(test.alerts(true)).toBe(1);
    expect(test.alerts(false)).toBe(1);
    expect(test.failed()).toBeUndefined();
  }, 120_000);

  it("keeps the objective quarantined and alarmed when no removal against a record of its header took it out", async () => {
    const test = await setup(() =>
      goneStall("fraudulent header disappeared without a retained proof token"),
    );
    await test.dispatch();
    await vi.waitFor(() => expect(test.heldRuns()).toBeGreaterThan(2), {
      timeout: 60_000,
    });
    await test.admitOther();
    expect(test.execution.readiness()).toEqual([
      {
        reason: QUARANTINED,
        detail: expect.stringContaining(
          "disappeared without a retained proof token",
        ),
      },
    ]);
    expect(test.alerts(true)).toBe(1);
    expect(test.alerts(false)).toBe(0);
    expect(test.heldSteps()).not.toContain("cancelled");
    expect(test.queue()).toContainEqual(
      expect.objectContaining({ headerHash: test.second.decision.headerHash }),
    );
    expect(test.failed()).toBeUndefined();
  }, 120_000);
});
