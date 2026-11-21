/**
 * An objective whose latest safe start passed with nothing signed. The
 * latest safe start is a design margin, not the last moment a removal lands:
 * the state queue accepts a completed fraud-proof removal until maturity. So
 * the watcher keeps trying, loudly, until maturity; only then is the
 * objective held by name.
 */
import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { WORKFLOW_FUNDING_SHORTFALL } from "@al-ft/midgard-fault-proofs/workflow/workflow-funding-shortfall";
import { expect, it, vi } from "vitest";

import { WatcherFaultProofL1RefusedError } from "../../src/l1-follower/fault-proof-l1-source.chain.js";
import {
  createWatcherOperationsObservability,
  type WatcherOperationsSink,
} from "../../src/runtime/operations-observability.js";
import { deploymentIdentity } from "../support/classified-fault-fixture.js";
import type { setup } from "./fault-proof-objective-progress.test.js";

const LATEST_SAFE_START_OFFSET_MS =
  MIDGARD_RETENTION_WINDOW.maturityMs -
  MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs;

export const registerLateStartTests = (setupScenario: typeof setup): void => {
  it("keeps trying an objective past its latest safe start but before maturity, with a deadline alert and a funding alert, and proves it once funding returns", async () => {
    // The supervisor's sink is the operations view's, which needs the
    // supervisor: alerts are forwarded to it once it exists.
    const forward: { sink?: WatcherOperationsSink } = {};
    const test = await setupScenario(
      // Its latest safe start passed a minute ago; maturity is about the
      // worst-case proof time away.
      BigInt(Date.now() - LATEST_SAFE_START_OFFSET_MS - 60_000),
      {
        recordProofStep: (step) => forward.sink?.recordProofStep(step),
        setAlert: (alert) => forward.sink?.setAlert(alert),
      },
    );
    const supervisor = test.createSupervisor();
    const operations = createWatcherOperationsObservability({
      deploymentFingerprint: deploymentIdentity.manifestId,
      supervisor,
      launchScopeStatus: () => ({
        installedCategoryCount: 1,
        requiredCategoryCount: 1,
      }),
      retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
      durableProofQueueStatus: () => supervisor.durableQueueStatus(),
      l1Readiness: test.readiness,
    });
    forward.sink = operations.sink;
    const objective = `doubleSpend/${test.fixture.fresh.headerHash}`;
    const alerts = () =>
      operations.api
        .status()
        .activeAlerts.map(({ code }) => code)
        .sort();
    const logged: string[] = [];
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation((line) => logged.push(String(line)) > 0);
    // The prover wallet cannot fund the run's next transaction.
    test.setAfterRun(async () => ({
      kind: "pending",
      resumeOnObservation: true,
      reason: `${WORKFLOW_FUNDING_SHORTFALL}: the prover wallet cannot fund init from its plain-ADA coins (a coin holding a token is never spent): Your wallet does not have enough funds`,
    }));
    try {
      await test.request(supervisor, 2).accepted;
      await test.idle(supervisor);
    } finally {
      stderr.mockRestore();
    }
    // Not refused: nothing holds it by name, and the run was attempted.
    expect(test.runOrResume).toHaveBeenCalledOnce();
    expect(supervisor.status()).toMatchObject({
      phase: "accepting",
      blockedJob: null,
      journalDecisionMissing: [],
      deadlineHealth: "unsafe",
    });
    expect(BigInt(supervisor.status().remainingSafeStartMs!)).toBeLessThan(0n);
    expect(test.readiness().map(({ reason }) => reason)).toEqual([
      "prover_funding_unavailable",
      "fault_proof_started_late",
    ]);
    expect(alerts()).toEqual([
      "maturity_deadline_risk",
      "prover_funding_unavailable",
    ]);
    expect(operations.api.status().readiness).toBe("not_ready");
    // One error line names the objective and its maturity.
    expect(
      logged.filter((line) => line.includes("fault_proof_started_late")),
    ).toEqual([expect.stringContaining(objective)]);
    // A top-up before maturity: the next observation runs it.
    test.setAfterRun(async (result) => result);
    await test.request(supervisor, 3).accepted;
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(2);
    expect(alerts()).toEqual(["maturity_deadline_risk"]);
    expect(test.readiness().map(({ reason }) => reason)).toEqual([
      "fault_proof_started_late",
    ]);
    // Its correction is included: the race against maturity is over.
    test.setAfterRun(async () => {
      test.landRemoval();
      return test.completed();
    });
    await test.request(supervisor, 4).accepted;
    await test.idle(supervisor);
    expect(alerts()).toEqual([]);
    expect(test.readiness()).toEqual([]);
    expect(supervisor.status()).toMatchObject({
      unfinishedObjectiveCount: 0,
      deadlineHealth: "safe",
      remainingSafeStartMs: null,
    });
  });

  it("settles a late objective's deadline and clears its alerts once its header leaves the queue, leaving its send to the chain", async () => {
    const forward: { sink?: WatcherOperationsSink } = {};
    const test = await setupScenario(
      BigInt(Date.now() - LATEST_SAFE_START_OFFSET_MS - 60_000),
      {
        recordProofStep: (step) => forward.sink?.recordProofStep(step),
        setAlert: (alert) => forward.sink?.setAlert(alert),
      },
    );
    const supervisor = test.createSupervisor();
    const operations = createWatcherOperationsObservability({
      deploymentFingerprint: deploymentIdentity.manifestId,
      supervisor,
      launchScopeStatus: () => ({
        installedCategoryCount: 1,
        requiredCategoryCount: 1,
      }),
      retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
      durableProofQueueStatus: () => supervisor.durableQueueStatus(),
      l1Readiness: test.readiness,
    });
    forward.sink = operations.sink;
    const alerts = () =>
      operations.api.status().activeAlerts.map(({ code }) => code);
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation(() => true);
    try {
      await test.request(supervisor, 2).accepted;
      await test.idle(supervisor);
    } finally {
      stderr.mockRestore();
    }
    expect(alerts()).toEqual(["maturity_deadline_risk"]);
    expect(supervisor.status().deadlineHealth).toBe("unsafe");
    // The header merges: it leaves the queue with the step unresolved.
    // Nothing drives the objective again; what it sent is the chain's.
    await test.request(supervisor, 3, test.fixture.fresh, false).accepted;
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledOnce();
    expect(supervisor.status()).toMatchObject({
      unfinishedObjectiveCount: 0,
      deadlineHealth: "safe",
      remainingSafeStartMs: null,
    });
    expect(alerts()).toEqual([]);
    expect(test.readiness()).toEqual([]);
  });

  it("raises a late objective's deadline alert again after a restart before its maturity, and after its maturity with nothing landed holds it by name without running it", async () => {
    vi.useFakeTimers({ toFake: ["Date"] });
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation(() => true);
    try {
      const startedAt = Date.now();
      // Its latest safe start passed a minute ago.
      const headerEndTime = startedAt - LATEST_SAFE_START_OFFSET_MS - 60_000;
      const forward: { sink?: WatcherOperationsSink } = {};
      const test = await setupScenario(BigInt(headerEndTime), {
        recordProofStep: (step) => forward.sink?.recordProofStep(step),
        setAlert: (alert) => forward.sink?.setAlert(alert),
      });
      /** One watcher process: a supervisor and its own operations view. */
      const start = () => {
        const supervisor = test.createSupervisor();
        const operations = createWatcherOperationsObservability({
          deploymentFingerprint: deploymentIdentity.manifestId,
          supervisor,
          launchScopeStatus: () => ({
            installedCategoryCount: 1,
            requiredCategoryCount: 1,
          }),
          retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
          durableProofQueueStatus: () => supervisor.durableQueueStatus(),
          l1Readiness: test.readiness,
        });
        forward.sink = operations.sink;
        return { supervisor, operations };
      };
      const late = async (
        process: ReturnType<typeof start>,
        generation: number,
      ) => {
        await test.request(process.supervisor, generation).accepted;
        await test.idle(process.supervisor);
        return {
          alerts: process.operations.api
            .status()
            .activeAlerts.map(({ code }) => code),
          readiness: test.readiness().map(({ reason }) => reason),
        };
      };
      const raised = {
        alerts: ["maturity_deadline_risk"],
        readiness: ["fault_proof_started_late"],
      };
      const first = start();
      expect(await late(first, 2)).toEqual(raised);
      await first.supervisor.close();

      // A restart before maturity raises them again and runs it again.
      const second = start();
      expect(await late(second, 3)).toEqual(raised);
      expect(test.runOrResume).toHaveBeenCalledTimes(2);
      await second.supervisor.close();

      // After maturity the chain shows no removal: it is held by name, never
      // run.
      vi.setSystemTime(
        headerEndTime + MIDGARD_RETENTION_WINDOW.maturityMs + 60_000,
      );
      const third = start();
      await test.request(third.supervisor, 4).accepted;
      await test.idle(third.supervisor);
      expect(third.supervisor.status().journalDecisionMissing).toMatchObject([
        { readiness: "fault_proof_start_deadline_passed" },
      ]);
      expect(test.runOrResume).toHaveBeenCalledTimes(2);
      expect(test.verifyCompleted).toHaveBeenCalledOnce();
    } finally {
      stderr.mockRestore();
      vi.useRealTimers();
    }
  });

  it("drops a refusal hold once maturity holds the objective under its own name", async () => {
    vi.useFakeTimers({ toFake: ["Date"] });
    try {
      const startedAt = Date.now();
      // Its latest safe start is a minute away.
      const test = await setupScenario(
        BigInt(startedAt - LATEST_SAFE_START_OFFSET_MS + 60_000),
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
      // A minute past its maturity.
      vi.setSystemTime(
        startedAt +
          60_000 +
          MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs +
          60_000,
      );
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
};
