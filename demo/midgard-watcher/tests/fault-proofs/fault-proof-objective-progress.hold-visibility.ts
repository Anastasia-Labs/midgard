import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { FRAUD_PROOF_WORKFLOW_FRESH_SUBMISSION_REQUIRED } from "@al-ft/midgard-fault-proofs/workflow/orchestrator";
import { expect, it, vi } from "vitest";

import { WATCHER_FAULT_PROOF_STARTED_LATE } from "../../src/fault-proofs/fault-proof-objective-alerts.js";
import { WATCHER_FAULT_PROOF_RESUBMISSION_REQUIRED } from "../../src/fault-proofs/fault-proof-resubmission-hold.js";
import { createWatcherOperationsObservability } from "../../src/runtime/operations-observability.js";
import { deploymentIdentity } from "../support/classified-fault-fixture.js";
import type { setup } from "./fault-proof-objective-progress.test.js";

export const registerFaultProofResubmissionVisibilityTest = (
  setupScenario: typeof setup,
): void => {
  it("names an objective whose next action needs a new submission after its header matured on readiness and metrics until a run moves on", async () => {
    vi.useFakeTimers({ toFake: ["Date"] });
    const stderr = vi
      .spyOn(process.stderr, "write")
      .mockImplementation(() => true);
    try {
      const startedAt = Date.now();
      const test = await setupScenario(BigInt(startedAt));
      // Inside its window the run sends a step, which stays in flight.
      test.sendOnRun();
      const supervisor = test.createSupervisor();
      await test.request(supervisor, 1).accepted;
      await test.idle(supervisor);
      // Past maturity the process continues what it has in flight, but the
      // next step needs a new submission, which reconciliation-only
      // authority never makes.
      vi.setSystemTime(
        startedAt + MIDGARD_RETENTION_WINDOW.maturityMs + 60_000,
      );
      test.setAfterRun(async () => ({
        kind: "pending",
        reason: FRAUD_PROOF_WORKFLOW_FRESH_SUBMISSION_REQUIRED,
      }));
      await test.request(supervisor, 2).accepted;
      await test.idle(supervisor);
      const objective = `doubleSpend/${test.fixture.old.headerHash}`;
      // Past maturity it also ran past its latest safe start, which stays
      // named until its header leaves the queue.
      const resubmission = () =>
        test
          .readiness()
          .filter(
            ({ reason }) =>
              reason === WATCHER_FAULT_PROOF_RESUBMISSION_REQUIRED,
          );
      expect(resubmission()).toEqual([
        {
          reason: WATCHER_FAULT_PROOF_RESUBMISSION_REQUIRED,
          detail: `${objective}: ${FRAUD_PROOF_WORKFLOW_FRESH_SUBMISSION_REQUIRED}`,
        },
      ]);
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
        nowMs: () => 100_000n,
      });
      expect(operations.api.status()).toMatchObject({
        readiness: "not_ready",
        readinessReasons: expect.arrayContaining([
          WATCHER_FAULT_PROOF_RESUBMISSION_REQUIRED,
        ]),
      });
      expect(operations.api.metrics().l1Readiness).toEqual({
        [WATCHER_FAULT_PROOF_RESUBMISSION_REQUIRED]: "1",
        [WATCHER_FAULT_PROOF_STARTED_LATE]: "1",
      });
      // A later observation that no longer needs the action clears it.
      test.setAfterRun(async (result) => result);
      await test.request(supervisor, 3).accepted;
      await test.idle(supervisor);
      expect(test.runOrResume).toHaveBeenCalledTimes(3);
      expect(resubmission()).toEqual([]);
      expect(operations.api.metrics().l1Readiness).toEqual({
        [WATCHER_FAULT_PROOF_STARTED_LATE]: "1",
      });
    } finally {
      stderr.mockRestore();
      vi.useRealTimers();
    }
  });
};
