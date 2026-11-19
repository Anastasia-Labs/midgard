import { FRAUD_PROOF_WORKFLOW_FRESH_SUBMISSION_REQUIRED } from "@al-ft/midgard-fault-proofs";
import { expect, it, vi } from "vitest";

import { WATCHER_FAULT_PROOF_STARTED_LATE } from "../../src/fault-proofs/fault-proof-objective-alerts.js";
import { WATCHER_FAULT_PROOF_RESUBMISSION_REQUIRED } from "../../src/fault-proofs/fault-proof-resubmission-hold.js";
import { createWatcherOperationsObservability } from "../../src/runtime/operations-observability.js";
import { deploymentIdentity } from "../support/fault-proof-funding-fixture.sources-for.js";
import type { setup } from "./fault-proof-objective-progress.test.js";

export const registerFaultProofResubmissionVisibilityTest = (
  setupScenario: typeof setup,
): void => {
  it("names an objective whose next action needs a new submission after its header matured on readiness and metrics until a run moves on", async () => {
    const test = await setupScenario(20n);
    // The signed attempt reconciles, but the next step needs a new
    // submission, which reconciliation-only authority never makes.
    vi.mocked(test.fixture.adapter.observe).mockResolvedValueOnce({
      kind: "action_required",
      action: { actionId: "next", input: {} },
    });
    const supervisor = test.createSupervisor();
    await test.request(supervisor, 2).accepted;
    await test.idle(supervisor);
    const objective = `doubleSpend/${test.fixture.old.headerHash}`;
    // Past maturity it also ran past its latest safe start, which stays
    // named until its header leaves the queue.
    const resubmission = () =>
      test
        .readiness()
        .filter(
          ({ reason }) => reason === WATCHER_FAULT_PROOF_RESUBMISSION_REQUIRED,
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
    expect(test.fixture.adapter.preflight).not.toHaveBeenCalled();
    expect(test.fixture.adapter.submit).not.toHaveBeenCalled();
    // A later observation that no longer needs the action clears it.
    await test.request(supervisor, 3).accepted;
    await test.idle(supervisor);
    expect(test.runOrResume).toHaveBeenCalledTimes(2);
    expect(resubmission()).toEqual([]);
    expect(operations.api.metrics().l1Readiness).toEqual({
      [WATCHER_FAULT_PROOF_STARTED_LATE]: "1",
    });
  });
};
