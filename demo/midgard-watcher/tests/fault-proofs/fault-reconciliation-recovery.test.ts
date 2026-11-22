import {
  assertWorkflowRunActuation,
  bindWorkflowRunActuation,
  createWorkflowReconciliationPermitController,
  workflowRunIsReconciliationOnly,
} from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import type { HeaderFaultDecision } from "@al-ft/midgard-fault-proofs/workflow/header-classifier";
import {
  normalizeWorkflowJson,
  workflowJsonDigest,
} from "@al-ft/midgard-fault-proofs/workflow/identity";
import {
  createWorkflowObjectiveWallet,
  createWorkflowRun,
} from "@al-ft/midgard-fault-proofs/workflow/workflow-run";
import { describe, expect, it } from "vitest";

import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "../../src/fault-proofs/fault-proof-application.production-dependencies.js";

const DEPLOYMENT = "dd".repeat(28),
  HEADER = "aa".repeat(28);
const fixture = (): { decision: HeaderFaultDecision } => {
  const envelope = {
    schemaVersion: "midgard-production-header-decision-v1",
    classifierVersion: "midgard-production-header-classifier-v1",
    hubOraclePolicyId: DEPLOYMENT,
    headerHash: HEADER,
    authenticatedObservationDigest: "11".repeat(32),
    payloadEnvelopeSha256: "12".repeat(32),
    payloadSha256: "13".repeat(32),
    replayVersion: "midgard-complete-canonical-replay-v1",
    replayDigest: "14".repeat(32),
    launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    launchScopeDigest: workflowJsonDigest([
      ...WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    ]),
    classificationDigest: "16".repeat(32),
    decision: "fault_detected",
    category: "transitionTrace",
    violationId: "transitionTrace_v1",
    detectionId: "transitionTrace_v1:0",
    position: "0",
  } as const;
  const decision = {
    ...envelope,
    decisionDigest: workflowJsonDigest(normalizeWorkflowJson(envelope)),
  };
  return { decision };
};

describe("reconciliation workflow authority", () => {
  it("lets a run bound to it observe and verify its terminal, and refuses everything once revoked", () => {
    const { decision } = fixture();
    const controller = createWorkflowReconciliationPermitController({
      decision,
      hubOraclePolicyId: DEPLOYMENT,
      rollbackGeneration: "0",
    });
    const run = createWorkflowRun(
      createWorkflowObjectiveWallet({
        category: "transitionTrace",
        headerHash: HEADER,
      }),
    );
    bindWorkflowRunActuation({
      run,
      permit: controller.permit,
      decisionDigest: decision.decisionDigest,
      hubOraclePolicyId: DEPLOYMENT,
      category: "transitionTrace",
      headerHash: HEADER,
    });
    expect(workflowRunIsReconciliationOnly(run)).toBe(true);
    const check = (
      checkpoint: Parameters<
        typeof assertWorkflowRunActuation
      >[0]["checkpoint"],
    ) =>
      assertWorkflowRunActuation({
        run,
        hubOraclePolicyId: DEPLOYMENT,
        category: "transitionTrace",
        headerHash: HEADER,
        checkpoint,
      });
    check("before_observe");
    check("before_terminal_verify");
    controller.revoke("native_chain_rollback");
    expect(() => check("before_observe")).toThrow("native_chain_rollback");
  });
  it("rejects substituted evidence and a foreign deployment", () => {
    const { decision } = fixture();
    const mint = (
      changes: Partial<
        Parameters<typeof createWorkflowReconciliationPermitController>[0]
      >,
    ) =>
      createWorkflowReconciliationPermitController({
        decision,
        hubOraclePolicyId: DEPLOYMENT,
        rollbackGeneration: "0",
        ...changes,
      });
    expect(() => mint({ hubOraclePolicyId: "ff".repeat(28) })).toThrow(
      "exact classified fault",
    );
    expect(() =>
      mint({ decision: { ...decision, payloadSha256: "fe".repeat(32) } }),
    ).toThrow("exact classified fault");
  });
});
