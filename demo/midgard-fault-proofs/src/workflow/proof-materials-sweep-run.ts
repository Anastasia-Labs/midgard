/**
 * The proof-materials sweep of an objective the chain alone completed: its
 * header left the queue and our removal is final, so no run of its family
 * happens. One run of the orchestrator over a family that has nothing left
 * to do takes the run's own sweep at its ending (`proof-materials-sweep.ts`),
 * under the objective's reconciliation-only authority, which admits a sweep
 * by its kind. The completion is the chain's already: this run neither
 * prepares nor verifies anything, and what it sweeps never changes it.
 */
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";

import type { FraudProofWorkflowTerminal } from "./journal.js";
import {
  FRAUD_PROOF_WORKFLOW_ADAPTER,
  FRAUD_PROOF_WORKFLOW_SAFETY,
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  type FraudProofFamilyWorkflowAdapter,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import type { FraudProofWorkflowRunResult } from "./orchestrator.fraud-proof-workflow-run-result.js";
import { runAdmittedFraudProofWorkflow } from "./orchestrator.run-admitted-fraud-proof-workflow.js";
import type { VerifiedFraudProofReleaseFinalityPolicy } from "./release-finality-policy.js";
import type { WorkflowRun } from "./workflow-run.js";

const noFamilyAction = async (): Promise<never> => {
  throw new Error("a completed objective's sweep builds no family action");
};

export const runProofMaterialsSweep = async ({
  terminal,
  ...admitted
}: {
  readonly deploymentFingerprint: string;
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  /** Bound to the objective's reconciliation permit. */
  readonly run: WorkflowRun;
  /** The completion the chain verified, final at release depth. */
  readonly terminal: FraudProofWorkflowTerminal;
  readonly releaseFinality: VerifiedFraudProofReleaseFinalityPolicy;
}): Promise<FraudProofWorkflowRunResult> => {
  const completed: FraudProofFamilyWorkflowAdapter = {
    adapterVersion: FRAUD_PROOF_WORKFLOW_ADAPTER,
    category: admitted.category,
    safety: FRAUD_PROOF_WORKFLOW_SAFETY,
    prepare: noFamilyAction,
    observe: async () => ({ kind: "completed", terminal }),
    preflight: noFamilyAction,
    submit: noFamilyAction,
  };
  return await runAdmittedFraudProofWorkflow({
    ...admitted,
    prepareFamilyArtifact: async () => ({}),
    registry: new Map([[admitted.category, completed]]),
    terminalVerifier: {
      verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
      verify: async () => terminal,
      verifyIncluded: async () => terminal,
    },
  });
};
