import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";

import {
  admittedPermits,
  admittedRevocationErrors,
  assertPermit,
  runPermits,
  WORKFLOW_ACTUATION_PERMIT,
  type WorkflowActuationCheckpoint,
  type WorkflowActuationPermit,
  WorkflowActuationRevokedError,
} from "./actuation-permit.create-workflow-reconciliation-permit-controller.js";
import type { HeaderFaultDecision } from "./header-classifier.js";
import { normalizeWorkflowJson, workflowJsonDigest } from "./identity.js";
import type { WorkflowRun } from "./workflow-run.js";

/**
 * Cross-check used by the watcher's supervisor, execution and progress
 * authority. It exposes only the already-public workflow identity, never a
 * permit minter or revocation state mutation, and structural permit
 * lookalikes remain rejected by the WeakMap.
 */
export const assertWorkflowActuationPermitIdentity = ({
  permit,
  category,
  rollbackGeneration,
}: {
  readonly permit: WorkflowActuationPermit;
  readonly category: FraudProofCatalogueCategoryName;
  readonly rollbackGeneration: string;
}): Readonly<{
  decisionDigest: string;
  executionDecisionDigest: string;
  launchScope: HeaderFaultDecision["launchScope"];
  hubOraclePolicyId: string;
  headerHash: string;
  authority: "submission" | "reconciliation";
}> => {
  const state = admittedPermits.get(permit);
  if (
    permit.permitVersion !== WORKFLOW_ACTUATION_PERMIT ||
    state === undefined
  ) {
    throw new Error("production workflow actuation permit was not admitted");
  }
  if (
    state.category !== category ||
    state.rollbackGeneration !== rollbackGeneration
  ) {
    throw new Error("production workflow actuation permit identity mismatch");
  }
  if (state.revokedReason !== undefined) {
    const error = new WorkflowActuationRevokedError({
      decisionDigest: state.decisionDigest,
      rollbackGeneration: state.rollbackGeneration,
      checkpoint: "runner_start",
      revocationReason: state.revokedReason,
    });
    admittedRevocationErrors.add(error);
    Object.freeze(error);
    throw error;
  }
  return Object.freeze({
    decisionDigest: state.decisionDigest,
    executionDecisionDigest: state.executionDecisionDigest,
    launchScope: state.decision.launchScope,
    hubOraclePolicyId: state.hubOraclePolicyId,
    headerHash: state.headerHash,
    authority: state.reconciliation ? "reconciliation" : "submission",
  });
};

/** Preserve an existing execution only when fresh classification proves the
 * identical fault. Historical envelopes never become runnable authorities. */
export const bindWorkflowActuationRecoveryIdentity = (input: {
  readonly permit: WorkflowActuationPermit;
  readonly category: FraudProofCatalogueCategoryName;
  readonly rollbackGeneration: string;
  readonly originalDecision: HeaderFaultDecision;
}): void => {
  assertWorkflowActuationPermitIdentity(input);
  const state = admittedPermits.get(input.permit)!;
  const original = input.originalDecision;
  const { decisionDigest, ...unsealed } = original;
  if (
    !/^[0-9a-f]{64}$/u.test(original.authenticatedObservationDigest) ||
    workflowJsonDigest(normalizeWorkflowJson(unsealed)) !== decisionDigest ||
    workflowJsonDigest(normalizeWorkflowJson(original)) !==
      workflowJsonDigest(
        normalizeWorkflowJson({
          ...state.decision,
          authenticatedObservationDigest:
            original.authenticatedObservationDigest,
          decisionDigest,
        }),
      )
  )
    throw new Error("workflow recovery changed the classified fault evidence");
  if (
    state.executionDecisionDigest !== state.decisionDigest &&
    state.executionDecisionDigest !== decisionDigest
  ) {
    throw new Error(
      "workflow recovery attempted to replace its execution identity",
    );
  }
  if (state.executionBound && state.executionDecisionDigest !== decisionDigest)
    throw new Error(
      "workflow recovery cannot change an execution after its run is bound",
    );
  state.executionDecisionDigest = decisionDigest;
};

/** Binds an opaque live permit to one invocation's run. */
export const bindWorkflowRunActuation = ({
  run,
  permit,
  decisionDigest,
  hubOraclePolicyId,
  category,
  headerHash,
}: {
  readonly run: WorkflowRun;
  readonly permit: WorkflowActuationPermit;
  readonly decisionDigest: string;
  readonly hubOraclePolicyId: string;
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
}): WorkflowRun => {
  assertPermit({
    permit,
    decisionDigest,
    hubOraclePolicyId,
    category,
    headerHash,
    checkpoint: "runner_start",
  });
  if (runPermits.has(run)) {
    throw new Error("production workflow run already has actuation authority");
  }
  admittedPermits.get(permit)!.executionBound = true;
  runPermits.set(
    run,
    Object.freeze({
      permit,
      decisionDigest,
      executionDecisionDigest:
        admittedPermits.get(permit)!.executionDecisionDigest,
      hubOraclePolicyId,
      category,
      headerHash,
    }),
  );
  return run;
};

export const workflowActuationDecisionDigest = (
  run: WorkflowRun,
): string | undefined => {
  return runPermits.get(run)?.executionDecisionDigest;
};

/** The current classified decision authorizes infrastructure configuration;
 * recovery retains a separate, original decision for the execution identity. */
export const workflowActuationAuthorizingDecisionDigest = (
  run: WorkflowRun,
): string | undefined => {
  return runPermits.get(run)?.decisionDigest;
};

/** Whether the run is bound to a permit over a fault that is no longer
 * live (`createWorkflowReconciliationPermitController`). */
export const workflowRunIsReconciliationOnly = (run: WorkflowRun): boolean => {
  const binding = runPermits.get(run);
  if (binding === undefined) return false;
  return admittedPermits.get(binding.permit)!.reconciliation;
};

/**
 * Shared checkpoint used by the orchestrator. A run bound to no permit is
 * retained only for lower-level tests and diagnostics; admitted production
 * runners always bind before loading runtime infrastructure.
 */
export const assertWorkflowRunActuation = ({
  run,
  hubOraclePolicyId,
  category,
  headerHash,
  checkpoint,
}: {
  readonly run: WorkflowRun;
  readonly hubOraclePolicyId: string;
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly checkpoint: WorkflowActuationCheckpoint;
}): void => {
  const binding = runPermits.get(run);
  if (binding === undefined) {
    return;
  }
  assertPermit({
    permit: binding.permit,
    decisionDigest: binding.decisionDigest,
    hubOraclePolicyId,
    category,
    headerHash,
    checkpoint,
  });
};
