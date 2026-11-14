import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import {
  admittedPermits,
  admittedRevocationErrors,
  assertPermit,
  journalPermits,
  WORKFLOW_ACTUATION_PERMIT,
  type WorkflowActuationCheckpoint,
  type WorkflowActuationPermit,
  WorkflowActuationRevokedError,
} from "./actuation-permit.create-workflow-reconciliation-permit-controller.js";
import { type HeaderFaultDecision } from "./header-classifier.js";
import {
  type FraudProofWorkflowJournalStore,
  journalJsonDigest,
  normalizeJournalJson,
} from "./journal.js";

/**
 * Cross-check used by the funding reservation controller. It exposes only the
 * already-public workflow identity, never a permit minter or revocation state
 * mutation, and structural permit lookalikes remain rejected by the WeakMap.
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
  deploymentFingerprint: string;
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
    deploymentFingerprint: state.deploymentFingerprint,
    headerHash: state.headerHash,
    authority:
      state.reconciliationReason === undefined
        ? "submission"
        : "reconciliation",
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
    journalJsonDigest(normalizeJournalJson(unsealed)) !== decisionDigest ||
    journalJsonDigest(normalizeJournalJson(original)) !==
      journalJsonDigest(
        normalizeJournalJson({
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
      "workflow recovery cannot change an execution after journal binding",
    );
  state.executionDecisionDigest = decisionDigest;
};

/** Bind an opaque live permit to the exact journal object passed downstream. */
export const bindWorkflowActuationJournal = <
  Journal extends FraudProofWorkflowJournalStore,
>({
  journal,
  permit,
  decisionDigest,
  deploymentFingerprint,
  category,
  headerHash,
}: {
  readonly journal: Journal;
  readonly permit: WorkflowActuationPermit;
  readonly decisionDigest: string;
  readonly deploymentFingerprint: string;
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
}): Journal => {
  assertPermit({
    permit,
    decisionDigest,
    deploymentFingerprint,
    category,
    headerHash,
    checkpoint: "runner_start",
  });
  if (journalPermits.has(journal)) {
    throw new Error(
      "production workflow journal already has actuation authority",
    );
  }
  admittedPermits.get(permit)!.executionBound = true;
  journalPermits.set(
    journal,
    Object.freeze({
      permit,
      decisionDigest,
      executionDecisionDigest:
        admittedPermits.get(permit)!.executionDecisionDigest,
      deploymentFingerprint,
      category,
      headerHash,
    }),
  );
  return journal;
};

export const workflowActuationDecisionDigest = (
  journal: FraudProofWorkflowJournalStore,
): string | undefined => {
  return journalPermits.get(journal)?.executionDecisionDigest;
};

/** The current classified decision authorizes infrastructure configuration;
 * recovery retains a separate, original decision for durable execution identity. */
export const workflowActuationAuthorizingDecisionDigest = (
  journal: FraudProofWorkflowJournalStore,
): string | undefined => {
  return journalPermits.get(journal)?.decisionDigest;
};

export const workflowActuationPermitIsReconciliationOnly = (
  permit: WorkflowActuationPermit,
): boolean => {
  const state = admittedPermits.get(permit);
  if (state === undefined)
    throw new Error("production workflow actuation permit was not admitted");
  assertPermit({
    permit,
    decisionDigest: state.decisionDigest,
    deploymentFingerprint: state.deploymentFingerprint,
    category: state.category,
    headerHash: state.headerHash,
    checkpoint: "runner_start",
  });
  return state.reconciliationReason !== undefined;
};

/** Recovery-only authority never becomes permission to build or broadcast. */
export const workflowJournalIsReconciliationOnly = (
  journal: FraudProofWorkflowJournalStore,
): boolean => {
  const binding = journalPermits.get(journal);
  if (binding === undefined) return false;
  return (
    admittedPermits.get(binding.permit)!.reconciliationReason !== undefined
  );
};

/**
 * Shared checkpoint used by the orchestrator. An unbound journal is retained
 * only for lower-level tests/diagnostics; admitted production runners always
 * bind before loading runtime infrastructure.
 */
export const assertWorkflowJournalActuation = ({
  journal,
  deploymentFingerprint,
  category,
  headerHash,
  checkpoint,
}: {
  readonly journal: FraudProofWorkflowJournalStore;
  readonly deploymentFingerprint: string;
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly checkpoint: WorkflowActuationCheckpoint;
}): void => {
  const binding = journalPermits.get(journal);
  if (binding === undefined) {
    return;
  }
  assertPermit({
    permit: binding.permit,
    decisionDigest: binding.decisionDigest,
    deploymentFingerprint,
    category,
    headerHash,
    checkpoint,
  });
};
