import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import type { FraudProofWorkflowJournalStore } from "./journal-v1.js";
import {
  type HeaderFaultDecision,
  requireRunnableHeaderFault,
} from "./production-header-classifier-v1.js";

export const WORKFLOW_ACTUATION_PERMIT =
  "midgard-production-workflow-actuation-permit-v1" as const;

export type WorkflowActuationCheckpoint =
  | "runner_start"
  | "workflow_resume"
  | "before_observe"
  | "before_preflight"
  | "before_submit"
  | "before_reconcile"
  | "before_terminal_verify";

/**
 * Opaque, live authority to actuate one classified fault under one rollback
 * generation. Structural lookalikes are rejected by module-private admission.
 */
export interface WorkflowActuationPermit {
  readonly permitVersion: typeof WORKFLOW_ACTUATION_PERMIT;
}

export type WorkflowActuationPermitController = Readonly<{
  permit: WorkflowActuationPermit;
  revoke(reason: string): void;
}>;

type PermitState = {
  readonly decisionDigest: string;
  readonly deploymentFingerprint: string;
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly rollbackGeneration: string;
  revokedReason: string | undefined;
};

export class WorkflowActuationRevokedError extends Error {
  readonly decisionDigest: string;
  readonly rollbackGeneration: string;
  readonly checkpoint: WorkflowActuationCheckpoint;
  readonly revocationReason: string;

  constructor(input: {
    readonly decisionDigest: string;
    readonly rollbackGeneration: string;
    readonly checkpoint: WorkflowActuationCheckpoint;
    readonly revocationReason: string;
  }) {
    super(
      `production workflow actuation revoked before ${input.checkpoint}: ${input.revocationReason}`,
    );
    this.name = "ProductionWorkflowActuationRevokedErrorV1";
    this.decisionDigest = input.decisionDigest;
    this.rollbackGeneration = input.rollbackGeneration;
    this.checkpoint = input.checkpoint;
    this.revocationReason = input.revocationReason;
  }
}

const CANONICAL_NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const admittedPermits = new WeakMap<object, PermitState>();
const admittedRevocationErrors = new WeakSet<object>();
const journalPermits = new WeakMap<
  object,
  Readonly<{
    permit: WorkflowActuationPermit;
    decisionDigest: string;
    deploymentFingerprint: string;
    category: FraudProofCatalogueCategoryName;
    headerHash: string;
  }>
>();

const canonicalReason = (reason: string): string => {
  if (reason.length === 0 || reason.trim() !== reason) {
    throw new Error("actuation permit revocation reason must be canonical");
  }
  return reason;
};

export const createWorkflowActuationPermitController = ({
  decision,
  rollbackGeneration,
}: {
  readonly decision: HeaderFaultDecision;
  readonly rollbackGeneration: string;
}): WorkflowActuationPermitController => {
  const admitted = requireRunnableHeaderFault(decision);
  if (!CANONICAL_NATURAL.test(rollbackGeneration)) {
    throw new Error(
      "actuation permit rollback generation must be a canonical natural",
    );
  }
  const permit: WorkflowActuationPermit = Object.freeze({
    permitVersion: WORKFLOW_ACTUATION_PERMIT,
  });
  const state: PermitState = {
    decisionDigest: admitted.decisionDigest,
    deploymentFingerprint: admitted.deploymentFingerprint,
    category: admitted.category,
    headerHash: admitted.headerHash,
    rollbackGeneration,
    revokedReason: undefined,
  };
  admittedPermits.set(permit, state);
  return Object.freeze({
    permit,
    revoke: (reason: string): void => {
      const admittedReason = canonicalReason(reason);
      if (state.revokedReason === undefined) {
        state.revokedReason = admittedReason;
      }
    },
  });
};

const assertPermit = ({
  permit,
  decisionDigest,
  deploymentFingerprint,
  category,
  headerHash,
  checkpoint,
}: {
  readonly permit: WorkflowActuationPermit;
  readonly decisionDigest: string;
  readonly deploymentFingerprint: string;
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly checkpoint: WorkflowActuationCheckpoint;
}): PermitState => {
  const state = admittedPermits.get(permit);
  if (
    permit.permitVersion !== WORKFLOW_ACTUATION_PERMIT ||
    state === undefined
  ) {
    throw new Error("production workflow actuation permit was not admitted");
  }
  if (
    state.decisionDigest !== decisionDigest ||
    state.deploymentFingerprint !== deploymentFingerprint ||
    state.category !== category ||
    state.headerHash !== headerHash
  ) {
    throw new Error(
      `production workflow actuation permit identity mismatch at ${checkpoint}`,
    );
  }
  if (state.revokedReason !== undefined) {
    const error = new WorkflowActuationRevokedError({
      decisionDigest: state.decisionDigest,
      rollbackGeneration: state.rollbackGeneration,
      checkpoint,
      revocationReason: state.revokedReason,
    });
    admittedRevocationErrors.add(error);
    Object.freeze(error);
    throw error;
  }
  return state;
};

export const isWorkflowActuationRevokedError = (
  error: unknown,
): error is WorkflowActuationRevokedError =>
  typeof error === "object" &&
  error !== null &&
  admittedRevocationErrors.has(error);

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
  deploymentFingerprint: string;
  headerHash: string;
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
    deploymentFingerprint: state.deploymentFingerprint,
    headerHash: state.headerHash,
  });
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
  journalPermits.set(
    journal,
    Object.freeze({
      permit,
      decisionDigest,
      deploymentFingerprint,
      category,
      headerHash,
    }),
  );
  return journal;
};

export const workflowActuationDecisionDigest = (
  journal: FraudProofWorkflowJournalStore,
): string | undefined => journalPermits.get(journal)?.decisionDigest;

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
