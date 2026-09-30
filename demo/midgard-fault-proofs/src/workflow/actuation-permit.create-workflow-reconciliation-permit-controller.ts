import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import {
  type HeaderFaultDecision,
  requireRunnableHeaderFault,
} from "./header-classifier.js";
import {
  type FraudProofWorkflowJournalEntry,
  journalJsonDigest,
  normalizeJournalJson,
  validateFraudProofWorkflowJournal,
} from "./journal.js";

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
  restrictToReconciliation(reason: string): void;
  revoke(reason: string): void;
}>;

type PermitState = {
  readonly decision: HeaderFaultDecision;
  readonly decisionDigest: string;
  executionDecisionDigest: string;
  executionBound: boolean;
  readonly deploymentFingerprint: string;
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly rollbackGeneration: string;
  revokedReason: string | undefined;
  reconciliationReason: string | undefined;
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

export const admittedPermits = new WeakMap<object, PermitState>();

export const admittedRevocationErrors = new WeakSet<object>();

export const journalPermits = new WeakMap<
  object,
  Readonly<{
    permit: WorkflowActuationPermit;
    decisionDigest: string;
    executionDecisionDigest: string;
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

/** Supervisor lifecycle fences accept only the exact admitted opaque permit. */
export const revokeWorkflowActuationPermit = (
  permit: WorkflowActuationPermit,
  reason: string,
): void => {
  const state = admittedPermits.get(permit);
  if (state === undefined)
    throw new Error("production workflow actuation permit was not admitted");
  const admittedReason = canonicalReason(reason);
  state.revokedReason ??= admittedReason;
};

const createController = ({
  decision,
  rollbackGeneration,
  reconciliationReason,
}: {
  readonly decision: HeaderFaultDecision;
  readonly rollbackGeneration: string;
  readonly reconciliationReason?: string;
}): WorkflowActuationPermitController => {
  const admitted =
    reconciliationReason === undefined
      ? requireRunnableHeaderFault(decision)
      : decision;
  if (!CANONICAL_NATURAL.test(rollbackGeneration)) {
    throw new Error(
      "actuation permit rollback generation must be a canonical natural",
    );
  }
  const permit: WorkflowActuationPermit = Object.freeze({
    permitVersion: WORKFLOW_ACTUATION_PERMIT,
  });
  const state: PermitState = {
    decision: admitted,
    executionDecisionDigest: admitted.decisionDigest,
    executionBound: false,
    decisionDigest: admitted.decisionDigest,
    deploymentFingerprint: admitted.deploymentFingerprint,
    category: admitted.category,
    headerHash: admitted.headerHash,
    rollbackGeneration,
    revokedReason: undefined,
    reconciliationReason,
  };
  admittedPermits.set(permit, state);
  return Object.freeze({
    permit,
    restrictToReconciliation: (reason: string): void => {
      state.reconciliationReason ??= canonicalReason(reason);
    },
    revoke: (reason: string): void => {
      revokeWorkflowActuationPermit(permit, reason);
    },
  });
};

export const createWorkflowActuationPermitController = (input: {
  readonly decision: HeaderFaultDecision;
  readonly rollbackGeneration: string;
}): WorkflowActuationPermitController => createController(input);

/** Mint's central journal records its exact family evidence identity rather
 * than the generic orchestrator envelope. The sealed decision pins the header
 * and payload; its detection coordinate and every durable attempt must agree
 * with this recorded identity. Callers must validate the full journal and its
 * identity against the sealed decision first. This never grants classification. */
export const assertMintWorkflowPreparedEvidence = (
  decision: HeaderFaultDecision,
  entries: readonly FraudProofWorkflowJournalEntry[],
): void => {
  const prepared = entries[1];
  if (prepared?.event.kind !== "prepared")
    throw new Error("mint reconciliation requires prepared evidence");
  const artifact = prepared.event.artifact;
  const familyIdentity = artifact.familyIdentity;
  const coordinate =
    typeof familyIdentity === "string"
      ? /^([0-9a-f]{64}):[01]:(0|[1-9][0-9]*):[0-9a-f]{64}:[0-9a-f]{64}$/u.exec(
          familyIdentity,
        )
      : null;
  if (
    Object.keys(artifact).length !== 2 ||
    artifact.category !== "mintItemNonCanonical" ||
    coordinate === null ||
    !CANONICAL_NATURAL.test(decision.position) ||
    decision.violationId !== "mint-item-non-canonical" ||
    decision.detectionId !==
      `mint-item-non-canonical:${decision.position}:${coordinate[1]}:${coordinate[2]}` ||
    entries.some(
      ({ event }) =>
        event.kind === "submission_intent" &&
        (event.durableRecovery?.familyIdentity !== familyIdentity ||
          event.actionInput.familyIdentity !== familyIdentity),
    )
  )
    throw new Error(
      "reconciliation authority changed its prepared mint fault evidence",
    );
};

/** Historical classification grants only observation of an already signed
 * execution. It never regains runnable classification or submission authority. */
export const createWorkflowReconciliationPermitController = (input: {
  readonly decision: HeaderFaultDecision;
  readonly deploymentFingerprint: string;
  readonly rollbackGeneration: string;
  readonly entries: readonly FraudProofWorkflowJournalEntry[];
}): WorkflowActuationPermitController => {
  const { decisionDigest, ...unsealed } = input.decision;
  const first = input.entries[0];
  const prepared = input.entries[1];
  if (
    first === undefined ||
    prepared?.event.kind !== "prepared" ||
    !input.entries.some(({ event }) => event.kind === "submission_intent") ||
    input.entries.some(({ event }) => event.kind === "completed") ||
    input.decision.decision !== "fault_detected" ||
    journalJsonDigest(normalizeJournalJson(unsealed)) !== decisionDigest ||
    input.decision.deploymentFingerprint !== input.deploymentFingerprint ||
    first.identity.deploymentFingerprint !== input.deploymentFingerprint ||
    first.identity.decisionDigest !== decisionDigest ||
    first.identity.category !== input.decision.category ||
    first.identity.target.kind !== "state_queue_header" ||
    first.identity.target.headerHash !== input.decision.headerHash
  )
    throw new Error(
      "reconciliation authority requires an exact existing signed workflow",
    );
  validateFraudProofWorkflowJournal({
    workflowId: first.workflowId,
    entries: input.entries,
    expectedIdentity: first.identity,
  });
  if (input.decision.category === "mintItemNonCanonical") {
    assertMintWorkflowPreparedEvidence(input.decision, input.entries);
  } else {
    const binding = prepared.event.artifact.evidenceBinding;
    if (
      typeof binding !== "object" ||
      binding === null ||
      Array.isArray(binding) ||
      !("headerHash" in binding) ||
      !("payloadSha256" in binding) ||
      binding.headerHash !== input.decision.headerHash ||
      binding.payloadSha256 !== input.decision.payloadSha256
    )
      throw new Error(
        "reconciliation authority changed its prepared fault evidence",
      );
  }
  return createController({
    ...input,
    reconciliationReason: "existing_signed_workflow_only",
  });
};

export const assertPermit = ({
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
  const revokedReason =
    state.revokedReason ??
    (checkpoint === "before_preflight" || checkpoint === "before_submit"
      ? state.reconciliationReason
      : undefined);
  if (revokedReason !== undefined) {
    const error = new WorkflowActuationRevokedError({
      decisionDigest: state.decisionDigest,
      rollbackGeneration: state.rollbackGeneration,
      checkpoint,
      revocationReason: revokedReason,
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
