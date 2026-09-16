import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import {
  type HeaderFaultDecision,
  requireRunnableHeaderFault,
} from "./header-classifier.js";
import {
  type FraudProofWorkflowJournalEntry,
  type FraudProofWorkflowJournalStore,
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
const admittedPermits = new WeakMap<object, PermitState>();
const admittedRevocationErrors = new WeakSet<object>();
const journalPermits = new WeakMap<
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
