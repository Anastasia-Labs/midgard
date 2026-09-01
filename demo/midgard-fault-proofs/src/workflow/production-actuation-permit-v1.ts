import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import type { FraudProofWorkflowJournalStoreV1 } from "./journal-v1.js";
import {
  type ProductionHeaderFaultDecisionV1,
  requireRunnableProductionHeaderFaultV1,
} from "./production-header-classifier-v1.js";

export const PRODUCTION_WORKFLOW_ACTUATION_PERMIT_V1 =
  "midgard-production-workflow-actuation-permit-v1" as const;

export type ProductionWorkflowActuationCheckpointV1 =
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
export interface ProductionWorkflowActuationPermitV1 {
  readonly permitVersion: typeof PRODUCTION_WORKFLOW_ACTUATION_PERMIT_V1;
}

export type ProductionWorkflowActuationPermitControllerV1 = Readonly<{
  permit: ProductionWorkflowActuationPermitV1;
  revoke(reason: string): void;
}>;

type PermitStateV1 = {
  readonly decisionDigest: string;
  readonly deploymentFingerprint: string;
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly rollbackGeneration: string;
  revokedReason: string | undefined;
};

export class ProductionWorkflowActuationRevokedErrorV1 extends Error {
  readonly decisionDigest: string;
  readonly rollbackGeneration: string;
  readonly checkpoint: ProductionWorkflowActuationCheckpointV1;
  readonly revocationReason: string;

  constructor(input: {
    readonly decisionDigest: string;
    readonly rollbackGeneration: string;
    readonly checkpoint: ProductionWorkflowActuationCheckpointV1;
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
const admittedPermits = new WeakMap<object, PermitStateV1>();
const admittedRevocationErrors = new WeakSet<object>();
const journalPermits = new WeakMap<
  object,
  Readonly<{
    permit: ProductionWorkflowActuationPermitV1;
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

export const createProductionWorkflowActuationPermitControllerV1 = ({
  decision,
  rollbackGeneration,
}: {
  readonly decision: ProductionHeaderFaultDecisionV1;
  readonly rollbackGeneration: string;
}): ProductionWorkflowActuationPermitControllerV1 => {
  const admitted = requireRunnableProductionHeaderFaultV1(decision);
  if (!CANONICAL_NATURAL.test(rollbackGeneration)) {
    throw new Error(
      "actuation permit rollback generation must be a canonical natural",
    );
  }
  const permit: ProductionWorkflowActuationPermitV1 = Object.freeze({
    permitVersion: PRODUCTION_WORKFLOW_ACTUATION_PERMIT_V1,
  });
  const state: PermitStateV1 = {
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
  readonly permit: ProductionWorkflowActuationPermitV1;
  readonly decisionDigest: string;
  readonly deploymentFingerprint: string;
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly checkpoint: ProductionWorkflowActuationCheckpointV1;
}): PermitStateV1 => {
  const state = admittedPermits.get(permit);
  if (
    permit.permitVersion !== PRODUCTION_WORKFLOW_ACTUATION_PERMIT_V1 ||
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
    const error = new ProductionWorkflowActuationRevokedErrorV1({
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

export const isProductionWorkflowActuationRevokedErrorV1 = (
  error: unknown,
): error is ProductionWorkflowActuationRevokedErrorV1 =>
  typeof error === "object" &&
  error !== null &&
  admittedRevocationErrors.has(error);

/**
 * Cross-check used by the funding reservation controller. It exposes only the
 * already-public workflow identity, never a permit minter or revocation state
 * mutation, and structural permit lookalikes remain rejected by the WeakMap.
 */
export const assertProductionWorkflowActuationPermitIdentityV1 = ({
  permit,
  category,
  rollbackGeneration,
}: {
  readonly permit: ProductionWorkflowActuationPermitV1;
  readonly category: FraudProofCatalogueCategoryName;
  readonly rollbackGeneration: string;
}): Readonly<{
  decisionDigest: string;
  deploymentFingerprint: string;
  headerHash: string;
}> => {
  const state = admittedPermits.get(permit);
  if (
    permit.permitVersion !== PRODUCTION_WORKFLOW_ACTUATION_PERMIT_V1 ||
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
    const error = new ProductionWorkflowActuationRevokedErrorV1({
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
export const bindProductionWorkflowActuationJournalV1 = <
  Journal extends FraudProofWorkflowJournalStoreV1,
>({
  journal,
  permit,
  decisionDigest,
  deploymentFingerprint,
  category,
  headerHash,
}: {
  readonly journal: Journal;
  readonly permit: ProductionWorkflowActuationPermitV1;
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

export const productionWorkflowActuationDecisionDigestV1 = (
  journal: FraudProofWorkflowJournalStoreV1,
): string | undefined => journalPermits.get(journal)?.decisionDigest;

/**
 * Shared checkpoint used by the orchestrator. An unbound journal is retained
 * only for lower-level tests/diagnostics; admitted production runners always
 * bind before loading runtime infrastructure.
 */
export const assertProductionWorkflowJournalActuationV1 = ({
  journal,
  deploymentFingerprint,
  category,
  headerHash,
  checkpoint,
}: {
  readonly journal: FraudProofWorkflowJournalStoreV1;
  readonly deploymentFingerprint: string;
  readonly category: FraudProofCatalogueCategoryName;
  readonly headerHash: string;
  readonly checkpoint: ProductionWorkflowActuationCheckpointV1;
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
