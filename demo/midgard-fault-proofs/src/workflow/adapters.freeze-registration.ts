import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import type { WorkflowActuationPermit } from "./actuation-permit.js";
import type { WorkflowFundingReservationPermit } from "./funding-reservation-permit.js";
import { WORKFLOW_ADAPTER_RUNNER } from "./runner-admission.js";

export const WORKFLOW_ADAPTER_REGISTRY_SCHEMA_VERSION =
  "midgard-production-fraud-proof-workflow-adapters-v1" as const;

export const WORKFLOW_APPLICATION_REGISTRY_SCHEMA_VERSION =
  "midgard-production-fraud-proof-application-registry-v1" as const;

/** Permit-free identity/configuration used only for startup readiness. */
export type WorkflowAdapterReadinessInput = {
  readonly mode: "run" | "resume";
  readonly category: FraudProofCatalogueCategoryName;
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly journalDirectory: string;
  /** Versioned infrastructure configuration only; never proof evidence. */
  readonly runtimeConfigPath: string;
};

export type WorkflowAdapterRunnerInput = WorkflowAdapterReadinessInput & {
  /** Exact admitted classifier decision; forms part of the durable run key. */
  readonly decisionDigest: string;
  /** Live, revocable authority for this decision and rollback generation. */
  readonly actuationPermit: WorkflowActuationPermit;
  /** Durable, exact-input funding authority for this decision/generation. */
  readonly fundingReservationPermit: WorkflowFundingReservationPermit;
};

export type WorkflowAdapterRunner = {
  readonly runnerVersion: typeof WORKFLOW_ADAPTER_RUNNER;
  readonly runOrResume: (input: WorkflowAdapterRunnerInput) => Promise<unknown>;
};

export type MissingWorkflowAdapterReason =
  | "manual_step_chain_has_no_atomic_driver"
  | "one_shot_prover_has_no_pre_submit_journal_hook"
  | "prover_is_not_chain_state_resumable"
  | "partial_resume_surface_has_no_complete_driver"
  | "detector_or_scanner_only"
  | "constrained_adapter_is_not_launch_scope_complete";

export type MissingWorkflowAdapterRegistration = {
  readonly category: FraudProofCatalogueCategoryName;
  readonly status: "missing";
  readonly reason: MissingWorkflowAdapterReason;
  readonly existingSurface: readonly string[];
  readonly requiredClosure: string;
};

export type ReadyWorkflowAdapterRegistration = {
  readonly category: FraudProofCatalogueCategoryName;
  readonly status: "ready";
  readonly adapterVersion: string;
  readonly runner: WorkflowAdapterRunner;
  readonly existingSurface: readonly string[];
  readonly guarantees: readonly string[];
};

export type WorkflowAdapterRegistration =
  | ReadyWorkflowAdapterRegistration
  | MissingWorkflowAdapterRegistration;

export type WorkflowApplicationRunnerInstallation = Readonly<{
  readonly category: FraudProofCatalogueCategoryName;
  readonly deploymentFingerprint: string;
  readonly runner: WorkflowAdapterRunner;
}>;

export type WorkflowApplicationRegistry = Readonly<{
  readonly schemaVersion: typeof WORKFLOW_APPLICATION_REGISTRY_SCHEMA_VERSION;
  readonly deploymentFingerprint: string;
  readonly installedCategories: readonly FraudProofCatalogueCategoryName[];
  /** Exact full-catalogue overlay; static rows remain unchanged. */
  readonly registrations: readonly WorkflowAdapterRegistration[];
}>;

export const manual = (
  category: FraudProofCatalogueCategoryName,
  existingSurface: readonly string[],
): WorkflowAdapterRegistration => ({
  category,
  status: "missing",
  reason: "manual_step_chain_has_no_atomic_driver",
  existingSurface,
  requiredClosure:
    "add a chain-state cursor plus build/local-evaluate, pre-submit-intent, submit, and authenticated reconcile hooks for every transaction",
});

export const freezeRegistration = (
  registration: WorkflowAdapterRegistration,
): WorkflowAdapterRegistration =>
  registration.status === "ready"
    ? Object.freeze({
        ...registration,
        existingSurface: Object.freeze([...registration.existingSurface]),
        guarantees: Object.freeze([...registration.guarantees]),
      })
    : Object.freeze({
        ...registration,
        existingSurface: Object.freeze([...registration.existingSurface]),
      });
