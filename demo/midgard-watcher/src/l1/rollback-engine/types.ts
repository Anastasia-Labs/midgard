import { type DeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  type WatcherDurableAtomicBackend,
  type WatcherDurableStore,
  watcherSha256CanonicalJson,
} from "../../storage/durable-store.js";
import {
  type WatcherUserEventCheckpoint,
  type WatcherUserEventValidation,
} from "../../storage/user-event-checkpoint.js";
import {
  type WatcherFinalityBoundObservation,
  type WatcherFinalityIncident,
  type WatcherFinalityPolicy,
  type WatcherFinalityResult,
  type WatcherFinalityRewindInstruction,
  type WatcherFinalityState,
} from ".././finality-engine.js";
import { type WatcherMultiProviderConsistency } from ".././multi-provider-consistency.js";

export const WATCHER_ROLLBACK_STATE_SCHEMA_VERSION =
  "midgard-watcher-rollback-state-v1" as const;
export const WATCHER_ROLLBACK_INCIDENT_SCHEMA_VERSION =
  "midgard-watcher-rollback-incident-v1" as const;
export const WATCHER_ROLLBACK_RESULT_SCHEMA_VERSION =
  "midgard-watcher-rollback-result-v1" as const;
export const WATCHER_ROLLBACK_TRANSITION_SCHEMA_VERSION =
  "midgard-watcher-rollback-transition-v1" as const;
export const WATCHER_ROLLBACK_EPOCH_CHECKPOINT_SCHEMA_VERSION =
  "midgard-watcher-rollback-epoch-checkpoint-v1" as const;
export const WATCHER_ROLLBACK_DURABLE_AUTHORITY_SCHEMA_VERSION =
  "midgard-watcher-rollback-durable-authority-v1" as const;
// Advance this binding whenever persisted validation semantics change. Policy,
// deployment, blueprint and authentication-key dependencies are bound separately.
export const ROLLBACK_VALIDATION_SCHEMA_VERSION =
  "midgard-watcher-rollback-validation-v1" as const;
export const WATCHER_ROLLBACK_DURABLE_AUTHORITY_HANDLE_SCHEMA_VERSION =
  "midgard-watcher-rollback-durable-authority-handle-v1" as const;
export const WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION =
  "midgard-watcher-rollback-durable-trusted-head-v1" as const;
export const WATCHER_POST_FINALITY_RECOVERY_STATE_SCHEMA_VERSION =
  "midgard-watcher-post-finality-recovery-state-v1" as const;
export const WATCHER_POST_FINALITY_RECOVERY_RESULT_SCHEMA_VERSION =
  "midgard-watcher-post-finality-recovery-result-v1" as const;
export const WATCHER_ROLLBACK_BOUNDS = Object.freeze({
  transitionHistory: 128,
  postFinalityRecoveryDepth: 2_160n,
});

export const HEX_32 = /^[0-9a-f]{64}$/u;
export const ROLLBACK_AUTHORITY_KEY_BYTES = 32;
export const CANONICAL_NATURAL = /^(?:0|[1-9][0-9]*)$/u;
export const NETWORKS = ["Mainnet", "Preprod", "Preview", "Custom"] as const;
export const isNetwork = (value: unknown): value is (typeof NETWORKS)[number] =>
  typeof value === "string" &&
  NETWORKS.includes(value as (typeof NETWORKS)[number]);

export const WATCHER_ROLLBACK_REASON_CODES = [
  "rewind_applied",
  "duplicate_instruction",
  "rewind_already_applied",
  "post_finality_incident",
  "malformed_policy",
  "malformed_store",
  "malformed_previous_finality_state",
  "malformed_finality_result",
  "malformed_rollback_state",
  "deployment_mismatch",
  "blueprint_mismatch",
  "network_mismatch",
  "policy_mismatch",
  "stale_finality_state",
  "finality_provenance_mismatch",
  "invalid_finality_transition",
  "replacement_point_missing",
  "replacement_evidence_missing",
  "consistency_evidence_missing",
  "unknown_rewind_target",
  "rollback_state_store_mismatch",
  "state_quarantined",
] as const;

export const WATCHER_ROLLBACK_ALERT_CODES = [
  "watcher_rollback_rewind_applied",
  "watcher_rollback_input_rejected",
  "watcher_rollback_configuration_mismatch",
  "watcher_rollback_state_rejected",
  "watcher_rollback_post_finality_incident",
  "watcher_rollback_quarantined",
] as const;

export type WatcherRollbackReasonCode =
  (typeof WATCHER_ROLLBACK_REASON_CODES)[number];
export type WatcherRollbackAlertCode =
  (typeof WATCHER_ROLLBACK_ALERT_CODES)[number];

export type WatcherRollbackRemovedRecords = Readonly<{
  l1ObservationIds: readonly string[];
  chainPointIds: readonly string[];
  protocolUtxoOutRefs: readonly string[];
  daProofInputIds: readonly string[];
  reconstructedBlockHashes: readonly string[];
  decisionBlockHashes: readonly string[];
  faultIds: readonly string[];
  submissionIds: readonly string[];
  confirmationIds: readonly string[];
  retryIds: readonly string[];
  deadlineIds: readonly string[];
  correctionResultIds: readonly string[];
}>;

export type WatcherRollbackIncident = Readonly<{
  schemaVersion: typeof WATCHER_ROLLBACK_INCIDENT_SCHEMA_VERSION;
  reasonCode: WatcherFinalityIncident["reasonCode"];
  policyDigest: string;
  blueprintHash: string;
  deploymentMarker: DeploymentMarker;
  finalityStateDigest: string;
  finalityIncidentDigest: string;
  triggerConsistencyDigest: string | null;
  finalizedBinding: WatcherFinalityBoundObservation;
  sourceStoreDigest: string;
  nextStoreDigest: string;
  transitionCount: string;
  previousFinalityStateDigest: string;
  consistencyDigest: string;
  finalityResultDigest: string;
  incidentDigest: string;
}>;

export type WatcherRollbackEpochCheckpoint = Readonly<{
  operation: "compaction" | "observation" | "canonical_progress" | "recovery";
  schemaVersion: typeof WATCHER_ROLLBACK_EPOCH_CHECKPOINT_SCHEMA_VERSION;
  epoch: string;
  rootBootstrapStateDigest: string;
  priorCheckpointDigest: string | null;
  priorTerminalStateDigest: string;
  priorTerminalTransitionCount: string;
  priorTerminalTransitionLineageDigest: string;
  priorTerminalStoreDigest: string;
  priorTerminalFinalityStateDigest: string;
  priorTerminalIncidentDigest: string | null;
  recoveryStateDigest: string | null;
  recoveryLifecycleDigest: string | null;
  checkpointStoreDigest: string;
  checkpointFinalityStateDigest: string;
  checkpointDigest: string;
}>;

export type WatcherRollbackState = Readonly<{
  schemaVersion: typeof WATCHER_ROLLBACK_STATE_SCHEMA_VERSION;
  policyDigest: string;
  network: (typeof NETWORKS)[number];
  blueprintHash: string;
  deploymentMarker: DeploymentMarker;
  bootstrapStore: WatcherDurableStore;
  bootstrapFinalityState: WatcherFinalityState;
  epoch: string;
  epochCheckpoint: WatcherRollbackEpochCheckpoint | null;
  transitions: readonly WatcherRollbackTransition[];
  storeDigest: string;
  transitionCount: string;
  currentFinalityStateDigest: string | null;
  lastPreviousFinalityStateDigest: string | null;
  lastConsistencyDigest: string | null;
  lastFinalityResultDigest: string | null;
  lastInstructionDigest: string | null;
  transitionLineageDigest: string;
  incident: WatcherRollbackIncident | null;
  stateDigest: string;
}>;

export type WatcherRollbackTransition = Readonly<{
  schemaVersion: typeof WATCHER_ROLLBACK_TRANSITION_SCHEMA_VERSION;
  previousFinalityState: WatcherFinalityState;
  consistency: WatcherMultiProviderConsistency;
  finalityResult: WatcherFinalityResult;
  transitionDigest: string;
}>;

export type WatcherRollbackStateVerificationContext = Readonly<{
  policy: unknown;
  rollbackBootstrapState: unknown;
  trustedCheckpointAuthority?: unknown;
  currentStore: unknown;
  transportAttestations?: unknown;
}>;

export type WatcherRollbackResult = Readonly<{
  schemaVersion: typeof WATCHER_ROLLBACK_RESULT_SCHEMA_VERSION;
  action:
    | "apply_rewind"
    | "duplicate_rewind"
    | "quarantine_incident"
    | "reject";
  protocolDecision: "resume_pending" | "hold" | "quarantined";
  reasonCodes: readonly WatcherRollbackReasonCode[];
  alertCodes: readonly WatcherRollbackAlertCode[];
  sourceRevision: string | null;
  nextRevision: string | null;
  instructionDigest: string | null;
  sourceStoreDigest: string | null;
  nextStoreDigest: string | null;
  removedRecords: WatcherRollbackRemovedRecords;
  nextStore: WatcherDurableStore | null;
  rollbackState: WatcherRollbackState | null;
  rollbackBootstrapState: WatcherRollbackState | null;
  trustedCheckpointStateDigest: string | null;
  resultDigest: string;
}>;

export const WATCHER_POST_FINALITY_RECOVERY_REASON_CODES = [
  "recovery_applied",
  "duplicate_recovery",
  "malformed_policy",
  "malformed_store",
  "malformed_rollback_state",
  "rollback_state_store_mismatch",
  "incident_required",
  "malformed_recovery_state",
  "recovery_state_mismatch",
  "canonical_agreement_required",
  "recovery_path_malformed",
  "recovery_path_gap",
  "common_ancestor_mismatch",
  "finalized_binding_mismatch",
  "incident_provenance_mismatch",
  "recovery_depth_exceeded",
  "unknown_recovery_target",
] as const;

export type WatcherPostFinalityRecoveryReasonCode =
  (typeof WATCHER_POST_FINALITY_RECOVERY_REASON_CODES)[number];

export type WatcherPostFinalityRecoveryPath = Readonly<{
  commonAncestorPointDigest: string;
  commonAncestorBlockHash: string;
  commonAncestorBlockNo: string;
  orphanedFinalizedPointDigest: string;
  orphanedFinalizedBlockHash: string;
  replacementTipPointDigest: string;
  replacementTipBlockHash: string;
  replacementTipBlockNo: string;
  rollbackDepth: string;
  previousConsistencyDigests: readonly string[];
  replacementConsistencyDigests: readonly string[];
  pathDigest: string;
}>;

export type WatcherPostFinalityRecoveryIncidentLifecycle = Readonly<{
  detectedIncidentDigest: string;
  detectedReasonCode: WatcherFinalityIncident["reasonCode"];
  detectedTriggerConsistencyDigest: string | null;
  detectedFinalityStateDigest: string;
  detectedStoreDigest: string;
  status: "recovered";
  recoveryPathDigest: string;
  recoveredStoreDigest: string;
  resumableFinalityStateDigest: string;
  lifecycleDigest: string;
}>;

export type WatcherPostFinalityRecoveryState = Readonly<{
  schemaVersion: typeof WATCHER_POST_FINALITY_RECOVERY_STATE_SCHEMA_VERSION;
  policyDigest: string;
  network: (typeof NETWORKS)[number];
  blueprintHash: string;
  deploymentMarker: DeploymentMarker;
  sourceRollbackStateDigest: string;
  sourceStoreDigest: string;
  nextStoreDigest: string;
  path: WatcherPostFinalityRecoveryPath;
  removedRecords: WatcherRollbackRemovedRecords;
  resumableFinalityState: WatcherFinalityState;
  incidentLifecycle: WatcherPostFinalityRecoveryIncidentLifecycle;
  stateDigest: string;
}>;

export type WatcherPostFinalityRecoveryResult = Readonly<{
  schemaVersion: typeof WATCHER_POST_FINALITY_RECOVERY_RESULT_SCHEMA_VERSION;
  action: "rewind_and_replay" | "duplicate_recovery" | "reject";
  protocolDecision: "resume_replay" | "hold" | "quarantined";
  reasonCodes: readonly WatcherPostFinalityRecoveryReasonCode[];
  sourceRevision: string | null;
  nextRevision: string | null;
  sourceStoreDigest: string | null;
  nextStoreDigest: string | null;
  removedRecords: WatcherRollbackRemovedRecords;
  nextStore: WatcherDurableStore | null;
  resumableFinalityState: WatcherFinalityState | null;
  resumableRollbackState: WatcherRollbackState | null;
  resumableRollbackBootstrapState: WatcherRollbackState | null;
  resumableTrustedCheckpointStateDigest: string | null;
  recoveryState: WatcherPostFinalityRecoveryState | null;
  resultDigest: string;
}>;

export type WatcherPostFinalityRecoveryInput = Readonly<{
  policy: unknown;
  sourceStore: unknown;
  currentStore: unknown;
  quarantinedRollbackState: unknown;
  rollbackBootstrapState: unknown;
  trustedCheckpointAuthority?: unknown;
  previousCanonicalPath: unknown;
  replacementCanonicalPath: unknown;
  previousRecoveryState: unknown;
  transportAttestations?: unknown;
}>;

export type WatcherRollbackDurableAuthority = Readonly<{
  schemaVersion: typeof WATCHER_ROLLBACK_DURABLE_AUTHORITY_HANDLE_SCHEMA_VERSION;
  revision: string;
  snapshotSha256: string;
  authorityDigest: string;
}>;

export type WatcherRollbackDurableAuthorityStatus = Readonly<{
  revision: string;
  snapshotSha256: string;
  authorityDigest: string;
  priorSnapshotSha256: string | null;
  storeDigest: string;
  rollbackStateDigest: string;
  rollbackBootstrapStateDigest: string;
  trustedCheckpointStateDigest: string;
  authenticationKeyId: string;
  epoch: string;
  transitionCount: string;
  incidentDigest: string | null;
}>;

export type WatcherRollbackDurableAuthorityRead = Readonly<{
  currentStore: WatcherDurableStore;
  currentFinalityState: WatcherFinalityState;
  authenticatedConsistencyHistory: readonly WatcherMultiProviderConsistency[];
}>;

/**
 * This head must be stored by a monotonic, non-rollbackable authority that is
 * independent of the snapshot backend. An ordinary row in the same database,
 * even in another table, cannot establish freshness when that database can be
 * restored from an older backup.
 */
export type WatcherRollbackDurableTrustedHead = Readonly<{
  schemaVersion: typeof WATCHER_ROLLBACK_DURABLE_TRUSTED_HEAD_SCHEMA_VERSION;
  policyDigest: string;
  deploymentMarker: DeploymentMarker;
  authenticationKeyId: string;
  revision: string;
  snapshotSha256: string;
  authorityDigest: string;
  headMac: string;
}>;

export type WatcherRollbackDurableAuthorityOpenResult = Readonly<{
  initialized: boolean;
  authority: WatcherRollbackDurableAuthority;
  trustedHead: WatcherRollbackDurableTrustedHead;
}>;

export type WatcherRollbackDurableTrustedHeadReconciliation =
  | Readonly<{
      action: "already_aligned";
      trustedHead: WatcherRollbackDurableTrustedHead;
    }>
  | Readonly<{
      action: "publish_direct_successor";
      expectedTrustedHead: WatcherRollbackDurableTrustedHead | null;
      nextTrustedHead: WatcherRollbackDurableTrustedHead;
    }>;

type WatcherRollbackDurablePersistenceConflict = Readonly<{
  persistence: "conflict";
}>;

export type WatcherRollbackDurableEvaluationResult =
  | Readonly<{
      persistence: "committed" | "unchanged";
      authority: WatcherRollbackDurableAuthority;
      trustedHead: WatcherRollbackDurableTrustedHead;
      result: WatcherRollbackResult;
    }>
  | WatcherRollbackDurablePersistenceConflict;

export type WatcherRollbackDurableRecoveryResult =
  | Readonly<{
      persistence: "committed" | "unchanged";
      authority: WatcherRollbackDurableAuthority;
      trustedHead: WatcherRollbackDurableTrustedHead;
      result: WatcherPostFinalityRecoveryResult;
    }>
  | WatcherRollbackDurablePersistenceConflict;

export type WatcherRollbackDurableCanonicalProgressResult =
  | Readonly<{
      persistence: "committed" | "unchanged";
      authority: WatcherRollbackDurableAuthority;
      trustedHead: WatcherRollbackDurableTrustedHead;
      finalityResult: WatcherFinalityResult;
    }>
  | WatcherRollbackDurablePersistenceConflict;

export type WatcherRollbackDurableObservationResult =
  | Readonly<{
      persistence: "committed" | "unchanged";
      authority: WatcherRollbackDurableAuthority;
      trustedHead: WatcherRollbackDurableTrustedHead;
    }>
  | WatcherRollbackDurablePersistenceConflict;

export type WatcherRollbackDurableAuthoritySnapshot = Readonly<{
  validationSchemaVersion: typeof ROLLBACK_VALIDATION_SCHEMA_VERSION;
  schemaVersion: typeof WATCHER_ROLLBACK_DURABLE_AUTHORITY_SCHEMA_VERSION;
  revision: string;
  priorSnapshotSha256: string | null;
  policyDigest: string;
  deploymentMarker: DeploymentMarker;
  currentStore: WatcherDurableStore;
  consistencyHistory: readonly WatcherMultiProviderConsistency[];
  rollbackState: WatcherRollbackState;
  rollbackBootstrapState: WatcherRollbackState;
  trustedCheckpointStateDigest: string;
  userEventCheckpoint: WatcherUserEventCheckpoint | null;
  userEventValidation: WatcherUserEventValidation | null;
  authenticationKeyId: string;
  authorityDigest: string;
  authorityMac: string;
}>;

export type WatcherRollbackDurableAuthorityRuntime = Readonly<{
  backend: WatcherDurableAtomicBackend;
  policy: WatcherFinalityPolicy;
  snapshot: WatcherRollbackDurableAuthoritySnapshot;
  encoded: Uint8Array;
  snapshotSha256: string;
  authenticationKey: Uint8Array;
}>;

export const rollbackDurableAuthorityRuntime = new WeakMap<
  WatcherRollbackDurableAuthority,
  WatcherRollbackDurableAuthorityRuntime
>();

export type PlainRecord = Record<string, unknown>;

export type ParsedFinalityTransition =
  | Readonly<{
      kind: "rewind";
      previous: WatcherFinalityState;
      next: WatcherFinalityState;
      instruction: WatcherFinalityRewindInstruction;
      consistency: WatcherMultiProviderConsistency;
      finalityResult: WatcherFinalityResult;
    }>
  | Readonly<{
      kind: "incident";
      previous: WatcherFinalityState;
      next: WatcherFinalityState;
      consistency: WatcherMultiProviderConsistency;
      finalityResult: WatcherFinalityResult;
    }>;

export const sha256Canonical = watcherSha256CanonicalJson;
