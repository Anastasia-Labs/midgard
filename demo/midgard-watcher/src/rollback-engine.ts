import { createHash } from "node:crypto";

import {
  type DeploymentMarkerV1,
  parseDeploymentMarkerV1,
} from "../../midgard-core/src/deployment-manifest-identity-v1.js";
import {
  encodeWatcherDurableStoreV1,
  makeWatcherDurableStoreV1,
  parseWatcherDurableStoreV1,
  type WatcherDurableRecordsV1,
  watcherDurableStoreBytesSha256,
  type WatcherDurableStoreV1,
} from "./durable-store.js";
import {
  evaluateWatcherFinalityV1,
  parseWatcherFinalityPolicyV1,
  parseWatcherFinalityStateV1,
  WATCHER_FINALITY_RESULT_V1_SCHEMA_VERSION,
  WATCHER_FINALITY_REWIND_INSTRUCTION_V1_SCHEMA_VERSION,
  type WatcherFinalityBoundObservationV1,
  watcherFinalityConfiguredSourceV1,
  type WatcherFinalityIncidentV1,
  type WatcherFinalityPolicyV1,
  type WatcherFinalityResultV1,
  type WatcherFinalityRewindInstructionV1,
  type WatcherFinalityStateV1,
} from "./finality-engine.js";
import {
  encodeWatcherNormalizedL1BlockV1,
  type WatcherNormalizedL1BlockV1,
} from "./l1-adapter.js";
import {
  evaluateWatcherMultiProviderConsistencyV1,
  type WatcherMultiProviderConsistencyV1,
} from "./multi-provider-consistency.js";

export const WATCHER_ROLLBACK_STATE_V1_SCHEMA_VERSION =
  "midgard-watcher-rollback-state-v1" as const;
export const WATCHER_ROLLBACK_INCIDENT_V1_SCHEMA_VERSION =
  "midgard-watcher-rollback-incident-v1" as const;
export const WATCHER_ROLLBACK_RESULT_V1_SCHEMA_VERSION =
  "midgard-watcher-rollback-result-v1" as const;
export const WATCHER_ROLLBACK_TRANSITION_V1_SCHEMA_VERSION =
  "midgard-watcher-rollback-transition-v1" as const;
export const WATCHER_ROLLBACK_EPOCH_CHECKPOINT_V1_SCHEMA_VERSION =
  "midgard-watcher-rollback-epoch-checkpoint-v1" as const;
export const WATCHER_POST_FINALITY_RECOVERY_STATE_V1_SCHEMA_VERSION =
  "midgard-watcher-post-finality-recovery-state-v1" as const;
export const WATCHER_POST_FINALITY_RECOVERY_RESULT_V1_SCHEMA_VERSION =
  "midgard-watcher-post-finality-recovery-result-v1" as const;
export const WATCHER_ROLLBACK_V1_BOUNDS = Object.freeze({
  transitionHistory: 128,
  postFinalityRecoveryDepth: 2_160n,
});

const HEX_32 = /^[0-9a-f]{64}$/u;
const CANONICAL_NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const NETWORKS = ["Mainnet", "Preprod", "Preview"] as const;
const isNetwork = (value: unknown): value is (typeof NETWORKS)[number] =>
  typeof value === "string" &&
  NETWORKS.includes(value as (typeof NETWORKS)[number]);

export const WATCHER_ROLLBACK_REASON_CODES_V1 = [
  "rewind_applied",
  "duplicate_instruction",
  "post_finality_incident",
  "malformed_policy",
  "malformed_store",
  "malformed_previous_finality_state",
  "malformed_finality_result",
  "malformed_rollback_state",
  "deployment_mismatch",
  "release_evidence_mismatch",
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

export const WATCHER_ROLLBACK_ALERT_CODES_V1 = [
  "watcher_rollback_rewind_applied",
  "watcher_rollback_input_rejected",
  "watcher_rollback_configuration_mismatch",
  "watcher_rollback_state_rejected",
  "watcher_rollback_post_finality_incident",
  "watcher_rollback_quarantined",
] as const;

export type WatcherRollbackReasonCodeV1 =
  (typeof WATCHER_ROLLBACK_REASON_CODES_V1)[number];
export type WatcherRollbackAlertCodeV1 =
  (typeof WATCHER_ROLLBACK_ALERT_CODES_V1)[number];

export type WatcherRollbackRemovedRecordsV1 = Readonly<{
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

export type WatcherRollbackIncidentV1 = Readonly<{
  schemaVersion: typeof WATCHER_ROLLBACK_INCIDENT_V1_SCHEMA_VERSION;
  reasonCode: WatcherFinalityIncidentV1["reasonCode"];
  policyDigest: string;
  releaseEvidenceDigest: string;
  deploymentMarker: DeploymentMarkerV1;
  finalityStateDigest: string;
  finalityIncidentDigest: string;
  triggerConsistencyDigest: string | null;
  finalizedBinding: WatcherFinalityBoundObservationV1;
  sourceStoreDigest: string;
  nextStoreDigest: string;
  transitionCount: string;
  previousFinalityStateDigest: string;
  consistencyDigest: string;
  finalityResultDigest: string;
  incidentDigest: string;
}>;

export type WatcherRollbackEpochCheckpointV1 = Readonly<{
  schemaVersion: typeof WATCHER_ROLLBACK_EPOCH_CHECKPOINT_V1_SCHEMA_VERSION;
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

export type WatcherRollbackStateV1 = Readonly<{
  schemaVersion: typeof WATCHER_ROLLBACK_STATE_V1_SCHEMA_VERSION;
  policyDigest: string;
  network: (typeof NETWORKS)[number];
  releaseEvidenceDigest: string;
  deploymentMarker: DeploymentMarkerV1;
  bootstrapStore: WatcherDurableStoreV1;
  bootstrapFinalityState: WatcherFinalityStateV1;
  epoch: string;
  epochCheckpoint: WatcherRollbackEpochCheckpointV1 | null;
  transitions: readonly WatcherRollbackTransitionV1[];
  storeDigest: string;
  transitionCount: string;
  currentFinalityStateDigest: string | null;
  lastPreviousFinalityStateDigest: string | null;
  lastConsistencyDigest: string | null;
  lastFinalityResultDigest: string | null;
  lastInstructionDigest: string | null;
  transitionLineageDigest: string;
  incident: WatcherRollbackIncidentV1 | null;
  stateDigest: string;
}>;

export type WatcherRollbackTransitionV1 = Readonly<{
  schemaVersion: typeof WATCHER_ROLLBACK_TRANSITION_V1_SCHEMA_VERSION;
  previousFinalityState: WatcherFinalityStateV1;
  consistency: WatcherMultiProviderConsistencyV1;
  finalityResult: WatcherFinalityResultV1;
  transitionDigest: string;
}>;

export type WatcherRollbackStateVerificationContextV1 = Readonly<{
  policy: unknown;
  rollbackBootstrapState: unknown;
  trustedCheckpointStateDigest?: unknown;
  currentStore: unknown;
}>;

export type WatcherRollbackVerificationContextV1 = Readonly<{
  policy: unknown;
  sourceStore: unknown;
  previousFinalityState: unknown;
  consistency: unknown;
  finalityResult: unknown;
  previousRollbackState: unknown;
  rollbackBootstrapState: unknown;
  trustedCheckpointStateDigest?: unknown;
}>;

export type WatcherRollbackResultV1 = Readonly<{
  schemaVersion: typeof WATCHER_ROLLBACK_RESULT_V1_SCHEMA_VERSION;
  action:
    | "apply_rewind"
    | "duplicate_rewind"
    | "quarantine_incident"
    | "reject";
  protocolDecision: "resume_pending" | "hold" | "quarantined";
  reasonCodes: readonly WatcherRollbackReasonCodeV1[];
  alertCodes: readonly WatcherRollbackAlertCodeV1[];
  sourceRevision: string | null;
  nextRevision: string | null;
  instructionDigest: string | null;
  sourceStoreDigest: string | null;
  nextStoreDigest: string | null;
  removedRecords: WatcherRollbackRemovedRecordsV1;
  nextStore: WatcherDurableStoreV1 | null;
  rollbackState: WatcherRollbackStateV1 | null;
  rollbackBootstrapState: WatcherRollbackStateV1 | null;
  trustedCheckpointStateDigest: string | null;
  resultDigest: string;
}>;

export const WATCHER_POST_FINALITY_RECOVERY_REASON_CODES_V1 = [
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

export type WatcherPostFinalityRecoveryReasonCodeV1 =
  (typeof WATCHER_POST_FINALITY_RECOVERY_REASON_CODES_V1)[number];

export type WatcherPostFinalityRecoveryPathV1 = Readonly<{
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

export type WatcherPostFinalityRecoveryIncidentLifecycleV1 = Readonly<{
  detectedIncidentDigest: string;
  detectedReasonCode: WatcherFinalityIncidentV1["reasonCode"];
  detectedTriggerConsistencyDigest: string | null;
  detectedFinalityStateDigest: string;
  detectedStoreDigest: string;
  status: "recovered";
  recoveryPathDigest: string;
  recoveredStoreDigest: string;
  resumableFinalityStateDigest: string;
  lifecycleDigest: string;
}>;

export type WatcherPostFinalityRecoveryStateV1 = Readonly<{
  schemaVersion: typeof WATCHER_POST_FINALITY_RECOVERY_STATE_V1_SCHEMA_VERSION;
  policyDigest: string;
  network: (typeof NETWORKS)[number];
  releaseEvidenceDigest: string;
  deploymentMarker: DeploymentMarkerV1;
  sourceRollbackStateDigest: string;
  sourceStoreDigest: string;
  nextStoreDigest: string;
  path: WatcherPostFinalityRecoveryPathV1;
  removedRecords: WatcherRollbackRemovedRecordsV1;
  resumableFinalityState: WatcherFinalityStateV1;
  incidentLifecycle: WatcherPostFinalityRecoveryIncidentLifecycleV1;
  stateDigest: string;
}>;

export type WatcherPostFinalityRecoveryResultV1 = Readonly<{
  schemaVersion: typeof WATCHER_POST_FINALITY_RECOVERY_RESULT_V1_SCHEMA_VERSION;
  action: "rewind_and_replay" | "duplicate_recovery" | "reject";
  protocolDecision: "resume_replay" | "hold" | "quarantined";
  reasonCodes: readonly WatcherPostFinalityRecoveryReasonCodeV1[];
  sourceRevision: string | null;
  nextRevision: string | null;
  sourceStoreDigest: string | null;
  nextStoreDigest: string | null;
  removedRecords: WatcherRollbackRemovedRecordsV1;
  nextStore: WatcherDurableStoreV1 | null;
  resumableFinalityState: WatcherFinalityStateV1 | null;
  resumableRollbackState: WatcherRollbackStateV1 | null;
  resumableRollbackBootstrapState: WatcherRollbackStateV1 | null;
  resumableTrustedCheckpointStateDigest: string | null;
  recoveryState: WatcherPostFinalityRecoveryStateV1 | null;
  resultDigest: string;
}>;

export type WatcherPostFinalityRecoveryInputV1 = Readonly<{
  policy: unknown;
  sourceStore: unknown;
  currentStore: unknown;
  quarantinedRollbackState: unknown;
  rollbackBootstrapState: unknown;
  trustedCheckpointStateDigest?: unknown;
  previousCanonicalPath: unknown;
  replacementCanonicalPath: unknown;
  previousRecoveryState: unknown;
}>;

type PlainRecord = Record<string, unknown>;

type ParsedFinalityTransition =
  | Readonly<{
      kind: "rewind";
      previous: WatcherFinalityStateV1;
      next: WatcherFinalityStateV1;
      instruction: WatcherFinalityRewindInstructionV1;
      consistency: WatcherMultiProviderConsistencyV1;
      finalityResult: WatcherFinalityResultV1;
    }>
  | Readonly<{
      kind: "incident";
      previous: WatcherFinalityStateV1;
      next: WatcherFinalityStateV1;
      consistency: WatcherMultiProviderConsistencyV1;
      finalityResult: WatcherFinalityResultV1;
    }>;

const sha256Canonical = (value: unknown): string =>
  createHash("sha256").update(JSON.stringify(value), "utf8").digest("hex");

const exactPlainRecord = (
  value: unknown,
  keys: readonly string[],
): PlainRecord | null => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    return null;
  }
  const candidate = value as object;
  if (
    Object.getPrototypeOf(candidate) !== Object.prototype ||
    Reflect.ownKeys(candidate).length !== keys.length
  ) {
    return null;
  }
  const expected = new Set(keys);
  for (const key of Reflect.ownKeys(candidate)) {
    if (typeof key !== "string" || !expected.has(key)) {
      return null;
    }
    const descriptor = Object.getOwnPropertyDescriptor(candidate, key);
    if (
      descriptor === undefined ||
      !descriptor.enumerable ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined
    ) {
      return null;
    }
  }
  return value as PlainRecord;
};

const exactStringArray = (
  value: unknown,
  allowed: readonly string[],
): readonly string[] | null => {
  if (
    !Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Array.prototype ||
    Reflect.ownKeys(value).length !== value.length + 1
  ) {
    return null;
  }
  const strings: string[] = [];
  for (let index = 0; index < value.length; index += 1) {
    const descriptor = Object.getOwnPropertyDescriptor(value, index.toString());
    const member = value[index] as unknown;
    if (
      descriptor === undefined ||
      !descriptor.enumerable ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined ||
      typeof member !== "string" ||
      !allowed.includes(member)
    ) {
      return null;
    }
    strings.push(member);
  }
  return Object.freeze(strings);
};

const exactUnrestrictedStringArray = (
  value: unknown,
): readonly string[] | null => {
  if (
    !Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Array.prototype ||
    Reflect.ownKeys(value).length !== value.length + 1
  ) {
    return null;
  }
  const strings: string[] = [];
  for (let index = 0; index < value.length; index += 1) {
    const descriptor = Object.getOwnPropertyDescriptor(value, index.toString());
    const member = value[index] as unknown;
    if (
      descriptor === undefined ||
      !descriptor.enumerable ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined ||
      typeof member !== "string"
    ) {
      return null;
    }
    strings.push(member);
  }
  return Object.freeze(strings);
};

const exactArray = (value: unknown): readonly unknown[] | null => {
  if (
    !Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Array.prototype ||
    Reflect.ownKeys(value).length !== value.length + 1
  ) {
    return null;
  }
  for (let index = 0; index < value.length; index += 1) {
    const descriptor = Object.getOwnPropertyDescriptor(value, index.toString());
    if (
      descriptor === undefined ||
      !descriptor.enumerable ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined
    ) {
      return null;
    }
  }
  return value as readonly unknown[];
};

const sameStrings = (
  left: readonly string[],
  right: readonly string[],
): boolean =>
  left.length === right.length &&
  left.every((member, index) => member === right[index]);

const marker = (value: unknown): DeploymentMarkerV1 | null => {
  try {
    return parseDeploymentMarkerV1(value);
  } catch {
    return null;
  }
};

const sameMarker = (
  left: DeploymentMarkerV1,
  right: DeploymentMarkerV1,
): boolean =>
  left.schemaVersion === right.schemaVersion &&
  left.manifestId === right.manifestId;

const sameBinding = (
  left: WatcherFinalityBoundObservationV1,
  right: WatcherFinalityBoundObservationV1,
): boolean => JSON.stringify(left) === JSON.stringify(right);

const emptyRemovedRecords = (): WatcherRollbackRemovedRecordsV1 =>
  Object.freeze({
    l1ObservationIds: Object.freeze([]),
    chainPointIds: Object.freeze([]),
    protocolUtxoOutRefs: Object.freeze([]),
    daProofInputIds: Object.freeze([]),
    reconstructedBlockHashes: Object.freeze([]),
    decisionBlockHashes: Object.freeze([]),
    faultIds: Object.freeze([]),
    submissionIds: Object.freeze([]),
    confirmationIds: Object.freeze([]),
    retryIds: Object.freeze([]),
    deadlineIds: Object.freeze([]),
    correctionResultIds: Object.freeze([]),
  });

const makeResult = (
  value: Omit<WatcherRollbackResultV1, "schemaVersion" | "resultDigest">,
): WatcherRollbackResultV1 => {
  const canonical = {
    schemaVersion: WATCHER_ROLLBACK_RESULT_V1_SCHEMA_VERSION,
    action: value.action,
    protocolDecision: value.protocolDecision,
    reasonCodes: Object.freeze([...value.reasonCodes]),
    alertCodes: Object.freeze([...value.alertCodes]),
    sourceRevision: value.sourceRevision,
    nextRevision: value.nextRevision,
    instructionDigest: value.instructionDigest,
    sourceStoreDigest: value.sourceStoreDigest,
    nextStoreDigest: value.nextStoreDigest,
    removedRecords: value.removedRecords,
    nextStore: value.nextStore,
    rollbackState: value.rollbackState,
    rollbackBootstrapState: value.rollbackBootstrapState,
    trustedCheckpointStateDigest: value.trustedCheckpointStateDigest,
  };
  return Object.freeze({
    ...canonical,
    resultDigest: sha256Canonical(canonical),
  });
};

const reject = (
  reason: WatcherRollbackReasonCodeV1,
  alert: WatcherRollbackAlertCodeV1 = "watcher_rollback_input_rejected",
): WatcherRollbackResultV1 =>
  makeResult({
    action: "reject",
    protocolDecision: "quarantined",
    reasonCodes: [reason],
    alertCodes: [alert],
    sourceRevision: null,
    nextRevision: null,
    instructionDigest: null,
    sourceStoreDigest: null,
    nextStoreDigest: null,
    removedRecords: emptyRemovedRecords(),
    nextStore: null,
    rollbackState: null,
    rollbackBootstrapState: null,
    trustedCheckpointStateDigest: null,
  });

const parseInstruction = (
  value: unknown,
): WatcherFinalityRewindInstructionV1 | null => {
  const instruction = exactPlainRecord(value, [
    "schemaVersion",
    "kind",
    "discardedStateDigest",
    "replacementPointDigest",
    "replacementContentDigest",
    "replacementDepth",
    "instructionDigest",
  ]);
  if (
    instruction === null ||
    instruction.schemaVersion !==
      WATCHER_FINALITY_REWIND_INSTRUCTION_V1_SCHEMA_VERSION ||
    ![
      "pending_depth_regression",
      "pending_point_changed",
      "pending_content_changed",
    ].includes(instruction.kind as string) ||
    typeof instruction.discardedStateDigest !== "string" ||
    !HEX_32.test(instruction.discardedStateDigest) ||
    typeof instruction.replacementPointDigest !== "string" ||
    !HEX_32.test(instruction.replacementPointDigest) ||
    typeof instruction.replacementContentDigest !== "string" ||
    !HEX_32.test(instruction.replacementContentDigest) ||
    typeof instruction.replacementDepth !== "string" ||
    !CANONICAL_NATURAL.test(instruction.replacementDepth) ||
    typeof instruction.instructionDigest !== "string" ||
    !HEX_32.test(instruction.instructionDigest)
  ) {
    return null;
  }
  const canonical = {
    schemaVersion: WATCHER_FINALITY_REWIND_INSTRUCTION_V1_SCHEMA_VERSION,
    kind: instruction.kind as WatcherFinalityRewindInstructionV1["kind"],
    discardedStateDigest: instruction.discardedStateDigest,
    replacementPointDigest: instruction.replacementPointDigest,
    replacementContentDigest: instruction.replacementContentDigest,
    replacementDepth: instruction.replacementDepth,
  };
  if (sha256Canonical(canonical) !== instruction.instructionDigest) {
    return null;
  }
  return Object.freeze({
    ...canonical,
    instructionDigest: instruction.instructionDigest,
  });
};

const stateBindingFailure = (
  policy: WatcherFinalityPolicyV1,
  state: WatcherFinalityStateV1,
): WatcherRollbackReasonCodeV1 | null => {
  if (state.network !== policy.network) {
    return "network_mismatch";
  }
  if (state.releaseEvidenceDigest !== policy.releaseEvidenceDigest) {
    return "release_evidence_mismatch";
  }
  if (!sameMarker(state.deploymentMarker, policy.deploymentMarker)) {
    return "deployment_mismatch";
  }
  return state.policyDigest === policy.policyDigest ? null : "policy_mismatch";
};

const parseFinalityTransition = (
  policy: WatcherFinalityPolicyV1,
  previousInput: unknown,
  consistencyInput: unknown,
  resultInput: unknown,
): ParsedFinalityTransition | WatcherRollbackReasonCodeV1 => {
  const previous = parseWatcherFinalityStateV1(previousInput);
  if (previous === null) {
    return "malformed_previous_finality_state";
  }
  const previousBindingFailure = stateBindingFailure(policy, previous);
  if (previousBindingFailure !== null) {
    return previousBindingFailure;
  }
  if (parseWatcherFinalityStateV1(previous, policy) === null) {
    return "malformed_previous_finality_state";
  }
  const consistencyRecord = exactPlainRecord(consistencyInput, [
    "schemaVersion",
    "status",
    "protocolDecision",
    "sourceMode",
    "configuredNetwork",
    "configuredSourceDigest",
    "authorityNodeId",
    "authorityGenesisIdentitySha256",
    "chainAuthorityObservationDigest",
    "queryObservationCount",
    "observationCount",
    "independentProviderCount",
    "externalProviderBindings",
    "localQueryServiceBindings",
    "reasonCodes",
    "alertCodes",
    "observationEvidenceDigests",
    "rejectedObservationCount",
    "agreement",
    "consistencyDigest",
  ]);
  if (
    consistencyRecord === null ||
    consistencyRecord.schemaVersion !==
      "midgard-watcher-multi-provider-consistency-v1" ||
    typeof consistencyRecord.consistencyDigest !== "string" ||
    !HEX_32.test(consistencyRecord.consistencyDigest)
  ) {
    return "finality_provenance_mismatch";
  }
  const recomputed = evaluateWatcherFinalityV1(
    policy,
    previous,
    consistencyInput,
  );
  if (JSON.stringify(recomputed) !== JSON.stringify(resultInput)) {
    return "finality_provenance_mismatch";
  }
  const consistency = consistencyInput as WatcherMultiProviderConsistencyV1;

  const result = exactPlainRecord(resultInput, [
    "schemaVersion",
    "action",
    "protocolDecision",
    "reasonCodes",
    "alertCodes",
    "state",
    "rewindInstruction",
    "resultDigest",
  ]);
  if (
    result === null ||
    result.schemaVersion !== WATCHER_FINALITY_RESULT_V1_SCHEMA_VERSION ||
    typeof result.resultDigest !== "string" ||
    !HEX_32.test(result.resultDigest)
  ) {
    return "malformed_finality_result";
  }
  const reasons = exactStringArray(result.reasonCodes, [
    "pending_depth_regression",
    "pending_point_changed",
    "pending_content_changed",
    "post_finality_point_changed",
    "post_finality_content_changed",
    "post_finality_contradiction",
  ]);
  const alerts = exactStringArray(result.alertCodes, [
    "watcher_finality_rewind_required",
    "watcher_finality_pending",
    "watcher_finality_post_finality_incident",
  ]);
  const next = parseWatcherFinalityStateV1(result.state);
  if (reasons === null || alerts === null || next === null) {
    return "malformed_finality_result";
  }
  const nextBindingFailure = stateBindingFailure(policy, next);
  if (nextBindingFailure !== null) {
    return nextBindingFailure;
  }
  if (parseWatcherFinalityStateV1(next, policy) === null) {
    return "malformed_finality_result";
  }
  const instruction =
    result.rewindInstruction === null
      ? null
      : parseInstruction(result.rewindInstruction);
  if (result.rewindInstruction !== null && instruction === null) {
    return "malformed_finality_result";
  }
  const canonical = {
    schemaVersion: WATCHER_FINALITY_RESULT_V1_SCHEMA_VERSION,
    action: result.action,
    protocolDecision: result.protocolDecision,
    reasonCodes: reasons,
    alertCodes: alerts,
    state: next,
    rewindInstruction: instruction,
  };
  if (sha256Canonical(canonical) !== result.resultDigest) {
    return "malformed_finality_result";
  }

  if (
    result.action === "rewind_pending" &&
    result.protocolDecision === "rewind_required" &&
    previous.phase === "pending" &&
    previous.pending !== null &&
    next.phase === "pending" &&
    next.pending !== null &&
    instruction !== null &&
    instruction.discardedStateDigest === previous.stateDigest &&
    instruction.replacementPointDigest === next.pending.pointDigest &&
    instruction.replacementContentDigest === next.pending.blockContentDigest &&
    instruction.replacementDepth === next.pending.currentDepth &&
    sameStrings(reasons, [instruction.kind]) &&
    sameStrings(alerts, [
      "watcher_finality_rewind_required",
      "watcher_finality_pending",
    ])
  ) {
    return Object.freeze({
      kind: "rewind",
      previous,
      next,
      instruction,
      consistency,
      finalityResult: recomputed,
    });
  }

  const finalityIncident = next.incident;
  if (
    result.action === "quarantine_incident" &&
    result.protocolDecision === "quarantined" &&
    previous.phase === "finalized" &&
    previous.finalized !== null &&
    next.phase === "quarantined" &&
    next.pending === null &&
    next.finalized !== null &&
    sameBinding(previous.finalized, next.finalized) &&
    finalityIncident !== null &&
    finalityIncident.triggerConsistencyDigest ===
      consistency.consistencyDigest &&
    instruction === null &&
    sameStrings(reasons, [
      finalityIncident.reasonCode,
      "post_finality_contradiction",
    ]) &&
    sameStrings(alerts, ["watcher_finality_post_finality_incident"]) &&
    finalityIncident.incidentDigest ===
      sha256Canonical({
        reasonCode: finalityIncident.reasonCode,
        triggerConsistencyDigest: finalityIncident.triggerConsistencyDigest,
        priorStateDigest: previous.stateDigest,
      })
  ) {
    return Object.freeze({
      kind: "incident",
      previous,
      next,
      consistency,
      finalityResult: recomputed,
    });
  }
  return "invalid_finality_transition";
};

const makeIncident = (
  policy: WatcherFinalityPolicyV1,
  transition: Extract<ParsedFinalityTransition, { readonly kind: "incident" }>,
  sourceStoreDigest: string,
  nextStoreDigest: string,
  transitionCount: string,
): WatcherRollbackIncidentV1 => {
  const finalityIncident = transition.next
    .incident as WatcherFinalityIncidentV1;
  const finalizedBinding = transition.next
    .finalized as WatcherFinalityBoundObservationV1;
  const canonical = {
    schemaVersion: WATCHER_ROLLBACK_INCIDENT_V1_SCHEMA_VERSION,
    reasonCode: finalityIncident.reasonCode,
    policyDigest: policy.policyDigest,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    deploymentMarker: policy.deploymentMarker,
    finalityStateDigest: transition.next.stateDigest,
    finalityIncidentDigest: finalityIncident.incidentDigest,
    triggerConsistencyDigest: finalityIncident.triggerConsistencyDigest,
    finalizedBinding,
    sourceStoreDigest,
    nextStoreDigest,
    transitionCount,
    previousFinalityStateDigest: transition.previous.stateDigest,
    consistencyDigest: transition.consistency.consistencyDigest,
    finalityResultDigest: transition.finalityResult.resultDigest,
  };
  return Object.freeze({
    ...canonical,
    incidentDigest: sha256Canonical(canonical),
  });
};

const makeRollbackState = (
  policy: WatcherFinalityPolicyV1,
  value: Readonly<{
    bootstrapStore: WatcherDurableStoreV1;
    bootstrapFinalityState: WatcherFinalityStateV1;
    epoch: string;
    epochCheckpoint: WatcherRollbackEpochCheckpointV1 | null;
    transitions: readonly WatcherRollbackTransitionV1[];
    storeDigest: string;
    transitionCount: string;
    currentFinalityStateDigest: string | null;
    lastPreviousFinalityStateDigest: string | null;
    lastConsistencyDigest: string | null;
    lastFinalityResultDigest: string | null;
    lastInstructionDigest: string | null;
    transitionLineageDigest: string;
    incident: WatcherRollbackIncidentV1 | null;
  }>,
): WatcherRollbackStateV1 => {
  const canonical = {
    schemaVersion: WATCHER_ROLLBACK_STATE_V1_SCHEMA_VERSION,
    policyDigest: policy.policyDigest,
    network: policy.network,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    deploymentMarker: policy.deploymentMarker,
    bootstrapStore: value.bootstrapStore,
    bootstrapFinalityState: value.bootstrapFinalityState,
    epoch: value.epoch,
    epochCheckpoint: value.epochCheckpoint,
    transitions: Object.freeze([...value.transitions]),
    storeDigest: value.storeDigest,
    transitionCount: value.transitionCount,
    currentFinalityStateDigest: value.currentFinalityStateDigest,
    lastPreviousFinalityStateDigest: value.lastPreviousFinalityStateDigest,
    lastConsistencyDigest: value.lastConsistencyDigest,
    lastFinalityResultDigest: value.lastFinalityResultDigest,
    lastInstructionDigest: value.lastInstructionDigest,
    transitionLineageDigest: value.transitionLineageDigest,
    incident: value.incident,
  };
  return Object.freeze({
    ...canonical,
    stateDigest: sha256Canonical(canonical),
  });
};

const genesisLineageDigest = (policy: WatcherFinalityPolicyV1): string =>
  sha256Canonical({
    schemaVersion: WATCHER_ROLLBACK_STATE_V1_SCHEMA_VERSION,
    kind: "genesis",
    policyDigest: policy.policyDigest,
    network: policy.network,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    deploymentMarker: policy.deploymentMarker,
  });

const initialRollbackState = (
  policy: WatcherFinalityPolicyV1,
  bootstrapStore: WatcherDurableStoreV1,
  bootstrapFinalityState: WatcherFinalityStateV1,
): WatcherRollbackStateV1 =>
  makeRollbackState(policy, {
    bootstrapStore,
    bootstrapFinalityState,
    epoch: "0",
    epochCheckpoint: null,
    transitions: [],
    storeDigest: storeDigest(bootstrapStore),
    transitionCount: "0",
    currentFinalityStateDigest: null,
    lastPreviousFinalityStateDigest: null,
    lastConsistencyDigest: null,
    lastFinalityResultDigest: null,
    lastInstructionDigest: null,
    transitionLineageDigest: genesisLineageDigest(policy),
    incident: null,
  });

const rootBootstrapStateDigest = (
  policy: WatcherFinalityPolicyV1,
  state: WatcherRollbackStateV1,
): string =>
  state.epochCheckpoint?.rootBootstrapStateDigest ??
  initialRollbackState(
    policy,
    state.bootstrapStore,
    state.bootstrapFinalityState,
  ).stateDigest;

const makeEpochCheckpoint = (
  policy: WatcherFinalityPolicyV1,
  prior: WatcherRollbackStateV1,
  checkpointStore: WatcherDurableStoreV1,
  checkpointFinalityState: WatcherFinalityStateV1,
  recovery: Readonly<{
    stateDigest: string;
    lifecycleDigest: string;
  }> | null = null,
): WatcherRollbackEpochCheckpointV1 => {
  const canonical = {
    schemaVersion: WATCHER_ROLLBACK_EPOCH_CHECKPOINT_V1_SCHEMA_VERSION,
    epoch: (BigInt(prior.epoch) + 1n).toString(),
    rootBootstrapStateDigest: rootBootstrapStateDigest(policy, prior),
    priorCheckpointDigest: prior.epochCheckpoint?.checkpointDigest ?? null,
    priorTerminalStateDigest: prior.stateDigest,
    priorTerminalTransitionCount: prior.transitionCount,
    priorTerminalTransitionLineageDigest: prior.transitionLineageDigest,
    priorTerminalStoreDigest: prior.storeDigest,
    priorTerminalFinalityStateDigest:
      prior.currentFinalityStateDigest ??
      prior.bootstrapFinalityState.stateDigest,
    priorTerminalIncidentDigest: prior.incident?.incidentDigest ?? null,
    recoveryStateDigest: recovery?.stateDigest ?? null,
    recoveryLifecycleDigest: recovery?.lifecycleDigest ?? null,
    checkpointStoreDigest: storeDigest(checkpointStore),
    checkpointFinalityStateDigest: checkpointFinalityState.stateDigest,
  };
  return Object.freeze({
    ...canonical,
    checkpointDigest: sha256Canonical(canonical),
  });
};

const makeEpochBootstrapState = (
  policy: WatcherFinalityPolicyV1,
  prior: WatcherRollbackStateV1,
  checkpointStore: WatcherDurableStoreV1,
  checkpointFinalityState: WatcherFinalityStateV1,
  recovery: Readonly<{
    stateDigest: string;
    lifecycleDigest: string;
  }> | null = null,
): WatcherRollbackStateV1 => {
  const epochCheckpoint = makeEpochCheckpoint(
    policy,
    prior,
    checkpointStore,
    checkpointFinalityState,
    recovery,
  );
  return makeRollbackState(policy, {
    bootstrapStore: checkpointStore,
    bootstrapFinalityState: checkpointFinalityState,
    epoch: epochCheckpoint.epoch,
    epochCheckpoint,
    transitions: [],
    storeDigest: epochCheckpoint.checkpointStoreDigest,
    transitionCount: prior.transitionCount,
    currentFinalityStateDigest: epochCheckpoint.checkpointFinalityStateDigest,
    lastPreviousFinalityStateDigest: null,
    lastConsistencyDigest: null,
    lastFinalityResultDigest: null,
    lastInstructionDigest: null,
    transitionLineageDigest:
      epochCheckpoint.priorTerminalTransitionLineageDigest,
    incident: null,
  });
};

const makeTransitionRecord = (
  transition: ParsedFinalityTransition,
): WatcherRollbackTransitionV1 => {
  const canonical = {
    schemaVersion: WATCHER_ROLLBACK_TRANSITION_V1_SCHEMA_VERSION,
    previousFinalityState: transition.previous,
    consistency: transition.consistency,
    finalityResult: transition.finalityResult,
  };
  return Object.freeze({
    ...canonical,
    transitionDigest: sha256Canonical(canonical),
  });
};

const advanceRollbackState = (
  policy: WatcherFinalityPolicyV1,
  previous: WatcherRollbackStateV1,
  activeBootstrapState: WatcherRollbackStateV1,
  transition: ParsedFinalityTransition,
  sourceStore: WatcherDurableStoreV1,
  durableStoreDigest: string,
  incident: WatcherRollbackIncidentV1 | null,
): Readonly<{
  state: WatcherRollbackStateV1;
  bootstrapState: WatcherRollbackStateV1;
}> => {
  const epochState =
    previous.transitions.length >= WATCHER_ROLLBACK_V1_BOUNDS.transitionHistory
      ? makeEpochBootstrapState(
          policy,
          previous,
          sourceStore,
          transition.previous,
        )
      : previous;
  const transitionCount = (BigInt(epochState.transitionCount) + 1n).toString();
  const lastInstructionDigest =
    transition.kind === "rewind"
      ? transition.instruction.instructionDigest
      : null;
  const transitionLineageDigest = sha256Canonical({
    priorLineageDigest: epochState.transitionLineageDigest,
    transitionCount,
    previousFinalityStateDigest: transition.previous.stateDigest,
    consistencyDigest: transition.consistency.consistencyDigest,
    finalityResultDigest: transition.finalityResult.resultDigest,
    currentFinalityStateDigest: transition.next.stateDigest,
    instructionDigest: lastInstructionDigest,
    storeDigest: durableStoreDigest,
  });
  const transitions = Object.freeze([
    ...epochState.transitions,
    makeTransitionRecord(transition),
  ]);
  const state = makeRollbackState(policy, {
    bootstrapStore: epochState.bootstrapStore,
    bootstrapFinalityState: epochState.bootstrapFinalityState,
    epoch: epochState.epoch,
    epochCheckpoint: epochState.epochCheckpoint,
    transitions,
    storeDigest: durableStoreDigest,
    transitionCount,
    currentFinalityStateDigest: transition.next.stateDigest,
    lastPreviousFinalityStateDigest: transition.previous.stateDigest,
    lastConsistencyDigest: transition.consistency.consistencyDigest,
    lastFinalityResultDigest: transition.finalityResult.resultDigest,
    lastInstructionDigest,
    transitionLineageDigest,
    incident,
  });
  return Object.freeze({
    state,
    bootstrapState: epochState === previous ? activeBootstrapState : epochState,
  });
};

const parseBoundObservation = (
  value: unknown,
): WatcherFinalityBoundObservationV1 | null => {
  const record = exactPlainRecord(value, [
    "pointDigest",
    "blockHash",
    "slot",
    "blockNo",
    "blockContentDigest",
    "firstSeenConsistencyDigest",
    "lastSeenConsistencyDigest",
    "firstSeenDepth",
    "currentDepth",
    "visibilityCount",
  ]);
  if (
    record === null ||
    [
      record.pointDigest,
      record.blockHash,
      record.blockContentDigest,
      record.firstSeenConsistencyDigest,
      record.lastSeenConsistencyDigest,
    ].some((member) => typeof member !== "string" || !HEX_32.test(member)) ||
    [
      record.slot,
      record.blockNo,
      record.firstSeenDepth,
      record.currentDepth,
    ].some(
      (member) => typeof member !== "string" || !CANONICAL_NATURAL.test(member),
    ) ||
    typeof record.visibilityCount !== "string" ||
    !/^[1-9][0-9]*$/u.test(record.visibilityCount)
  ) {
    return null;
  }
  return Object.freeze({
    pointDigest: record.pointDigest as string,
    blockHash: record.blockHash as string,
    slot: record.slot as string,
    blockNo: record.blockNo as string,
    blockContentDigest: record.blockContentDigest as string,
    firstSeenConsistencyDigest: record.firstSeenConsistencyDigest as string,
    lastSeenConsistencyDigest: record.lastSeenConsistencyDigest as string,
    firstSeenDepth: record.firstSeenDepth as string,
    currentDepth: record.currentDepth as string,
    visibilityCount: record.visibilityCount,
  });
};

const parseRollbackIncident = (
  value: unknown,
): WatcherRollbackIncidentV1 | null => {
  const record = exactPlainRecord(value, [
    "schemaVersion",
    "reasonCode",
    "policyDigest",
    "releaseEvidenceDigest",
    "deploymentMarker",
    "finalityStateDigest",
    "finalityIncidentDigest",
    "triggerConsistencyDigest",
    "finalizedBinding",
    "sourceStoreDigest",
    "nextStoreDigest",
    "transitionCount",
    "previousFinalityStateDigest",
    "consistencyDigest",
    "finalityResultDigest",
    "incidentDigest",
  ]);
  if (
    record === null ||
    record.schemaVersion !== WATCHER_ROLLBACK_INCIDENT_V1_SCHEMA_VERSION ||
    record.reasonCode !== "post_finality_point_changed"
  ) {
    return null;
  }
  const deploymentMarker = marker(record.deploymentMarker);
  const finalizedBinding = parseBoundObservation(record.finalizedBinding);
  if (
    deploymentMarker === null ||
    finalizedBinding === null ||
    [
      record.policyDigest,
      record.releaseEvidenceDigest,
      record.finalityStateDigest,
      record.finalityIncidentDigest,
      record.sourceStoreDigest,
      record.nextStoreDigest,
      record.previousFinalityStateDigest,
      record.consistencyDigest,
      record.finalityResultDigest,
      record.incidentDigest,
    ].some((member) => typeof member !== "string" || !HEX_32.test(member)) ||
    typeof record.transitionCount !== "string" ||
    !/^[1-9][0-9]*$/u.test(record.transitionCount) ||
    !(
      record.triggerConsistencyDigest === null ||
      (typeof record.triggerConsistencyDigest === "string" &&
        HEX_32.test(record.triggerConsistencyDigest))
    )
  ) {
    return null;
  }
  const canonical = {
    schemaVersion: WATCHER_ROLLBACK_INCIDENT_V1_SCHEMA_VERSION,
    reasonCode: record.reasonCode as WatcherFinalityIncidentV1["reasonCode"],
    policyDigest: record.policyDigest as string,
    releaseEvidenceDigest: record.releaseEvidenceDigest as string,
    deploymentMarker,
    finalityStateDigest: record.finalityStateDigest as string,
    finalityIncidentDigest: record.finalityIncidentDigest as string,
    triggerConsistencyDigest: record.triggerConsistencyDigest as string | null,
    finalizedBinding,
    sourceStoreDigest: record.sourceStoreDigest as string,
    nextStoreDigest: record.nextStoreDigest as string,
    transitionCount: record.transitionCount,
    previousFinalityStateDigest: record.previousFinalityStateDigest as string,
    consistencyDigest: record.consistencyDigest as string,
    finalityResultDigest: record.finalityResultDigest as string,
  };
  if (
    canonical.triggerConsistencyDigest !== canonical.consistencyDigest ||
    canonical.finalityIncidentDigest !==
      sha256Canonical({
        reasonCode: canonical.reasonCode,
        triggerConsistencyDigest: canonical.triggerConsistencyDigest,
        priorStateDigest: canonical.previousFinalityStateDigest,
      }) ||
    sha256Canonical(canonical) !== record.incidentDigest
  ) {
    return null;
  }
  return Object.freeze({
    ...canonical,
    incidentDigest: record.incidentDigest as string,
  });
};

const parseTransitionRecord = (
  value: unknown,
): WatcherRollbackTransitionV1 | null => {
  const record = exactPlainRecord(value, [
    "schemaVersion",
    "previousFinalityState",
    "consistency",
    "finalityResult",
    "transitionDigest",
  ]);
  if (
    record === null ||
    record.schemaVersion !== WATCHER_ROLLBACK_TRANSITION_V1_SCHEMA_VERSION ||
    typeof record.transitionDigest !== "string" ||
    !HEX_32.test(record.transitionDigest)
  ) {
    return null;
  }
  const previousFinalityState = parseWatcherFinalityStateV1(
    record.previousFinalityState,
  );
  const consistency = exactPlainRecord(record.consistency, [
    "schemaVersion",
    "status",
    "protocolDecision",
    "sourceMode",
    "configuredNetwork",
    "configuredSourceDigest",
    "authorityNodeId",
    "authorityGenesisIdentitySha256",
    "chainAuthorityObservationDigest",
    "queryObservationCount",
    "observationCount",
    "independentProviderCount",
    "externalProviderBindings",
    "localQueryServiceBindings",
    "reasonCodes",
    "alertCodes",
    "observationEvidenceDigests",
    "rejectedObservationCount",
    "agreement",
    "consistencyDigest",
  ]);
  const finalityResult = exactPlainRecord(record.finalityResult, [
    "schemaVersion",
    "action",
    "protocolDecision",
    "reasonCodes",
    "alertCodes",
    "state",
    "rewindInstruction",
    "resultDigest",
  ]);
  if (
    previousFinalityState === null ||
    consistency === null ||
    finalityResult === null
  ) {
    return null;
  }
  const canonical = {
    schemaVersion: WATCHER_ROLLBACK_TRANSITION_V1_SCHEMA_VERSION,
    previousFinalityState,
    consistency: consistency as WatcherMultiProviderConsistencyV1,
    finalityResult: finalityResult as unknown as WatcherFinalityResultV1,
  };
  if (sha256Canonical(canonical) !== record.transitionDigest) {
    return null;
  }
  return Object.freeze({
    ...canonical,
    transitionDigest: record.transitionDigest,
  });
};

const parseEpochCheckpoint = (
  value: unknown,
): WatcherRollbackEpochCheckpointV1 | null => {
  const record = exactPlainRecord(value, [
    "schemaVersion",
    "epoch",
    "rootBootstrapStateDigest",
    "priorCheckpointDigest",
    "priorTerminalStateDigest",
    "priorTerminalTransitionCount",
    "priorTerminalTransitionLineageDigest",
    "priorTerminalStoreDigest",
    "priorTerminalFinalityStateDigest",
    "priorTerminalIncidentDigest",
    "recoveryStateDigest",
    "recoveryLifecycleDigest",
    "checkpointStoreDigest",
    "checkpointFinalityStateDigest",
    "checkpointDigest",
  ]);
  if (
    record === null ||
    record.schemaVersion !==
      WATCHER_ROLLBACK_EPOCH_CHECKPOINT_V1_SCHEMA_VERSION ||
    typeof record.epoch !== "string" ||
    !/^[1-9][0-9]*$/u.test(record.epoch) ||
    typeof record.priorTerminalTransitionCount !== "string" ||
    !CANONICAL_NATURAL.test(record.priorTerminalTransitionCount) ||
    ![
      record.rootBootstrapStateDigest,
      record.priorTerminalStateDigest,
      record.priorTerminalTransitionLineageDigest,
      record.priorTerminalStoreDigest,
      record.priorTerminalFinalityStateDigest,
      record.checkpointStoreDigest,
      record.checkpointFinalityStateDigest,
      record.checkpointDigest,
    ].every((digest) => typeof digest === "string" && HEX_32.test(digest)) ||
    ![
      record.priorCheckpointDigest,
      record.priorTerminalIncidentDigest,
      record.recoveryStateDigest,
      record.recoveryLifecycleDigest,
    ].every(
      (digest) =>
        digest === null || (typeof digest === "string" && HEX_32.test(digest)),
    ) ||
    (record.recoveryStateDigest === null) !==
      (record.recoveryLifecycleDigest === null) ||
    (record.priorCheckpointDigest === null) !== (record.epoch === "1") ||
    (record.recoveryStateDigest === null
      ? record.priorTerminalIncidentDigest !== null ||
        record.checkpointStoreDigest !== record.priorTerminalStoreDigest ||
        record.checkpointFinalityStateDigest !==
          record.priorTerminalFinalityStateDigest
      : record.priorTerminalIncidentDigest === null)
  ) {
    return null;
  }
  const canonical = {
    schemaVersion: WATCHER_ROLLBACK_EPOCH_CHECKPOINT_V1_SCHEMA_VERSION,
    epoch: record.epoch,
    rootBootstrapStateDigest: record.rootBootstrapStateDigest as string,
    priorCheckpointDigest: record.priorCheckpointDigest as string | null,
    priorTerminalStateDigest: record.priorTerminalStateDigest as string,
    priorTerminalTransitionCount: record.priorTerminalTransitionCount,
    priorTerminalTransitionLineageDigest:
      record.priorTerminalTransitionLineageDigest as string,
    priorTerminalStoreDigest: record.priorTerminalStoreDigest as string,
    priorTerminalFinalityStateDigest:
      record.priorTerminalFinalityStateDigest as string,
    priorTerminalIncidentDigest: record.priorTerminalIncidentDigest as
      | string
      | null,
    recoveryStateDigest: record.recoveryStateDigest as string | null,
    recoveryLifecycleDigest: record.recoveryLifecycleDigest as string | null,
    checkpointStoreDigest: record.checkpointStoreDigest as string,
    checkpointFinalityStateDigest:
      record.checkpointFinalityStateDigest as string,
  };
  return sha256Canonical(canonical) === record.checkpointDigest
    ? Object.freeze({
        ...canonical,
        checkpointDigest: record.checkpointDigest as string,
      })
    : null;
};

const decodeWatcherRollbackStateV1Structural = (
  value: unknown,
): WatcherRollbackStateV1 | null => {
  try {
    const record = exactPlainRecord(value, [
      "schemaVersion",
      "policyDigest",
      "network",
      "releaseEvidenceDigest",
      "deploymentMarker",
      "bootstrapStore",
      "bootstrapFinalityState",
      "epoch",
      "epochCheckpoint",
      "transitions",
      "storeDigest",
      "transitionCount",
      "currentFinalityStateDigest",
      "lastPreviousFinalityStateDigest",
      "lastConsistencyDigest",
      "lastFinalityResultDigest",
      "lastInstructionDigest",
      "transitionLineageDigest",
      "incident",
      "stateDigest",
    ]);
    if (
      record === null ||
      record.schemaVersion !== WATCHER_ROLLBACK_STATE_V1_SCHEMA_VERSION ||
      typeof record.policyDigest !== "string" ||
      !HEX_32.test(record.policyDigest) ||
      !NETWORKS.includes(record.network as (typeof NETWORKS)[number]) ||
      typeof record.releaseEvidenceDigest !== "string" ||
      !HEX_32.test(record.releaseEvidenceDigest) ||
      typeof record.storeDigest !== "string" ||
      !HEX_32.test(record.storeDigest) ||
      typeof record.epoch !== "string" ||
      !CANONICAL_NATURAL.test(record.epoch) ||
      typeof record.transitionCount !== "string" ||
      !CANONICAL_NATURAL.test(record.transitionCount) ||
      ![
        record.currentFinalityStateDigest,
        record.lastPreviousFinalityStateDigest,
        record.lastConsistencyDigest,
        record.lastFinalityResultDigest,
        record.lastInstructionDigest,
      ].every(
        (member) =>
          member === null ||
          (typeof member === "string" && HEX_32.test(member)),
      ) ||
      typeof record.transitionLineageDigest !== "string" ||
      !HEX_32.test(record.transitionLineageDigest) ||
      typeof record.stateDigest !== "string" ||
      !HEX_32.test(record.stateDigest)
    ) {
      return null;
    }
    const deploymentMarker = marker(record.deploymentMarker);
    let bootstrapStore: WatcherDurableStoreV1;
    try {
      bootstrapStore = parseWatcherDurableStoreV1(record.bootstrapStore);
    } catch {
      return null;
    }
    const bootstrapFinalityState = parseWatcherFinalityStateV1(
      record.bootstrapFinalityState,
    );
    const epochCheckpoint =
      record.epochCheckpoint === null
        ? null
        : parseEpochCheckpoint(record.epochCheckpoint);
    const transitionInputs = exactArray(record.transitions);
    if (
      bootstrapFinalityState === null ||
      transitionInputs === null ||
      transitionInputs.length > WATCHER_ROLLBACK_V1_BOUNDS.transitionHistory
    ) {
      return null;
    }
    const transitions = transitionInputs.map(parseTransitionRecord);
    const parsedIncident =
      record.incident === null ? null : parseRollbackIncident(record.incident);
    if (
      deploymentMarker === null ||
      (record.epochCheckpoint !== null && epochCheckpoint === null) ||
      transitions.some((transition) => transition === null) ||
      !sameMarker(bootstrapStore.deploymentMarker, deploymentMarker) ||
      (record.incident !== null && parsedIncident === null)
    ) {
      return null;
    }
    const canonical = {
      schemaVersion: WATCHER_ROLLBACK_STATE_V1_SCHEMA_VERSION,
      policyDigest: record.policyDigest,
      network: record.network as (typeof NETWORKS)[number],
      releaseEvidenceDigest: record.releaseEvidenceDigest,
      deploymentMarker,
      bootstrapStore,
      bootstrapFinalityState,
      epoch: record.epoch,
      epochCheckpoint,
      transitions: Object.freeze(
        transitions as readonly WatcherRollbackTransitionV1[],
      ),
      storeDigest: record.storeDigest,
      transitionCount: record.transitionCount,
      currentFinalityStateDigest: record.currentFinalityStateDigest as
        | string
        | null,
      lastPreviousFinalityStateDigest:
        record.lastPreviousFinalityStateDigest as string | null,
      lastConsistencyDigest: record.lastConsistencyDigest as string | null,
      lastFinalityResultDigest: record.lastFinalityResultDigest as
        | string
        | null,
      lastInstructionDigest: record.lastInstructionDigest as string | null,
      transitionLineageDigest: record.transitionLineageDigest,
      incident: parsedIncident,
    };
    const count = BigInt(canonical.transitionCount);
    const epochTransitionCount = BigInt(canonical.transitions.length);
    const priorTransitionCount = BigInt(
      canonical.epochCheckpoint?.priorTerminalTransitionCount ?? "0",
    );
    if (
      count !== priorTransitionCount + epochTransitionCount ||
      BigInt(canonical.epoch) !==
        BigInt(canonical.epochCheckpoint?.epoch ?? "0") ||
      (canonical.epochCheckpoint === null) !== (canonical.epoch === "0") ||
      (canonical.epochCheckpoint !== null &&
        (canonical.epochCheckpoint.checkpointStoreDigest !==
          storeDigest(canonical.bootstrapStore) ||
          canonical.epochCheckpoint.checkpointFinalityStateDigest !==
            canonical.bootstrapFinalityState.stateDigest))
    ) {
      return null;
    }
    const initialShape =
      count === 0n &&
      canonical.epoch === "0" &&
      canonical.currentFinalityStateDigest === null &&
      canonical.lastPreviousFinalityStateDigest === null &&
      canonical.lastConsistencyDigest === null &&
      canonical.lastFinalityResultDigest === null &&
      canonical.lastInstructionDigest === null &&
      canonical.incident === null &&
      canonical.transitionLineageDigest ===
        sha256Canonical({
          schemaVersion: WATCHER_ROLLBACK_STATE_V1_SCHEMA_VERSION,
          kind: "genesis",
          policyDigest: canonical.policyDigest,
          network: canonical.network,
          releaseEvidenceDigest: canonical.releaseEvidenceDigest,
          deploymentMarker: canonical.deploymentMarker,
        });
    const checkpointShape =
      canonical.epochCheckpoint !== null &&
      epochTransitionCount === 0n &&
      canonical.currentFinalityStateDigest ===
        canonical.epochCheckpoint.checkpointFinalityStateDigest &&
      canonical.lastPreviousFinalityStateDigest === null &&
      canonical.lastConsistencyDigest === null &&
      canonical.lastFinalityResultDigest === null &&
      canonical.lastInstructionDigest === null &&
      canonical.incident === null &&
      canonical.transitionLineageDigest ===
        canonical.epochCheckpoint.priorTerminalTransitionLineageDigest;
    const transitionedShape =
      epochTransitionCount > 0n &&
      canonical.currentFinalityStateDigest !== null &&
      canonical.lastPreviousFinalityStateDigest !== null &&
      canonical.lastConsistencyDigest !== null &&
      canonical.lastFinalityResultDigest !== null &&
      ((canonical.incident === null &&
        canonical.lastInstructionDigest !== null) ||
        (canonical.incident !== null &&
          canonical.lastInstructionDigest === null));
    if (!initialShape && !checkpointShape && !transitionedShape) {
      return null;
    }
    if (
      parsedIncident !== null &&
      (parsedIncident.policyDigest !== canonical.policyDigest ||
        parsedIncident.releaseEvidenceDigest !==
          canonical.releaseEvidenceDigest ||
        !sameMarker(
          parsedIncident.deploymentMarker,
          canonical.deploymentMarker,
        ) ||
        parsedIncident.nextStoreDigest !== canonical.storeDigest ||
        parsedIncident.transitionCount !== canonical.transitionCount ||
        parsedIncident.previousFinalityStateDigest !==
          canonical.lastPreviousFinalityStateDigest ||
        parsedIncident.consistencyDigest !== canonical.lastConsistencyDigest ||
        parsedIncident.finalityResultDigest !==
          canonical.lastFinalityResultDigest ||
        parsedIncident.finalityStateDigest !==
          canonical.currentFinalityStateDigest)
    ) {
      return null;
    }
    if (sha256Canonical(canonical) !== record.stateDigest) {
      return null;
    }
    const state = Object.freeze({
      ...canonical,
      stateDigest: record.stateDigest,
    }) as WatcherRollbackStateV1;
    return state;
  } catch {
    return null;
  }
};

const parseRemovedRecords = (
  value: unknown,
): WatcherRollbackRemovedRecordsV1 | null => {
  const keys = [
    "l1ObservationIds",
    "chainPointIds",
    "protocolUtxoOutRefs",
    "daProofInputIds",
    "reconstructedBlockHashes",
    "decisionBlockHashes",
    "faultIds",
    "submissionIds",
    "confirmationIds",
    "retryIds",
    "deadlineIds",
    "correctionResultIds",
  ] as const;
  const record = exactPlainRecord(value, keys);
  if (record === null) {
    return null;
  }
  const parsed = {} as Record<(typeof keys)[number], readonly string[]>;
  for (const key of keys) {
    const array = exactUnrestrictedStringArray(record[key]);
    if (
      array === null ||
      array.some(
        (member) =>
          typeof member !== "string" ||
          (key === "protocolUtxoOutRefs"
            ? !/^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u.test(member)
            : !HEX_32.test(member)),
      )
    ) {
      return null;
    }
    const strings = array;
    if (
      new Set(strings).size !== strings.length ||
      !sameStrings(strings, [...strings].sort())
    ) {
      return null;
    }
    parsed[key] = Object.freeze([...strings]);
  }
  return Object.freeze(parsed) as WatcherRollbackRemovedRecordsV1;
};

const nullableNatural = (value: unknown): value is string | null =>
  value === null ||
  (typeof value === "string" && CANONICAL_NATURAL.test(value));

const nullableDigest = (value: unknown): value is string | null =>
  value === null || (typeof value === "string" && HEX_32.test(value));

const decodeWatcherRollbackResultV1Structural = (
  value: unknown,
): WatcherRollbackResultV1 | null => {
  try {
    const record = exactPlainRecord(value, [
      "schemaVersion",
      "action",
      "protocolDecision",
      "reasonCodes",
      "alertCodes",
      "sourceRevision",
      "nextRevision",
      "instructionDigest",
      "sourceStoreDigest",
      "nextStoreDigest",
      "removedRecords",
      "nextStore",
      "rollbackState",
      "rollbackBootstrapState",
      "trustedCheckpointStateDigest",
      "resultDigest",
    ]);
    if (
      record === null ||
      record.schemaVersion !== WATCHER_ROLLBACK_RESULT_V1_SCHEMA_VERSION ||
      ![
        "apply_rewind",
        "duplicate_rewind",
        "quarantine_incident",
        "reject",
      ].includes(record.action as string) ||
      !["resume_pending", "hold", "quarantined"].includes(
        record.protocolDecision as string,
      ) ||
      !nullableNatural(record.sourceRevision) ||
      !nullableNatural(record.nextRevision) ||
      !nullableDigest(record.instructionDigest) ||
      !nullableDigest(record.sourceStoreDigest) ||
      !nullableDigest(record.nextStoreDigest) ||
      !nullableDigest(record.trustedCheckpointStateDigest) ||
      typeof record.resultDigest !== "string" ||
      !HEX_32.test(record.resultDigest)
    ) {
      return null;
    }
    const reasonCodes = exactStringArray(
      record.reasonCodes,
      WATCHER_ROLLBACK_REASON_CODES_V1,
    ) as readonly WatcherRollbackReasonCodeV1[] | null;
    const alertCodes = exactStringArray(
      record.alertCodes,
      WATCHER_ROLLBACK_ALERT_CODES_V1,
    ) as readonly WatcherRollbackAlertCodeV1[] | null;
    const removedRecords = parseRemovedRecords(record.removedRecords);
    let nextStore: WatcherDurableStoreV1 | null = null;
    if (record.nextStore !== null) {
      try {
        nextStore = parseWatcherDurableStoreV1(record.nextStore);
      } catch {
        return null;
      }
    }
    const rollbackState =
      record.rollbackState === null
        ? null
        : decodeWatcherRollbackStateV1Structural(record.rollbackState);
    const rollbackBootstrapState =
      record.rollbackBootstrapState === null
        ? null
        : decodeWatcherRollbackStateV1Structural(record.rollbackBootstrapState);
    if (
      reasonCodes === null ||
      alertCodes === null ||
      removedRecords === null ||
      (record.rollbackState !== null && rollbackState === null) ||
      (record.rollbackBootstrapState !== null &&
        rollbackBootstrapState === null)
    ) {
      return null;
    }
    if (
      nextStore !== null &&
      (record.nextStoreDigest !== storeDigest(nextStore) ||
        record.nextRevision !== nextStore.revision)
    ) {
      return null;
    }
    if (
      rollbackState !== null &&
      rollbackState.storeDigest !== record.nextStoreDigest
    ) {
      return null;
    }
    if (
      rollbackBootstrapState !== null &&
      record.trustedCheckpointStateDigest !==
        trustedCheckpointStateDigest(rollbackBootstrapState)
    ) {
      return null;
    }
    const action = record.action as WatcherRollbackResultV1["action"];
    const protocolDecision =
      record.protocolDecision as WatcherRollbackResultV1["protocolDecision"];
    const hasRemovedRecords = removedRecordCount(removedRecords) > 0;
    const validSemantics =
      (action === "apply_rewind" &&
        protocolDecision === "resume_pending" &&
        sameStrings(reasonCodes, ["rewind_applied"]) &&
        sameStrings(alertCodes, ["watcher_rollback_rewind_applied"]) &&
        record.sourceRevision !== null &&
        record.nextRevision !== null &&
        BigInt(record.nextRevision) === BigInt(record.sourceRevision) + 1n &&
        record.instructionDigest !== null &&
        record.sourceStoreDigest !== null &&
        record.nextStoreDigest !== null &&
        hasRemovedRecords &&
        nextStore !== null &&
        rollbackState !== null &&
        rollbackBootstrapState !== null &&
        rollbackState.incident === null &&
        rollbackState.lastInstructionDigest === record.instructionDigest) ||
      (action === "duplicate_rewind" &&
        protocolDecision === "hold" &&
        sameStrings(reasonCodes, ["duplicate_instruction"]) &&
        alertCodes.length === 0 &&
        record.sourceRevision === record.nextRevision &&
        record.sourceStoreDigest === record.nextStoreDigest &&
        record.instructionDigest !== null &&
        !hasRemovedRecords &&
        nextStore !== null &&
        rollbackState !== null &&
        rollbackBootstrapState !== null &&
        rollbackState.incident === null &&
        rollbackState.lastInstructionDigest === record.instructionDigest) ||
      (action === "quarantine_incident" &&
        protocolDecision === "quarantined" &&
        sameStrings(reasonCodes, ["post_finality_incident"]) &&
        sameStrings(alertCodes, ["watcher_rollback_post_finality_incident"]) &&
        record.sourceRevision !== null &&
        record.nextRevision !== null &&
        BigInt(record.nextRevision) === BigInt(record.sourceRevision) + 1n &&
        record.sourceStoreDigest !== null &&
        record.nextStoreDigest !== null &&
        record.instructionDigest === null &&
        !hasRemovedRecords &&
        nextStore !== null &&
        rollbackState !== null &&
        rollbackBootstrapState !== null &&
        rollbackState.incident !== null) ||
      (action === "reject" &&
        protocolDecision === "quarantined" &&
        !hasRemovedRecords &&
        ((sameStrings(reasonCodes, ["state_quarantined"]) &&
          sameStrings(alertCodes, ["watcher_rollback_quarantined"]) &&
          record.sourceRevision === record.nextRevision &&
          record.sourceStoreDigest === record.nextStoreDigest &&
          nextStore !== null &&
          rollbackState !== null &&
          rollbackBootstrapState !== null &&
          rollbackState.incident !== null) ||
          (nextStore === null &&
            rollbackState === null &&
            record.sourceRevision === null &&
            record.nextRevision === null &&
            record.instructionDigest === null &&
            record.sourceStoreDigest === null &&
            record.nextStoreDigest === null &&
            rollbackBootstrapState === null)));
    if (!validSemantics) {
      return null;
    }
    const canonical = {
      schemaVersion: WATCHER_ROLLBACK_RESULT_V1_SCHEMA_VERSION,
      action,
      protocolDecision,
      reasonCodes,
      alertCodes,
      sourceRevision: record.sourceRevision as string | null,
      nextRevision: record.nextRevision as string | null,
      instructionDigest: record.instructionDigest as string | null,
      sourceStoreDigest: record.sourceStoreDigest as string | null,
      nextStoreDigest: record.nextStoreDigest as string | null,
      removedRecords,
      nextStore,
      rollbackState,
      rollbackBootstrapState,
      trustedCheckpointStateDigest: record.trustedCheckpointStateDigest as
        | string
        | null,
    };
    if (sha256Canonical(canonical) !== record.resultDigest) {
      return null;
    }
    return Object.freeze({
      ...canonical,
      resultDigest: record.resultDigest,
    });
  } catch {
    return null;
  }
};

const removedRecordCount = (removed: WatcherRollbackRemovedRecordsV1): number =>
  Object.values(removed).reduce((total, values) => total + values.length, 0);

const sorted = (values: Iterable<string>): readonly string[] =>
  Object.freeze([...values].sort());

type PersistedConsistencyEvidence = Readonly<{
  observationIds: ReadonlySet<string>;
  chainPointIds: ReadonlySet<string>;
  observations: readonly WatcherNormalizedL1BlockV1[];
  consistency: WatcherMultiProviderConsistencyV1;
}>;

type PersistedObservationIndexEntry = Readonly<{
  durable: WatcherDurableStoreV1["l1Observations"][number];
  point: WatcherDurableStoreV1["chainPoints"][number] | null;
  observation: WatcherNormalizedL1BlockV1;
}>;

type PersistedObservationIndex = ReadonlyMap<
  string,
  PersistedObservationIndexEntry | null
>;

const decodePersistedObservation = (
  cborHex: string,
): WatcherNormalizedL1BlockV1 | null => {
  try {
    const bytes = Buffer.from(cborHex, "hex");
    const text = new TextDecoder("utf-8", { fatal: true }).decode(bytes);
    const decoded = JSON.parse(text) as unknown;
    const root = exactPlainRecord(decoded, [
      "schemaVersion",
      "network",
      "provider",
      "chainPoint",
      "transactions",
      "blockContentDigest",
      "observationDigest",
    ]);
    if (
      root === null ||
      typeof root.observationDigest !== "string" ||
      !HEX_32.test(root.observationDigest)
    ) {
      return null;
    }
    const observation = decoded as WatcherNormalizedL1BlockV1;
    if (!bytes.equals(encodeWatcherNormalizedL1BlockV1(observation))) {
      return null;
    }
    return observation;
  } catch {
    return null;
  }
};

const indexPersistedObservations = (
  store: WatcherDurableStoreV1,
): PersistedObservationIndex => {
  const points = new Map(
    store.chainPoints.map((point) => [point.chainPointId, point] as const),
  );
  const index = new Map<string, PersistedObservationIndexEntry | null>();
  for (const durable of store.l1Observations) {
    const observation = decodePersistedObservation(durable.payload.cborHex);
    if (observation === null) {
      continue;
    }
    const digest = observation.observationDigest;
    if (index.has(digest)) {
      index.set(digest, null);
      continue;
    }
    index.set(
      digest,
      Object.freeze({
        durable,
        point: points.get(observation.chainPoint.chainPointId) ?? null,
        observation,
      }),
    );
  }
  return index;
};

const verifyPersistedConsistencyEvidence = (
  policy: WatcherFinalityPolicyV1,
  store: WatcherDurableStoreV1,
  consistency: WatcherMultiProviderConsistencyV1,
  persistedIndex: PersistedObservationIndex = indexPersistedObservations(store),
): PersistedConsistencyEvidence | null => {
  const structuralVerification = evaluateWatcherFinalityV1(
    policy,
    null,
    consistency,
  );
  if (
    structuralVerification.action !== "observe_pending" ||
    structuralVerification.state?.pending === null ||
    structuralVerification.state?.pending?.lastSeenConsistencyDigest !==
      consistency.consistencyDigest
  ) {
    return null;
  }
  const evidenceDigests = consistency.observationEvidenceDigests;
  if (
    consistency.sourceMode !== policy.sourceMode ||
    consistency.configuredNetwork !== policy.network ||
    consistency.authorityNodeId !== policy.authorityNodeId ||
    consistency.authorityGenesisIdentitySha256 !==
      policy.authorityGenesisIdentitySha256 ||
    consistency.configuredSourceDigest !==
      sha256Canonical(watcherFinalityConfiguredSourceV1(policy)) ||
    consistency.rejectedObservationCount !== 0 ||
    evidenceDigests.length === 0 ||
    consistency.observationCount !== evidenceDigests.length
  ) {
    return null;
  }
  const decoded: WatcherNormalizedL1BlockV1[] = [];
  const observationIds = new Set<string>();
  const chainPointIds = new Set<string>();
  for (const evidenceDigest of evidenceDigests) {
    const indexed = persistedIndex.get(evidenceDigest);
    if (indexed === undefined || indexed === null) {
      return null;
    }
    const { durable, observation, point } = indexed;
    if (
      durable.providerId !== observation.provider.providerId ||
      durable.chainPointId !== observation.chainPoint.chainPointId ||
      point === null ||
      point.providerId !== observation.provider.providerId ||
      point.blockHash !== observation.chainPoint.blockHash ||
      point.slot !== observation.chainPoint.slot ||
      point.blockNo !== observation.chainPoint.blockNo ||
      point.depth !== observation.chainPoint.depth
    ) {
      return null;
    }
    observationIds.add(durable.observationId);
    chainPointIds.add(durable.chainPointId);
    decoded.push(observation);
  }
  if (
    decoded.length !== evidenceDigests.length ||
    !sameStrings(
      sorted(decoded.map(({ observationDigest }) => observationDigest)),
      evidenceDigests,
    )
  ) {
    return null;
  }
  const recomputed = evaluateWatcherMultiProviderConsistencyV1(
    watcherFinalityConfiguredSourceV1(policy),
    decoded,
  );
  return JSON.stringify(recomputed) === JSON.stringify(consistency)
    ? Object.freeze({
        observationIds,
        chainPointIds,
        observations: Object.freeze(decoded),
        consistency: recomputed,
      })
    : null;
};

const verifyPersistedReplacementEvidence = (
  policy: WatcherFinalityPolicyV1,
  store: WatcherDurableStoreV1,
  transition: Extract<ParsedFinalityTransition, { readonly kind: "rewind" }>,
): PersistedConsistencyEvidence | null => {
  const evidence = verifyPersistedConsistencyEvidence(
    policy,
    store,
    transition.consistency,
  );
  const agreement = transition.consistency.agreement;
  const replacement = transition.next
    .pending as WatcherFinalityBoundObservationV1;
  return evidence !== null &&
    transition.consistency.status === "agreed" &&
    transition.consistency.protocolDecision === "allowed" &&
    agreement !== null &&
    agreement.pointDigest === replacement.pointDigest &&
    agreement.blockHash === replacement.blockHash &&
    agreement.slot === replacement.slot &&
    agreement.blockNo === replacement.blockNo &&
    agreement.minimumDepth === replacement.currentDepth &&
    agreement.blockContentDigest === replacement.blockContentDigest
    ? evidence
    : null;
};

const planRewind = (
  store: WatcherDurableStoreV1,
  transition: Extract<ParsedFinalityTransition, { readonly kind: "rewind" }>,
  replacementEvidence: PersistedConsistencyEvidence,
): Readonly<{
  records: WatcherDurableRecordsV1;
  removed: WatcherRollbackRemovedRecordsV1;
}> => {
  const prior = transition.previous
    .pending as WatcherFinalityBoundObservationV1;
  const replacement = transition.next
    .pending as WatcherFinalityBoundObservationV1;
  const kind = transition.instruction.kind;
  const directInvalidPointIds = new Set<string>();
  const removedPointIds = new Set<string>();
  const pivot =
    BigInt(prior.blockNo) < BigInt(replacement.blockNo)
      ? BigInt(prior.blockNo)
      : BigInt(replacement.blockNo);
  const replacementTip =
    BigInt(replacement.blockNo) + BigInt(replacement.currentDepth);

  for (const point of store.chainPoints) {
    const isReplacement =
      point.blockHash === replacement.blockHash &&
      point.slot === replacement.slot &&
      point.blockNo === replacement.blockNo &&
      BigInt(point.depth) <= BigInt(replacement.currentDepth);
    let removePoint = false;
    let invalidateDependents = false;
    if (kind === "pending_point_changed") {
      removePoint =
        BigInt(point.blockNo) >= pivot &&
        !isReplacement &&
        !replacementEvidence.chainPointIds.has(point.chainPointId);
      invalidateDependents = removePoint;
    } else if (kind === "pending_content_changed") {
      invalidateDependents =
        point.blockHash === prior.blockHash &&
        point.slot === prior.slot &&
        point.blockNo === prior.blockNo;
    } else {
      const sameAnchor =
        point.blockHash === replacement.blockHash &&
        point.slot === replacement.slot &&
        point.blockNo === replacement.blockNo;
      removePoint =
        (sameAnchor &&
          BigInt(point.depth) > BigInt(replacement.currentDepth)) ||
        BigInt(point.blockNo) > replacementTip;
      invalidateDependents =
        removePoint && !sameAnchor && BigInt(point.blockNo) > replacementTip;
    }
    if (removePoint) {
      removedPointIds.add(point.chainPointId);
      directInvalidPointIds.add(point.chainPointId);
    }
    if (invalidateDependents) {
      directInvalidPointIds.add(point.chainPointId);
    }
  }

  const removedObservationIds = new Set(
    store.l1Observations
      .filter(
        (entry) =>
          directInvalidPointIds.has(entry.chainPointId) &&
          !replacementEvidence.observationIds.has(entry.observationId),
      )
      .map((entry) => entry.observationId),
  );
  const removedUtxoOutRefs = new Set([
    ...store.protocolUtxos
      .filter((entry) => directInvalidPointIds.has(entry.chainPointId))
      .map((entry) => entry.outRef),
    ...store.spentProtocolUtxos
      .filter((entry) => directInvalidPointIds.has(entry.chainPointId))
      .map((entry) => entry.outRef),
  ]);
  const restoredProtocolUtxos = store.spentProtocolUtxos
    .filter(
      (entry) =>
        !directInvalidPointIds.has(entry.chainPointId) &&
        directInvalidPointIds.has(entry.spentAtChainPointId),
    )
    .map(({ spentAtChainPointId: _spentAtChainPointId, ...entry }) => entry);
  const retainedSpentProtocolUtxos = store.spentProtocolUtxos.filter(
    (entry) =>
      !directInvalidPointIds.has(entry.chainPointId) &&
      !directInvalidPointIds.has(entry.spentAtChainPointId),
  );
  const removedStateBlockHashes = new Set(
    store.reconstructedStates
      .filter((entry) => directInvalidPointIds.has(entry.chainPointId))
      .map((entry) => entry.blockHash),
  );
  const removedDecisionBlockHashes = new Set(
    store.decisions
      .filter((entry) => removedStateBlockHashes.has(entry.blockHash))
      .map((entry) => entry.blockHash),
  );
  const removedFaultIds = new Set(
    store.faults
      .filter((entry) => removedDecisionBlockHashes.has(entry.blockHash))
      .map((entry) => entry.faultId),
  );
  const removedSubmissionIds = new Set(
    store.submissions
      .filter((entry) => removedFaultIds.has(entry.faultId))
      .map((entry) => entry.submissionId),
  );
  const removedConfirmationIds = new Set(
    store.confirmations
      .filter(
        (entry) =>
          removedSubmissionIds.has(entry.submissionId) ||
          directInvalidPointIds.has(entry.chainPointId),
      )
      .map((entry) => entry.confirmationId),
  );
  const removedRetryIds = new Set(
    store.retries
      .filter((entry) => removedSubmissionIds.has(entry.submissionId))
      .map((entry) => entry.retryId),
  );
  const removedDeadlineIds = new Set(
    store.deadlines
      .filter(
        (entry) =>
          (entry.subjectKind === "fault" &&
            removedFaultIds.has(entry.subjectId)) ||
          (entry.subjectKind === "submission" &&
            removedSubmissionIds.has(entry.subjectId)),
      )
      .map((entry) => entry.deadlineId),
  );
  const removedCorrectionResultIds = new Set(
    store.correctionResults
      .filter(
        (entry) =>
          removedFaultIds.has(entry.faultId) ||
          removedConfirmationIds.has(entry.confirmationId),
      )
      .map((entry) => entry.correctionId),
  );
  const retainedStates = store.reconstructedStates.filter(
    (entry) => !removedStateBlockHashes.has(entry.blockHash),
  );
  const retainedInputIds = new Set(
    retainedStates.flatMap((entry) => [...entry.inputIds]),
  );
  const removedStateInputIds = new Set(
    store.reconstructedStates
      .filter((entry) => removedStateBlockHashes.has(entry.blockHash))
      .flatMap((entry) => [...entry.inputIds])
      .filter((inputId) => !retainedInputIds.has(inputId)),
  );
  const removed = Object.freeze({
    l1ObservationIds: sorted(removedObservationIds),
    chainPointIds: sorted(removedPointIds),
    protocolUtxoOutRefs: sorted(removedUtxoOutRefs),
    daProofInputIds: sorted(removedStateInputIds),
    reconstructedBlockHashes: sorted(removedStateBlockHashes),
    decisionBlockHashes: sorted(removedDecisionBlockHashes),
    faultIds: sorted(removedFaultIds),
    submissionIds: sorted(removedSubmissionIds),
    confirmationIds: sorted(removedConfirmationIds),
    retryIds: sorted(removedRetryIds),
    deadlineIds: sorted(removedDeadlineIds),
    correctionResultIds: sorted(removedCorrectionResultIds),
  });
  return Object.freeze({
    removed,
    records: {
      l1Observations: store.l1Observations.filter(
        (entry) => !removedObservationIds.has(entry.observationId),
      ),
      chainPoints: store.chainPoints.filter(
        (entry) => !removedPointIds.has(entry.chainPointId),
      ),
      protocolUtxos: [
        ...store.protocolUtxos.filter(
          (entry) => !removedUtxoOutRefs.has(entry.outRef),
        ),
        ...restoredProtocolUtxos,
      ].sort((left, right) => left.outRef.localeCompare(right.outRef)),
      spentProtocolUtxos: retainedSpentProtocolUtxos,
      daProofInputs: store.daProofInputs.filter(
        (entry) => !removedStateInputIds.has(entry.inputId),
      ),
      reconstructedStates: retainedStates,
      decisions: store.decisions.filter(
        (entry) => !removedDecisionBlockHashes.has(entry.blockHash),
      ),
      faults: store.faults.filter(
        (entry) => !removedFaultIds.has(entry.faultId),
      ),
      submissions: store.submissions.filter(
        (entry) => !removedSubmissionIds.has(entry.submissionId),
      ),
      confirmations: store.confirmations.filter(
        (entry) => !removedConfirmationIds.has(entry.confirmationId),
      ),
      retries: store.retries.filter(
        (entry) => !removedRetryIds.has(entry.retryId),
      ),
      deadlines: store.deadlines.filter(
        (entry) => !removedDeadlineIds.has(entry.deadlineId),
      ),
      correctionResults: store.correctionResults.filter(
        (entry) => !removedCorrectionResultIds.has(entry.correctionId),
      ),
    },
  });
};

const storeDigest = (store: WatcherDurableStoreV1): string =>
  watcherDurableStoreBytesSha256(encodeWatcherDurableStoreV1(store));

const rollbackStateBindingFailure = (
  policy: WatcherFinalityPolicyV1,
  state: WatcherRollbackStateV1,
): WatcherRollbackReasonCodeV1 | null => {
  if (state.network !== policy.network) {
    return "network_mismatch";
  }
  if (state.releaseEvidenceDigest !== policy.releaseEvidenceDigest) {
    return "release_evidence_mismatch";
  }
  if (!sameMarker(state.deploymentMarker, policy.deploymentMarker)) {
    return "deployment_mismatch";
  }
  return state.policyDigest === policy.policyDigest ? null : "policy_mismatch";
};

const trustedCheckpointStateDigest = (
  bootstrapState: WatcherRollbackStateV1,
): string | null =>
  bootstrapState.epoch === "0" ? null : bootstrapState.stateDigest;

const evaluateWatcherRollbackStep = (
  policy: WatcherFinalityPolicyV1,
  store: WatcherDurableStoreV1,
  rollbackState: WatcherRollbackStateV1,
  rollbackBootstrapState: WatcherRollbackStateV1,
  previousFinalityStateInput: unknown,
  consistencyInput: unknown,
  finalityResultInput: unknown,
): WatcherRollbackResultV1 => {
  const sourceStoreDigest = storeDigest(store);
  if (rollbackState.incident !== null) {
    return makeResult({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["state_quarantined"],
      alertCodes: ["watcher_rollback_quarantined"],
      sourceRevision: store.revision,
      nextRevision: store.revision,
      instructionDigest: null,
      sourceStoreDigest,
      nextStoreDigest: sourceStoreDigest,
      removedRecords: emptyRemovedRecords(),
      nextStore: store,
      rollbackState,
      rollbackBootstrapState,
      trustedCheckpointStateDigest: trustedCheckpointStateDigest(
        rollbackBootstrapState,
      ),
    });
  }

  const transition = parseFinalityTransition(
    policy,
    previousFinalityStateInput,
    consistencyInput,
    finalityResultInput,
  );
  if (typeof transition === "string") {
    const bindingReasons: readonly WatcherRollbackReasonCodeV1[] = [
      "deployment_mismatch",
      "release_evidence_mismatch",
      "network_mismatch",
      "policy_mismatch",
    ];
    return reject(
      transition,
      bindingReasons.includes(transition)
        ? "watcher_rollback_configuration_mismatch"
        : "watcher_rollback_input_rejected",
    );
  }

  const adjacentDuplicate =
    BigInt(rollbackState.transitionCount) > 0n &&
    rollbackState.currentFinalityStateDigest === transition.next.stateDigest &&
    rollbackState.lastPreviousFinalityStateDigest ===
      transition.previous.stateDigest &&
    rollbackState.lastConsistencyDigest ===
      transition.consistency.consistencyDigest &&
    rollbackState.lastFinalityResultDigest ===
      transition.finalityResult.resultDigest &&
    rollbackState.lastInstructionDigest ===
      (transition.kind === "rewind"
        ? transition.instruction.instructionDigest
        : null);
  if (
    !adjacentDuplicate &&
    (BigInt(rollbackState.transitionCount) === 0n
      ? rollbackState.bootstrapFinalityState.stateDigest
      : rollbackState.currentFinalityStateDigest) !==
      transition.previous.stateDigest
  ) {
    return reject("stale_finality_state", "watcher_rollback_state_rejected");
  }
  if (transition.kind === "incident") {
    const consistencyEvidence = verifyPersistedConsistencyEvidence(
      policy,
      store,
      transition.consistency,
    );
    if (consistencyEvidence === null) {
      return reject("consistency_evidence_missing");
    }
    const canonicalTransition = Object.freeze({
      ...transition,
      consistency: consistencyEvidence.consistency,
    });
    const transitionCount = (
      BigInt(rollbackState.transitionCount) + 1n
    ).toString();
    const nextStore = makeWatcherDurableStoreV1({
      deploymentMarker: store.deploymentMarker,
      revision: (BigInt(store.revision) + 1n).toString(),
      records: store,
    });
    const nextStoreDigest = storeDigest(nextStore);
    const incident = makeIncident(
      policy,
      canonicalTransition,
      sourceStoreDigest,
      nextStoreDigest,
      transitionCount,
    );
    const nextRollback = advanceRollbackState(
      policy,
      rollbackState,
      rollbackBootstrapState,
      canonicalTransition,
      store,
      nextStoreDigest,
      incident,
    );
    return makeResult({
      action: "quarantine_incident",
      protocolDecision: "quarantined",
      reasonCodes: ["post_finality_incident"],
      alertCodes: ["watcher_rollback_post_finality_incident"],
      sourceRevision: store.revision,
      nextRevision: nextStore.revision,
      instructionDigest: null,
      sourceStoreDigest,
      nextStoreDigest,
      removedRecords: emptyRemovedRecords(),
      nextStore,
      rollbackState: nextRollback.state,
      rollbackBootstrapState: nextRollback.bootstrapState,
      trustedCheckpointStateDigest: trustedCheckpointStateDigest(
        nextRollback.bootstrapState,
      ),
    });
  }

  const instructionDigest = transition.instruction.instructionDigest;
  const replacementEvidence = verifyPersistedReplacementEvidence(
    policy,
    store,
    transition,
  );
  if (replacementEvidence === null) {
    return reject("replacement_evidence_missing");
  }
  const canonicalTransition = Object.freeze({
    ...transition,
    consistency: replacementEvidence.consistency,
  });
  if (adjacentDuplicate) {
    return makeResult({
      action: "duplicate_rewind",
      protocolDecision: "hold",
      reasonCodes: ["duplicate_instruction"],
      alertCodes: [],
      sourceRevision: store.revision,
      nextRevision: store.revision,
      instructionDigest,
      sourceStoreDigest,
      nextStoreDigest: sourceStoreDigest,
      removedRecords: emptyRemovedRecords(),
      nextStore: store,
      rollbackState,
      rollbackBootstrapState,
      trustedCheckpointStateDigest: trustedCheckpointStateDigest(
        rollbackBootstrapState,
      ),
    });
  }

  const plan = planRewind(store, canonicalTransition, replacementEvidence);
  if (
    plan.removed.l1ObservationIds.some((id) =>
      replacementEvidence.observationIds.has(id),
    ) ||
    plan.removed.chainPointIds.some((id) =>
      replacementEvidence.chainPointIds.has(id),
    )
  ) {
    return reject("replacement_evidence_missing");
  }
  if (removedRecordCount(plan.removed) === 0) {
    return reject("unknown_rewind_target");
  }
  const nextStore = makeWatcherDurableStoreV1({
    deploymentMarker: store.deploymentMarker,
    revision: (BigInt(store.revision) + 1n).toString(),
    records: plan.records,
  });
  const nextStoreDigest = storeDigest(nextStore);
  const nextRollback = advanceRollbackState(
    policy,
    rollbackState,
    rollbackBootstrapState,
    canonicalTransition,
    store,
    nextStoreDigest,
    null,
  );
  return makeResult({
    action: "apply_rewind",
    protocolDecision: "resume_pending",
    reasonCodes: ["rewind_applied"],
    alertCodes: ["watcher_rollback_rewind_applied"],
    sourceRevision: store.revision,
    nextRevision: nextStore.revision,
    instructionDigest,
    sourceStoreDigest,
    nextStoreDigest,
    removedRecords: plan.removed,
    nextStore,
    rollbackState: nextRollback.state,
    rollbackBootstrapState: nextRollback.bootstrapState,
    trustedCheckpointStateDigest: trustedCheckpointStateDigest(
      nextRollback.bootstrapState,
    ),
  });
};

const replayWatcherRollbackState = (
  policy: WatcherFinalityPolicyV1,
  bootstrapState: WatcherRollbackStateV1,
  candidate: WatcherRollbackStateV1,
  currentStore: WatcherDurableStoreV1,
): WatcherRollbackStateV1 | null => {
  if (
    rollbackStateBindingFailure(policy, candidate) !== null ||
    !sameMarker(
      candidate.bootstrapStore.deploymentMarker,
      policy.deploymentMarker,
    )
  ) {
    return null;
  }
  if (
    candidate.epoch !== bootstrapState.epoch ||
    JSON.stringify(candidate.epochCheckpoint) !==
      JSON.stringify(bootstrapState.epochCheckpoint) ||
    JSON.stringify(candidate.bootstrapStore) !==
      JSON.stringify(bootstrapState.bootstrapStore) ||
    JSON.stringify(candidate.bootstrapFinalityState) !==
      JSON.stringify(bootstrapState.bootstrapFinalityState)
  ) {
    return null;
  }
  let derivedState = bootstrapState;
  let derivedStore = bootstrapState.bootstrapStore;
  for (const transition of candidate.transitions) {
    const result = evaluateWatcherRollbackStep(
      policy,
      derivedStore,
      derivedState,
      bootstrapState,
      transition.previousFinalityState,
      transition.consistency,
      transition.finalityResult,
    );
    if (
      !["apply_rewind", "quarantine_incident"].includes(result.action) ||
      result.nextStore === null ||
      result.rollbackState === null
    ) {
      return null;
    }
    derivedStore = result.nextStore;
    derivedState = result.rollbackState;
  }
  return JSON.stringify(derivedStore) === JSON.stringify(currentStore) &&
    JSON.stringify(derivedState) === JSON.stringify(candidate)
    ? derivedState
    : null;
};

/**
 * Creates the only valid zero-transition rollback state. Callers must persist
 * this explicit bootstrap and subsequently persist each returned state; the
 * evaluator never treats null or a compact self-hash as a fresh installation.
 */
export const makeWatcherRollbackBootstrapStateV1 = (
  policyInput: unknown,
  bootstrapStoreInput: unknown,
  bootstrapFinalityStateInput: unknown,
): WatcherRollbackStateV1 | null => {
  const policy = parseWatcherFinalityPolicyV1(policyInput);
  if (policy === null) {
    return null;
  }
  try {
    const bootstrapStore = parseWatcherDurableStoreV1(bootstrapStoreInput);
    const bootstrapFinalityState = parseWatcherFinalityStateV1(
      bootstrapFinalityStateInput,
      policy,
    );
    return sameMarker(
      bootstrapStore.deploymentMarker,
      policy.deploymentMarker,
    ) && bootstrapFinalityState !== null
      ? initialRollbackState(policy, bootstrapStore, bootstrapFinalityState)
      : null;
  } catch {
    return null;
  }
};

const parseRollbackBootstrapState = (
  policy: WatcherFinalityPolicyV1,
  value: unknown,
  trustedCheckpointStateDigestInput: unknown = undefined,
): WatcherRollbackStateV1 | null => {
  const candidate = decodeWatcherRollbackStateV1Structural(value);
  if (
    candidate === null ||
    rollbackStateBindingFailure(policy, candidate) !== null ||
    parseWatcherFinalityStateV1(candidate.bootstrapFinalityState, policy) ===
      null
  ) {
    return null;
  }
  const expected = initialRollbackState(
    policy,
    candidate.bootstrapStore,
    candidate.bootstrapFinalityState,
  );
  return candidate.epoch === "0"
    ? JSON.stringify(candidate) === JSON.stringify(expected)
      ? candidate
      : null
    : candidate.epochCheckpoint !== null &&
        candidate.transitions.length === 0 &&
        candidate.incident === null &&
        typeof trustedCheckpointStateDigestInput === "string" &&
        HEX_32.test(trustedCheckpointStateDigestInput) &&
        trustedCheckpointStateDigestInput === candidate.stateDigest
      ? candidate
      : null;
};

/**
 * Authoritative rollback-state restart parser. It deterministically replays
 * the bounded transition history from a separately persisted explicit
 * bootstrap state and requires every derived intermediate store to lead to
 * `currentStore`.
 */
export const parseWatcherRollbackStateV1 = (
  value: unknown,
  context: WatcherRollbackStateVerificationContextV1,
): WatcherRollbackStateV1 | null => {
  const candidate = decodeWatcherRollbackStateV1Structural(value);
  const policy = parseWatcherFinalityPolicyV1(context.policy);
  if (candidate === null || policy === null) {
    return null;
  }
  try {
    const bootstrapState = parseRollbackBootstrapState(
      policy,
      context.rollbackBootstrapState,
      context.trustedCheckpointStateDigest,
    );
    const currentStore = parseWatcherDurableStoreV1(context.currentStore);
    return bootstrapState === null
      ? null
      : replayWatcherRollbackState(
          policy,
          bootstrapState,
          candidate,
          currentStore,
        );
  } catch {
    return null;
  }
};

/**
 * Applies exactly one W11→W12 transition to W03's canonical durable store.
 * The explicit prior rollback state is fully replayed from bootstrap before
 * use; null cannot reset the journal after restart.
 */
export const evaluateWatcherRollbackV1 = (
  policyInput: unknown,
  storeInput: unknown,
  previousFinalityStateInput: unknown,
  consistencyInput: unknown,
  finalityResultInput: unknown,
  previousRollbackStateInput: unknown,
  rollbackBootstrapStateInput: unknown,
  trustedCheckpointStateDigestInput: unknown = undefined,
): WatcherRollbackResultV1 => {
  const policy = parseWatcherFinalityPolicyV1(policyInput);
  if (policy === null) {
    return reject(
      "malformed_policy",
      "watcher_rollback_configuration_mismatch",
    );
  }
  let store: WatcherDurableStoreV1;
  try {
    store = parseWatcherDurableStoreV1(storeInput);
  } catch {
    return reject("malformed_store");
  }
  if (!sameMarker(store.deploymentMarker, policy.deploymentMarker)) {
    return reject(
      "deployment_mismatch",
      "watcher_rollback_configuration_mismatch",
    );
  }
  const rollbackStateCandidate = decodeWatcherRollbackStateV1Structural(
    previousRollbackStateInput,
  );
  if (rollbackStateCandidate === null) {
    return reject(
      "malformed_rollback_state",
      "watcher_rollback_state_rejected",
    );
  }
  const bindingFailure = rollbackStateBindingFailure(
    policy,
    rollbackStateCandidate,
  );
  if (bindingFailure !== null) {
    return reject(bindingFailure, "watcher_rollback_configuration_mismatch");
  }
  if (rollbackStateCandidate.storeDigest !== storeDigest(store)) {
    return reject(
      "rollback_state_store_mismatch",
      "watcher_rollback_state_rejected",
    );
  }
  const rollbackBootstrapState = parseRollbackBootstrapState(
    policy,
    rollbackBootstrapStateInput,
    trustedCheckpointStateDigestInput,
  );
  if (rollbackBootstrapState === null) {
    return reject(
      "malformed_rollback_state",
      "watcher_rollback_state_rejected",
    );
  }
  const rollbackState = replayWatcherRollbackState(
    policy,
    rollbackBootstrapState,
    rollbackStateCandidate,
    store,
  );
  if (rollbackState === null) {
    return reject(
      "malformed_rollback_state",
      "watcher_rollback_state_rejected",
    );
  }
  return evaluateWatcherRollbackStep(
    policy,
    store,
    rollbackState,
    rollbackBootstrapState,
    previousFinalityStateInput,
    consistencyInput,
    finalityResultInput,
  );
};

/**
 * Authoritative restart/result verifier. Structural self-hashes are
 * insufficient: the candidate is accepted only when replaying the exact
 * policy, source store, prior W12 state, W11 decision, W12 result, and prior
 * rollback lineage reproduces every byte of the candidate result.
 */
export const parseWatcherRollbackResultV1 = (
  value: unknown,
  context: WatcherRollbackVerificationContextV1,
): WatcherRollbackResultV1 | null => {
  const parsed = decodeWatcherRollbackResultV1Structural(value);
  if (parsed === null) {
    return null;
  }
  try {
    const expected = evaluateWatcherRollbackV1(
      context.policy,
      context.sourceStore,
      context.previousFinalityState,
      context.consistency,
      context.finalityResult,
      context.previousRollbackState,
      context.rollbackBootstrapState,
      context.trustedCheckpointStateDigest,
    );
    return JSON.stringify(parsed) === JSON.stringify(expected) ? parsed : null;
  } catch {
    return null;
  }
};

type VerifiedPostFinalityPath = Readonly<{
  agreements: readonly NonNullable<
    WatcherMultiProviderConsistencyV1["agreement"]
  >[];
  evidence: readonly PersistedConsistencyEvidence[];
  consistencyDigests: readonly string[];
  observationIds: ReadonlySet<string>;
  chainPointIds: ReadonlySet<string>;
}>;

const makePostFinalityRecoveryResult = (
  value: Omit<
    WatcherPostFinalityRecoveryResultV1,
    "schemaVersion" | "resultDigest"
  >,
): WatcherPostFinalityRecoveryResultV1 => {
  const canonical = {
    schemaVersion: WATCHER_POST_FINALITY_RECOVERY_RESULT_V1_SCHEMA_VERSION,
    action: value.action,
    protocolDecision: value.protocolDecision,
    reasonCodes: Object.freeze([...value.reasonCodes]),
    sourceRevision: value.sourceRevision,
    nextRevision: value.nextRevision,
    sourceStoreDigest: value.sourceStoreDigest,
    nextStoreDigest: value.nextStoreDigest,
    removedRecords: value.removedRecords,
    nextStore: value.nextStore,
    resumableFinalityState: value.resumableFinalityState,
    resumableRollbackState: value.resumableRollbackState,
    resumableRollbackBootstrapState: value.resumableRollbackBootstrapState,
    resumableTrustedCheckpointStateDigest:
      value.resumableTrustedCheckpointStateDigest,
    recoveryState: value.recoveryState,
  };
  return Object.freeze({
    ...canonical,
    resultDigest: sha256Canonical(canonical),
  });
};

const rejectPostFinalityRecovery = (
  reason: WatcherPostFinalityRecoveryReasonCodeV1,
): WatcherPostFinalityRecoveryResultV1 =>
  makePostFinalityRecoveryResult({
    action: "reject",
    protocolDecision: "quarantined",
    reasonCodes: [reason],
    sourceRevision: null,
    nextRevision: null,
    sourceStoreDigest: null,
    nextStoreDigest: null,
    removedRecords: emptyRemovedRecords(),
    nextStore: null,
    resumableFinalityState: null,
    resumableRollbackState: null,
    resumableRollbackBootstrapState: null,
    resumableTrustedCheckpointStateDigest: null,
    recoveryState: null,
  });

const verifyPostFinalityPath = (
  policy: WatcherFinalityPolicyV1,
  store: WatcherDurableStoreV1,
  value: unknown,
  persistedIndex: PersistedObservationIndex,
): VerifiedPostFinalityPath | WatcherPostFinalityRecoveryReasonCodeV1 => {
  const inputs = exactArray(value);
  if (inputs === null || inputs.length < 2) {
    return "recovery_path_malformed";
  }
  if (
    inputs.length >
    Number(WATCHER_ROLLBACK_V1_BOUNDS.postFinalityRecoveryDepth) + 1
  ) {
    return "recovery_depth_exceeded";
  }
  const evidence: PersistedConsistencyEvidence[] = [];
  const agreements: NonNullable<
    WatcherMultiProviderConsistencyV1["agreement"]
  >[] = [];
  for (const input of inputs) {
    if (
      exactPlainRecord(input, [
        "schemaVersion",
        "status",
        "protocolDecision",
        "sourceMode",
        "configuredNetwork",
        "configuredSourceDigest",
        "authorityNodeId",
        "authorityGenesisIdentitySha256",
        "chainAuthorityObservationDigest",
        "queryObservationCount",
        "observationCount",
        "independentProviderCount",
        "externalProviderBindings",
        "localQueryServiceBindings",
        "reasonCodes",
        "alertCodes",
        "observationEvidenceDigests",
        "rejectedObservationCount",
        "agreement",
        "consistencyDigest",
      ]) === null
    ) {
      return "recovery_path_malformed";
    }
    const persisted = verifyPersistedConsistencyEvidence(
      policy,
      store,
      input as WatcherMultiProviderConsistencyV1,
      persistedIndex,
    );
    const consistency = persisted?.consistency;
    if (
      persisted === null ||
      consistency === undefined ||
      consistency.status !== "agreed" ||
      consistency.protocolDecision !== "allowed" ||
      consistency.agreement === null ||
      (policy.sourceMode === "local_node"
        ? consistency.independentProviderCount !== 1 ||
          consistency.authorityNodeId !== policy.authorityNodeId ||
          consistency.chainAuthorityObservationDigest === null ||
          consistency.localQueryServiceBindings.length !==
            policy.localQueryServices.length ||
          consistency.localQueryServiceBindings.some(
            ({ observationStatus }) => observationStatus !== "aligned",
          )
        : consistency.independentProviderCount < 2 ||
          consistency.externalProviderBindings.length < 2 ||
          new Set(
            consistency.externalProviderBindings.map(
              ({ operatorIdentitySha256 }) => operatorIdentitySha256,
            ),
          ).size !== consistency.externalProviderBindings.length)
    ) {
      return "canonical_agreement_required";
    }
    evidence.push(persisted);
    agreements.push(consistency.agreement);
  }
  for (let index = 1; index < evidence.length; index += 1) {
    const previous = agreements[index - 1]!;
    const current = agreements[index]!;
    if (
      BigInt(current.blockNo) !== BigInt(previous.blockNo) + 1n ||
      BigInt(current.slot) <= BigInt(previous.slot) ||
      evidence[index]!.observations.some(
        ({ chainPoint }) =>
          chainPoint.parentBlockHash !== previous.blockHash ||
          chainPoint.blockHash !== current.blockHash ||
          chainPoint.blockNo !== current.blockNo ||
          chainPoint.slot !== current.slot,
      )
    ) {
      return "recovery_path_gap";
    }
  }
  return Object.freeze({
    agreements: Object.freeze(agreements),
    evidence: Object.freeze(evidence),
    consistencyDigests: Object.freeze(
      evidence.map(({ consistency }) => consistency.consistencyDigest),
    ),
    observationIds: new Set(
      evidence.flatMap(({ observationIds }) => [...observationIds]),
    ),
    chainPointIds: new Set(
      evidence.flatMap(({ chainPointIds }) => [...chainPointIds]),
    ),
  });
};

const sameAgreementPoint = (
  left: NonNullable<WatcherMultiProviderConsistencyV1["agreement"]>,
  right: NonNullable<WatcherMultiProviderConsistencyV1["agreement"]>,
): boolean =>
  left.pointDigest === right.pointDigest &&
  left.blockHash === right.blockHash &&
  left.slot === right.slot &&
  left.blockNo === right.blockNo &&
  left.blockContentDigest === right.blockContentDigest;

const makeRecoveryPath = (
  previous: VerifiedPostFinalityPath,
  replacement: VerifiedPostFinalityPath,
): WatcherPostFinalityRecoveryPathV1 => {
  const commonAncestor = previous.agreements[0]!;
  const orphanedFinalized = previous.agreements.at(-1)!;
  const replacementTip = replacement.agreements.at(-1)!;
  const canonical = {
    commonAncestorPointDigest: commonAncestor.pointDigest,
    commonAncestorBlockHash: commonAncestor.blockHash,
    commonAncestorBlockNo: commonAncestor.blockNo,
    orphanedFinalizedPointDigest: orphanedFinalized.pointDigest,
    orphanedFinalizedBlockHash: orphanedFinalized.blockHash,
    replacementTipPointDigest: replacementTip.pointDigest,
    replacementTipBlockHash: replacementTip.blockHash,
    replacementTipBlockNo: replacementTip.blockNo,
    rollbackDepth: (
      BigInt(orphanedFinalized.blockNo) - BigInt(commonAncestor.blockNo)
    ).toString(),
    previousConsistencyDigests: previous.consistencyDigests,
    replacementConsistencyDigests: replacement.consistencyDigests,
  };
  return Object.freeze({
    ...canonical,
    pathDigest: sha256Canonical(canonical),
  });
};

const makeResumableFinalityState = (
  policy: WatcherFinalityPolicyV1,
): WatcherFinalityStateV1 => {
  const canonical = {
    schemaVersion: "midgard-watcher-finality-state-v1" as const,
    policyDigest: policy.policyDigest,
    network: policy.network,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    deploymentMarker: policy.deploymentMarker,
    phase: "unobserved" as const,
    pending: null,
    finalized: null,
    incident: null,
  };
  return Object.freeze({
    ...canonical,
    stateDigest: sha256Canonical(canonical),
  });
};

const makePostFinalityRecoveryState = (
  policy: WatcherFinalityPolicyV1,
  sourceRollbackState: WatcherRollbackStateV1,
  sourceStoreDigest: string,
  nextStoreDigest: string,
  path: WatcherPostFinalityRecoveryPathV1,
  removedRecords: WatcherRollbackRemovedRecordsV1,
  resumableFinalityState: WatcherFinalityStateV1,
): WatcherPostFinalityRecoveryStateV1 => {
  const detectedIncident = sourceRollbackState.incident!;
  const lifecycleCanonical = {
    detectedIncidentDigest: detectedIncident.incidentDigest,
    detectedReasonCode: detectedIncident.reasonCode,
    detectedTriggerConsistencyDigest: detectedIncident.triggerConsistencyDigest,
    detectedFinalityStateDigest: detectedIncident.finalityStateDigest,
    detectedStoreDigest: sourceStoreDigest,
    status: "recovered" as const,
    recoveryPathDigest: path.pathDigest,
    recoveredStoreDigest: nextStoreDigest,
    resumableFinalityStateDigest: resumableFinalityState.stateDigest,
  };
  const incidentLifecycle = Object.freeze({
    ...lifecycleCanonical,
    lifecycleDigest: sha256Canonical(lifecycleCanonical),
  });
  const canonical = {
    schemaVersion: WATCHER_POST_FINALITY_RECOVERY_STATE_V1_SCHEMA_VERSION,
    policyDigest: policy.policyDigest,
    network: policy.network,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    deploymentMarker: policy.deploymentMarker,
    sourceRollbackStateDigest: sourceRollbackState.stateDigest,
    sourceStoreDigest,
    nextStoreDigest,
    path,
    removedRecords,
    resumableFinalityState,
    incidentLifecycle,
  };
  return Object.freeze({
    ...canonical,
    stateDigest: sha256Canonical(canonical),
  });
};

const parseRecoveryPath = (
  value: unknown,
): WatcherPostFinalityRecoveryPathV1 | null => {
  const path = exactPlainRecord(value, [
    "commonAncestorPointDigest",
    "commonAncestorBlockHash",
    "commonAncestorBlockNo",
    "orphanedFinalizedPointDigest",
    "orphanedFinalizedBlockHash",
    "replacementTipPointDigest",
    "replacementTipBlockHash",
    "replacementTipBlockNo",
    "rollbackDepth",
    "previousConsistencyDigests",
    "replacementConsistencyDigests",
    "pathDigest",
  ]);
  if (
    path === null ||
    ![
      path.commonAncestorPointDigest,
      path.commonAncestorBlockHash,
      path.orphanedFinalizedPointDigest,
      path.orphanedFinalizedBlockHash,
      path.replacementTipPointDigest,
      path.replacementTipBlockHash,
      path.pathDigest,
    ].every((digest) => typeof digest === "string" && HEX_32.test(digest)) ||
    ![
      path.commonAncestorBlockNo,
      path.replacementTipBlockNo,
      path.rollbackDepth,
    ].every(
      (natural) =>
        typeof natural === "string" && CANONICAL_NATURAL.test(natural),
    )
  ) {
    return null;
  }
  const previousConsistencyDigests = exactUnrestrictedStringArray(
    path.previousConsistencyDigests,
  );
  const replacementConsistencyDigests = exactUnrestrictedStringArray(
    path.replacementConsistencyDigests,
  );
  if (
    previousConsistencyDigests === null ||
    replacementConsistencyDigests === null ||
    previousConsistencyDigests.length < 2 ||
    replacementConsistencyDigests.length < 2 ||
    [...previousConsistencyDigests, ...replacementConsistencyDigests].some(
      (digest) => !HEX_32.test(digest),
    )
  ) {
    return null;
  }
  const canonical = {
    commonAncestorPointDigest: path.commonAncestorPointDigest as string,
    commonAncestorBlockHash: path.commonAncestorBlockHash as string,
    commonAncestorBlockNo: path.commonAncestorBlockNo as string,
    orphanedFinalizedPointDigest: path.orphanedFinalizedPointDigest as string,
    orphanedFinalizedBlockHash: path.orphanedFinalizedBlockHash as string,
    replacementTipPointDigest: path.replacementTipPointDigest as string,
    replacementTipBlockHash: path.replacementTipBlockHash as string,
    replacementTipBlockNo: path.replacementTipBlockNo as string,
    rollbackDepth: path.rollbackDepth as string,
    previousConsistencyDigests: Object.freeze([...previousConsistencyDigests]),
    replacementConsistencyDigests: Object.freeze([
      ...replacementConsistencyDigests,
    ]),
  };
  return sha256Canonical(canonical) === path.pathDigest
    ? Object.freeze({ ...canonical, pathDigest: path.pathDigest as string })
    : null;
};

const decodePostFinalityRecoveryState = (
  value: unknown,
): WatcherPostFinalityRecoveryStateV1 | null => {
  try {
    const state = exactPlainRecord(value, [
      "schemaVersion",
      "policyDigest",
      "network",
      "releaseEvidenceDigest",
      "deploymentMarker",
      "sourceRollbackStateDigest",
      "sourceStoreDigest",
      "nextStoreDigest",
      "path",
      "removedRecords",
      "resumableFinalityState",
      "incidentLifecycle",
      "stateDigest",
    ]);
    if (
      state === null ||
      state.schemaVersion !==
        WATCHER_POST_FINALITY_RECOVERY_STATE_V1_SCHEMA_VERSION ||
      !isNetwork(state.network) ||
      ![
        state.policyDigest,
        state.releaseEvidenceDigest,
        state.sourceRollbackStateDigest,
        state.sourceStoreDigest,
        state.nextStoreDigest,
        state.stateDigest,
      ].every((digest) => typeof digest === "string" && HEX_32.test(digest))
    ) {
      return null;
    }
    const deploymentMarker = marker(state.deploymentMarker);
    const path = parseRecoveryPath(state.path);
    const removedRecords = parseRemovedRecords(state.removedRecords);
    const resumableFinalityState = parseWatcherFinalityStateV1(
      state.resumableFinalityState,
    );
    const lifecycle = exactPlainRecord(state.incidentLifecycle, [
      "detectedIncidentDigest",
      "detectedReasonCode",
      "detectedTriggerConsistencyDigest",
      "detectedFinalityStateDigest",
      "detectedStoreDigest",
      "status",
      "recoveryPathDigest",
      "recoveredStoreDigest",
      "resumableFinalityStateDigest",
      "lifecycleDigest",
    ]);
    if (
      deploymentMarker === null ||
      path === null ||
      removedRecords === null ||
      resumableFinalityState === null ||
      lifecycle === null ||
      lifecycle.status !== "recovered" ||
      lifecycle.detectedReasonCode !== "post_finality_point_changed" ||
      !(
        lifecycle.detectedTriggerConsistencyDigest === null ||
        (typeof lifecycle.detectedTriggerConsistencyDigest === "string" &&
          HEX_32.test(lifecycle.detectedTriggerConsistencyDigest))
      ) ||
      ![
        lifecycle.detectedIncidentDigest,
        lifecycle.detectedFinalityStateDigest,
        lifecycle.detectedStoreDigest,
        lifecycle.recoveryPathDigest,
        lifecycle.recoveredStoreDigest,
        lifecycle.resumableFinalityStateDigest,
        lifecycle.lifecycleDigest,
      ].every((digest) => typeof digest === "string" && HEX_32.test(digest))
    ) {
      return null;
    }
    const lifecycleCanonical = {
      detectedIncidentDigest: lifecycle.detectedIncidentDigest as string,
      detectedReasonCode:
        lifecycle.detectedReasonCode as WatcherFinalityIncidentV1["reasonCode"],
      detectedTriggerConsistencyDigest:
        lifecycle.detectedTriggerConsistencyDigest as string | null,
      detectedFinalityStateDigest:
        lifecycle.detectedFinalityStateDigest as string,
      detectedStoreDigest: lifecycle.detectedStoreDigest as string,
      status: "recovered" as const,
      recoveryPathDigest: lifecycle.recoveryPathDigest as string,
      recoveredStoreDigest: lifecycle.recoveredStoreDigest as string,
      resumableFinalityStateDigest:
        lifecycle.resumableFinalityStateDigest as string,
    };
    if (
      sha256Canonical(lifecycleCanonical) !== lifecycle.lifecycleDigest ||
      lifecycleCanonical.detectedStoreDigest !== state.sourceStoreDigest ||
      lifecycleCanonical.recoveryPathDigest !== path.pathDigest ||
      lifecycleCanonical.recoveredStoreDigest !== state.nextStoreDigest ||
      lifecycleCanonical.resumableFinalityStateDigest !==
        resumableFinalityState.stateDigest
    ) {
      return null;
    }
    const canonical = {
      schemaVersion: WATCHER_POST_FINALITY_RECOVERY_STATE_V1_SCHEMA_VERSION,
      policyDigest: state.policyDigest as string,
      network: state.network as WatcherPostFinalityRecoveryStateV1["network"],
      releaseEvidenceDigest: state.releaseEvidenceDigest as string,
      deploymentMarker,
      sourceRollbackStateDigest: state.sourceRollbackStateDigest as string,
      sourceStoreDigest: state.sourceStoreDigest as string,
      nextStoreDigest: state.nextStoreDigest as string,
      path,
      removedRecords,
      resumableFinalityState,
      incidentLifecycle: Object.freeze({
        ...lifecycleCanonical,
        lifecycleDigest: lifecycle.lifecycleDigest as string,
      }),
    };
    return sha256Canonical(canonical) === state.stateDigest
      ? Object.freeze({
          ...canonical,
          stateDigest: state.stateDigest as string,
        })
      : null;
  } catch {
    return null;
  }
};

/**
 * Resolves a durable post-finality incident only after W10 bytes and W11
 * decisions prove both sides of the exact common ancestor. The recovery is
 * one W03 revision: every dependent orphan record is removed together while
 * canonical replacement evidence remains available for deterministic replay.
 */
const evaluateWatcherPostFinalityRecoveryInternalV1 = (
  input: WatcherPostFinalityRecoveryInputV1,
): WatcherPostFinalityRecoveryResultV1 => {
  const recoveryInputKeys = [
    "policy",
    "sourceStore",
    "currentStore",
    "quarantinedRollbackState",
    "rollbackBootstrapState",
    "previousCanonicalPath",
    "replacementCanonicalPath",
    "previousRecoveryState",
  ];
  const hasTrustedCheckpointStateDigest =
    typeof input === "object" &&
    input !== null &&
    !Array.isArray(input) &&
    Reflect.ownKeys(input).includes("trustedCheckpointStateDigest");
  if (
    exactPlainRecord(
      input,
      hasTrustedCheckpointStateDigest
        ? [...recoveryInputKeys, "trustedCheckpointStateDigest"]
        : recoveryInputKeys,
    ) === null
  ) {
    return rejectPostFinalityRecovery("recovery_path_malformed");
  }
  const policy = parseWatcherFinalityPolicyV1(input.policy);
  if (policy === null) {
    return rejectPostFinalityRecovery("malformed_policy");
  }
  if (
    BigInt(policy.maximumPostFinalityRecoveryDepth) >
    WATCHER_ROLLBACK_V1_BOUNDS.postFinalityRecoveryDepth
  ) {
    return rejectPostFinalityRecovery("malformed_policy");
  }
  let sourceStore: WatcherDurableStoreV1;
  let currentStore: WatcherDurableStoreV1;
  try {
    sourceStore = parseWatcherDurableStoreV1(input.sourceStore);
    currentStore = parseWatcherDurableStoreV1(input.currentStore);
  } catch {
    return rejectPostFinalityRecovery("malformed_store");
  }
  if (
    !sameMarker(sourceStore.deploymentMarker, policy.deploymentMarker) ||
    !sameMarker(currentStore.deploymentMarker, policy.deploymentMarker)
  ) {
    return rejectPostFinalityRecovery("malformed_store");
  }
  const sourceStoreDigest = storeDigest(sourceStore);
  const rollbackState = parseWatcherRollbackStateV1(
    input.quarantinedRollbackState,
    {
      policy,
      rollbackBootstrapState: input.rollbackBootstrapState,
      trustedCheckpointStateDigest: input.trustedCheckpointStateDigest,
      currentStore: sourceStore,
    },
  );
  if (rollbackState === null) {
    return rejectPostFinalityRecovery("malformed_rollback_state");
  }
  if (
    rollbackState.storeDigest !== sourceStoreDigest ||
    rollbackState.incident === null
  ) {
    return rejectPostFinalityRecovery(
      rollbackState.incident === null
        ? "incident_required"
        : "rollback_state_store_mismatch",
    );
  }
  const persistedIndex = indexPersistedObservations(sourceStore);
  const previousPath = verifyPostFinalityPath(
    policy,
    sourceStore,
    input.previousCanonicalPath,
    persistedIndex,
  );
  if (typeof previousPath === "string") {
    return rejectPostFinalityRecovery(previousPath);
  }
  const replacementPath = verifyPostFinalityPath(
    policy,
    sourceStore,
    input.replacementCanonicalPath,
    persistedIndex,
  );
  if (typeof replacementPath === "string") {
    return rejectPostFinalityRecovery(replacementPath);
  }
  const commonAncestor = previousPath.agreements[0]!;
  const replacementAncestor = replacementPath.agreements[0]!;
  const orphanedFinalized = previousPath.agreements.at(-1)!;
  const finalizedBinding = rollbackState.incident.finalizedBinding;
  if (
    !sameAgreementPoint(commonAncestor, replacementAncestor) ||
    previousPath.agreements
      .slice(1)
      .some((oldPoint) =>
        replacementPath.agreements
          .slice(1)
          .some((newPoint) => oldPoint.blockHash === newPoint.blockHash),
      )
  ) {
    return rejectPostFinalityRecovery("common_ancestor_mismatch");
  }
  if (
    orphanedFinalized.pointDigest !== finalizedBinding.pointDigest ||
    orphanedFinalized.blockHash !== finalizedBinding.blockHash ||
    orphanedFinalized.slot !== finalizedBinding.slot ||
    orphanedFinalized.blockNo !== finalizedBinding.blockNo ||
    orphanedFinalized.blockContentDigest !==
      finalizedBinding.blockContentDigest ||
    previousPath.consistencyDigests.at(-1) !==
      finalizedBinding.lastSeenConsistencyDigest
  ) {
    return rejectPostFinalityRecovery("finalized_binding_mismatch");
  }
  if (
    rollbackState.incident.triggerConsistencyDigest === null ||
    replacementPath.consistencyDigests.at(-1) !==
      rollbackState.incident.triggerConsistencyDigest
  ) {
    return rejectPostFinalityRecovery("incident_provenance_mismatch");
  }
  const rollbackDepth =
    BigInt(orphanedFinalized.blockNo) - BigInt(commonAncestor.blockNo);
  if (
    rollbackDepth <= 0n ||
    rollbackDepth > BigInt(policy.maximumPostFinalityRecoveryDepth) ||
    BigInt(replacementPath.agreements.at(-1)!.blockNo) -
      BigInt(commonAncestor.blockNo) >
      BigInt(policy.maximumPostFinalityRecoveryDepth)
  ) {
    return rejectPostFinalityRecovery("recovery_depth_exceeded");
  }
  const path = makeRecoveryPath(previousPath, replacementPath);
  const previousRecoveryState =
    input.previousRecoveryState === null
      ? null
      : decodePostFinalityRecoveryState(input.previousRecoveryState);
  if (input.previousRecoveryState !== null && previousRecoveryState === null) {
    return rejectPostFinalityRecovery("malformed_recovery_state");
  }
  if (
    previousRecoveryState === null &&
    JSON.stringify(currentStore) !== JSON.stringify(sourceStore)
  ) {
    return rejectPostFinalityRecovery("recovery_state_mismatch");
  }
  const replacementEvidence: PersistedConsistencyEvidence = Object.freeze({
    consistency: replacementPath.evidence.at(-1)!.consistency,
    observations: Object.freeze(
      replacementPath.evidence.flatMap(({ observations }) => observations),
    ),
    observationIds: replacementPath.observationIds,
    chainPointIds: replacementPath.chainPointIds,
  });
  const firstReplacement = replacementPath.agreements[1]!;
  const syntheticTransition = {
    previous: {
      pending: {
        ...finalizedBinding,
        blockNo: (BigInt(commonAncestor.blockNo) + 1n).toString(),
      },
    },
    next: {
      pending: {
        pointDigest: firstReplacement.pointDigest,
        blockHash: firstReplacement.blockHash,
        slot: firstReplacement.slot,
        blockNo: firstReplacement.blockNo,
        blockContentDigest: firstReplacement.blockContentDigest,
        firstSeenConsistencyDigest: replacementPath.consistencyDigests[1]!,
        lastSeenConsistencyDigest: replacementPath.consistencyDigests[1]!,
        firstSeenDepth: firstReplacement.minimumDepth,
        currentDepth: firstReplacement.minimumDepth,
        visibilityCount: "1",
      },
    },
    instruction: { kind: "pending_point_changed" },
  } as unknown as Extract<
    ParsedFinalityTransition,
    { readonly kind: "rewind" }
  >;
  const plan = planRewind(
    sourceStore,
    syntheticTransition,
    replacementEvidence,
  );
  if (
    plan.removed.l1ObservationIds.some((id) =>
      replacementPath.observationIds.has(id),
    ) ||
    plan.removed.chainPointIds.some((id) =>
      replacementPath.chainPointIds.has(id),
    )
  ) {
    return rejectPostFinalityRecovery("canonical_agreement_required");
  }
  if (removedRecordCount(plan.removed) === 0) {
    return rejectPostFinalityRecovery("unknown_recovery_target");
  }
  const nextStore = makeWatcherDurableStoreV1({
    deploymentMarker: sourceStore.deploymentMarker,
    revision: (BigInt(sourceStore.revision) + 1n).toString(),
    records: plan.records,
  });
  const nextStoreDigest = storeDigest(nextStore);
  const resumableFinalityState = makeResumableFinalityState(policy);
  if (parseWatcherFinalityStateV1(resumableFinalityState, policy) === null) {
    return rejectPostFinalityRecovery("malformed_policy");
  }
  const recoveryState = makePostFinalityRecoveryState(
    policy,
    rollbackState,
    sourceStoreDigest,
    nextStoreDigest,
    path,
    plan.removed,
    resumableFinalityState,
  );
  const resumableRollbackState = makeEpochBootstrapState(
    policy,
    rollbackState,
    nextStore,
    resumableFinalityState,
    {
      stateDigest: recoveryState.stateDigest,
      lifecycleDigest: recoveryState.incidentLifecycle.lifecycleDigest,
    },
  );
  if (previousRecoveryState !== null) {
    const currentStoreDigest = storeDigest(currentStore);
    if (
      currentStoreDigest !== nextStoreDigest ||
      JSON.stringify(currentStore) !== JSON.stringify(nextStore) ||
      JSON.stringify(previousRecoveryState) !== JSON.stringify(recoveryState)
    ) {
      return rejectPostFinalityRecovery("recovery_state_mismatch");
    }
    return makePostFinalityRecoveryResult({
      action: "duplicate_recovery",
      protocolDecision: "hold",
      reasonCodes: ["duplicate_recovery"],
      sourceRevision: currentStore.revision,
      nextRevision: currentStore.revision,
      sourceStoreDigest: currentStoreDigest,
      nextStoreDigest: currentStoreDigest,
      removedRecords: emptyRemovedRecords(),
      nextStore: currentStore,
      resumableFinalityState,
      resumableRollbackState,
      resumableRollbackBootstrapState: resumableRollbackState,
      resumableTrustedCheckpointStateDigest: resumableRollbackState.stateDigest,
      recoveryState,
    });
  }
  return makePostFinalityRecoveryResult({
    action: "rewind_and_replay",
    protocolDecision: "resume_replay",
    reasonCodes: ["recovery_applied"],
    sourceRevision: sourceStore.revision,
    nextRevision: nextStore.revision,
    sourceStoreDigest,
    nextStoreDigest,
    removedRecords: plan.removed,
    nextStore,
    resumableFinalityState,
    resumableRollbackState,
    resumableRollbackBootstrapState: resumableRollbackState,
    resumableTrustedCheckpointStateDigest: resumableRollbackState.stateDigest,
    recoveryState,
  });
};

export const evaluateWatcherPostFinalityRecoveryV1 = (
  input: WatcherPostFinalityRecoveryInputV1,
): WatcherPostFinalityRecoveryResultV1 => {
  try {
    return evaluateWatcherPostFinalityRecoveryInternalV1(input);
  } catch {
    return rejectPostFinalityRecovery("recovery_path_malformed");
  }
};

const sameCanonicalStructure = (
  value: unknown,
  canonical: unknown,
  visiting: WeakSet<object> = new WeakSet<object>(),
  depth = 0,
): boolean => {
  if (
    value === null ||
    canonical === null ||
    typeof value !== "object" ||
    typeof canonical !== "object"
  ) {
    return value === canonical;
  }
  if (depth > 256 || visiting.has(value)) {
    return false;
  }
  visiting.add(value);
  try {
    if (Array.isArray(canonical)) {
      const members = exactArray(value);
      return (
        members !== null &&
        members.length === canonical.length &&
        canonical.every((expected, index) =>
          sameCanonicalStructure(members[index], expected, visiting, depth + 1),
        )
      );
    }
    if (Array.isArray(value)) {
      return false;
    }
    const canonicalRecord = canonical as Record<string, unknown>;
    const record = exactPlainRecord(value, Object.keys(canonicalRecord));
    return (
      record !== null &&
      Object.keys(canonicalRecord).every((key) =>
        sameCanonicalStructure(
          record[key],
          canonicalRecord[key],
          visiting,
          depth + 1,
        ),
      )
    );
  } finally {
    visiting.delete(value);
  }
};

/**
 * Shared W13 trust boundary for W14-W17. Candidate self-hashes are not
 * authority: the exact recovery input is replayed and only a safe,
 * byte-equivalent canonical result shape is accepted.
 */
export const parseWatcherPostFinalityRecoveryResultV1 = (
  value: unknown,
  input: WatcherPostFinalityRecoveryInputV1,
): WatcherPostFinalityRecoveryResultV1 | null => {
  try {
    const expected = evaluateWatcherPostFinalityRecoveryV1(input);
    return sameCanonicalStructure(value, expected) ? expected : null;
  } catch {
    return null;
  }
};
