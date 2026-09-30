import { type DeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";

import { WATCHER_CARDANO_SECURITY_PARAMETER_K } from "../runtime/config.js";
import { type WatcherCustomNetwork } from "../runtime/custom-network.js";

export const WATCHER_FINALITY_POLICY_SCHEMA_VERSION =
  "midgard-watcher-finality-policy-v1" as const;

export const WATCHER_FINALITY_STATE_SCHEMA_VERSION =
  "midgard-watcher-finality-state-v1" as const;

export const WATCHER_FINALITY_REWIND_INSTRUCTION_SCHEMA_VERSION =
  "midgard-watcher-finality-rewind-instruction-v1" as const;

export const WATCHER_FINALITY_RESULT_SCHEMA_VERSION =
  "midgard-watcher-finality-result-v1" as const;

export const WATCHER_FINALITY_BOUNDS = Object.freeze({
  confirmationDepth: 2_160n,
  postFinalityRecoveryDepth: BigInt(WATCHER_CARDANO_SECURITY_PARAMETER_K),
  uint64Maximum: 18_446_744_073_709_551_615n,
});

const NETWORKS = ["Mainnet", "Preprod", "Preview", "Custom"] as const;

const HEX_32 = /^[0-9a-f]{64}$/u;

export const CANONICAL_NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export const SOURCE_AUTHORITY_ID = /^[a-z][a-z0-9-]{0,62}$/u;

export type WatcherFinalityExternalProvider = Readonly<{
  providerId: string;
  operatorIdentitySha256: string;
  endpoint: string;
  authenticationKind: "https_tls_identity_v1";
}>;

export type WatcherFinalityLocalQueryService = Readonly<{
  kind: "ogmios" | "kupo" | "kupmios" | "db_sync";
  providerId: string;
  endpoint: string;
}>;

export const WATCHER_FINALITY_REASON_CODES = [
  "first_visibility_pending",
  "confirmation_depth_pending",
  "confirmation_depth_reached",
  "pending_depth_progress",
  "duplicate_observation",
  "already_finalized",
  "pending_depth_regression",
  "pending_point_changed",
  "pending_content_changed",
  "stale_observation",
  "provider_result_pending",
  "provider_result_quarantined",
  "malformed_provider_result",
  "source_mode_mismatch",
  "source_authority_mismatch",
  "source_provider_mismatch",
  "source_provider_binding_unrun",
  "malformed_policy",
  "malformed_state",
  "invalid_state_semantics",
  "stale_state",
  "configured_network_mismatch",
  "blueprint_mismatch",
  "deployment_mismatch",
  "post_finality_depth_regression",
  "post_finality_point_changed",
  "post_finality_content_changed",
  "post_finality_contradiction",
  "pre_finality_rollback_depth_exceeded",
  "state_quarantined",
] as const;

export const WATCHER_FINALITY_ALERT_CODES = [
  "watcher_finality_pending",
  "watcher_finality_input_rejected",
  "watcher_finality_rewind_required",
  "watcher_finality_rollback_limit_exceeded",
  "watcher_finality_configuration_mismatch",
  "watcher_finality_state_rejected",
  "watcher_finality_post_finality_incident",
] as const;

export type WatcherFinalityReasonCode =
  (typeof WATCHER_FINALITY_REASON_CODES)[number];

export type WatcherFinalityAlertCode =
  (typeof WATCHER_FINALITY_ALERT_CODES)[number];

export type WatcherFinalityPhase =
  | "unobserved"
  | "pending"
  | "finalized"
  | "quarantined";

export type WatcherFinalityAction =
  | "observe_pending"
  | "advance_pending"
  | "finalize"
  | "duplicate"
  | "rewind_pending"
  | "reject"
  | "quarantine_incident";

export type WatcherFinalityPolicy = Readonly<{
  customNetwork?: WatcherCustomNetwork;
  schemaVersion: typeof WATCHER_FINALITY_POLICY_SCHEMA_VERSION;
  network: (typeof NETWORKS)[number];
  sourceMode: "local_node" | "external_providers";
  authorityNodeId: string | null;
  authorityGenesisIdentitySha256: string | null;
  authorityChainSyncSocketPath: string | null;
  localQueryServices: readonly WatcherFinalityLocalQueryService[];
  externalProviders: readonly WatcherFinalityExternalProvider[] | null;
  confirmationDepth: string;
  maximumPreFinalityRollbackDepth: string;
  maximumPostFinalityRecoveryDepth: string;
  beforeFinalityRollback: "rewind";
  afterFinalityRollback: "quarantine";
  blueprintHash: string;
  deploymentMarker: DeploymentMarker;
  policyDigest: string;
}>;

export type WatcherFinalityBoundObservation = Readonly<{
  pointDigest: string;
  blockHash: string;
  slot: string;
  blockNo: string;
  blockContentDigest: string;
  firstSeenConsistencyDigest: string;
  lastSeenConsistencyDigest: string;
  firstSeenDepth: string;
  currentDepth: string;
  visibilityCount: string;
}>;

export type WatcherFinalityIncident = Readonly<{
  reasonCode: "post_finality_point_changed";
  triggerConsistencyDigest: string | null;
  incidentDigest: string;
}>;

export type WatcherFinalityState = Readonly<{
  schemaVersion: typeof WATCHER_FINALITY_STATE_SCHEMA_VERSION;
  policyDigest: string;
  network: (typeof NETWORKS)[number];
  blueprintHash: string;
  deploymentMarker: DeploymentMarker;
  phase: WatcherFinalityPhase;
  pending: WatcherFinalityBoundObservation | null;
  finalized: WatcherFinalityBoundObservation | null;
  incident: WatcherFinalityIncident | null;
  stateDigest: string;
}>;

export type WatcherFinalityRewindInstruction = Readonly<{
  schemaVersion: typeof WATCHER_FINALITY_REWIND_INSTRUCTION_SCHEMA_VERSION;
  kind:
    | "pending_depth_regression"
    | "pending_point_changed"
    | "pending_content_changed";
  discardedStateDigest: string;
  replacementPointDigest: string;
  replacementContentDigest: string;
  replacementDepth: string;
  instructionDigest: string;
}>;

export type WatcherFinalityResult = Readonly<{
  schemaVersion: typeof WATCHER_FINALITY_RESULT_SCHEMA_VERSION;
  action: WatcherFinalityAction;
  protocolDecision:
    | "hold"
    | "finality_granted"
    | "rewind_required"
    | "quarantined";
  reasonCodes: readonly WatcherFinalityReasonCode[];
  alertCodes: readonly WatcherFinalityAlertCode[];
  state: WatcherFinalityState | null;
  rewindInstruction: WatcherFinalityRewindInstruction | null;
  resultDigest: string;
}>;

type PlainRecord = Record<string, unknown>;

export type Agreement = Readonly<{
  configuredNetwork: (typeof NETWORKS)[number];
  pointDigest: string;
  blockHash: string;
  slot: string;
  blockNo: string;
  minimumDepth: string;
  blockContentDigest: string;
  consistencyDigest: string;
}>;

export type ParsedConsistency =
  | Readonly<{
      kind: "agreed";
      sourceMode: "local_node" | "external_providers";
      configuredSourceDigest: string;
      authorityNodeId: string | null;
      authorityGenesisIdentitySha256: string | null;
      authorityChainSyncSocketPath: string | null;
      externalProviderBindings: readonly ExternalProviderBinding[];
      localQueryServiceBindings: readonly LocalQueryServiceBinding[];
      agreement: Agreement;
    }>
  | Readonly<{
      kind: "pending" | "quarantined";
      sourceMode: "local_node" | "external_providers";
      configuredSourceDigest: string;
      authorityNodeId: string | null;
      authorityGenesisIdentitySha256: string | null;
      authorityChainSyncSocketPath: string | null;
      externalProviderBindings: readonly ExternalProviderBinding[];
      localQueryServiceBindings: readonly LocalQueryServiceBinding[];
      consistencyDigest: string;
    }>;

export type ExternalProviderBinding = Readonly<{
  providerId: string;
  operatorIdentitySha256: string;
  authenticationKind: "https_tls_identity_v1";
  publicIdentitySha256: string;
  endpoint: string;
}>;

export type LocalQueryServiceBinding = Readonly<{
  kind: WatcherFinalityLocalQueryService["kind"];
  providerId: string;
  endpoint: string;
  observationStatus:
    | "aligned"
    | "unavailable"
    | "stale"
    | "forked"
    | "rollback_not_propagated"
    | "content_mismatch";
  observationDigest: string | null;
}>;

export const exactPlainRecord = (
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

export const exactArray = (value: unknown): readonly unknown[] | null => {
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

export const isNetwork = (value: unknown): value is (typeof NETWORKS)[number] =>
  typeof value === "string" &&
  NETWORKS.includes(value as (typeof NETWORKS)[number]);

export const isHex32 = (value: unknown): value is string =>
  typeof value === "string" && HEX_32.test(value);
