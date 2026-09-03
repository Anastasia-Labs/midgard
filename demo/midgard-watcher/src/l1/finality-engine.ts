import { isAbsolute, normalize as normalizePath } from "node:path";

import {
  type DeploymentMarker,
  MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";

import {
  parseWatcherConfig,
  WATCHER_CARDANO_SECURITY_PARAMETER_K,
  type WatcherConfig,
} from "../runtime/config.js";
import type { VerifiedWatcherDeploymentIdentity } from "../runtime/deployment-identity.js";
import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import {
  WATCHER_MULTI_PROVIDER_ALERT_CODES,
  WATCHER_MULTI_PROVIDER_CONSISTENCY_BOUNDS,
  WATCHER_MULTI_PROVIDER_CONSISTENCY_SCHEMA_VERSION,
  WATCHER_MULTI_PROVIDER_REASON_CODES,
} from "./multi-provider-consistency.js";

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

const NETWORKS = ["Mainnet", "Preprod", "Preview"] as const;
const HEX_32 = /^[0-9a-f]{64}$/u;
const CANONICAL_NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const SOURCE_AUTHORITY_ID = /^[a-z][a-z0-9-]{0,62}$/u;

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
  "release_evidence_mismatch",
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
  releaseEvidenceDigest: string;
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
  releaseEvidenceDigest: string;
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

type Agreement = Readonly<{
  configuredNetwork: (typeof NETWORKS)[number];
  pointDigest: string;
  blockHash: string;
  slot: string;
  blockNo: string;
  minimumDepth: string;
  blockContentDigest: string;
  consistencyDigest: string;
}>;

type ParsedConsistency =
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

type ExternalProviderBinding = Readonly<{
  providerId: string;
  operatorIdentitySha256: string;
  authenticationKind: "https_tls_identity_v1";
  publicIdentitySha256: string;
  endpoint: string;
}>;

type LocalQueryServiceBinding = Readonly<{
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

const isNetwork = (value: unknown): value is (typeof NETWORKS)[number] =>
  typeof value === "string" &&
  NETWORKS.includes(value as (typeof NETWORKS)[number]);

const isHex32 = (value: unknown): value is string =>
  typeof value === "string" && HEX_32.test(value);

const isExactEndpoint = (
  value: unknown,
  protocols: readonly string[],
): value is string => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value.length > 2_048 ||
    value !== value.trim()
  ) {
    return false;
  }
  try {
    const endpoint = new URL(value);
    return (
      protocols.includes(endpoint.protocol) &&
      endpoint.username.length === 0 &&
      endpoint.password.length === 0 &&
      endpoint.search.length === 0 &&
      endpoint.hash.length === 0
    );
  } catch {
    return false;
  }
};

const isExactAbsoluteSocketPath = (value: unknown): value is string =>
  typeof value === "string" &&
  value.length > 0 &&
  value.length <= 4_096 &&
  !value.includes("\0") &&
  isAbsolute(value) &&
  normalizePath(value) === value;

const endpointAlias = (value: string): string => {
  const endpoint = new URL(value);
  endpoint.hostname = endpoint.hostname.toLowerCase().replace(/\.$/u, "");
  return `${endpoint.protocol}//${endpoint.host.toLowerCase()}${endpoint.pathname.replace(/\/+$/u, "")}`;
};

const isUint64 = (value: unknown): value is string =>
  typeof value === "string" &&
  CANONICAL_NATURAL.test(value) &&
  value.length <= 20 &&
  BigInt(value) <= WATCHER_FINALITY_BOUNDS.uint64Maximum;

const isPositiveUint64 = (value: unknown): value is string =>
  isUint64(value) && value !== "0";

const sha256Canonical = watcherSha256CanonicalJson;

const freezeStrings = <T extends string>(values: readonly T[]): readonly T[] =>
  Object.freeze([...values]);

const cloneMarker = (value: unknown): DeploymentMarker | null => {
  const marker = exactPlainRecord(value, ["schemaVersion", "manifestId"]);
  if (
    marker === null ||
    marker.schemaVersion !== MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION ||
    !isHex32(marker.manifestId)
  ) {
    return null;
  }
  return Object.freeze({
    schemaVersion: MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
    manifestId: marker.manifestId,
  });
};

const sameMarker = (left: DeploymentMarker, right: DeploymentMarker): boolean =>
  left.schemaVersion === right.schemaVersion &&
  left.manifestId === right.manifestId;

const cloneExternalProviders = (
  value: unknown,
): readonly WatcherFinalityExternalProvider[] | null => {
  const providers = exactArray(value);
  if (providers === null || providers.length < 2 || providers.length > 4) {
    return null;
  }
  const normalized: WatcherFinalityExternalProvider[] = [];
  const providerIds = new Set<string>();
  const operatorIdentities = new Set<string>();
  const endpoints = new Set<string>();
  for (const candidate of providers) {
    const provider = exactPlainRecord(candidate, [
      "providerId",
      "operatorIdentitySha256",
      "endpoint",
      "authenticationKind",
    ]);
    if (
      provider === null ||
      typeof provider.providerId !== "string" ||
      !SOURCE_AUTHORITY_ID.test(provider.providerId) ||
      !isHex32(provider.operatorIdentitySha256) ||
      !isExactEndpoint(provider.endpoint, ["https:"]) ||
      provider.authenticationKind !== "https_tls_identity_v1"
    ) {
      return null;
    }
    const alias = endpointAlias(provider.endpoint);
    if (
      providerIds.has(provider.providerId) ||
      operatorIdentities.has(provider.operatorIdentitySha256) ||
      endpoints.has(alias)
    ) {
      return null;
    }
    providerIds.add(provider.providerId);
    operatorIdentities.add(provider.operatorIdentitySha256);
    endpoints.add(alias);
    normalized.push(
      Object.freeze({
        providerId: provider.providerId,
        operatorIdentitySha256: provider.operatorIdentitySha256,
        endpoint: provider.endpoint,
        authenticationKind: "https_tls_identity_v1",
      }),
    );
  }
  return Object.freeze(
    normalized.sort((left, right) =>
      left.providerId < right.providerId
        ? -1
        : left.providerId > right.providerId
          ? 1
          : 0,
    ),
  );
};

const cloneLocalQueryServices = (
  value: unknown,
): readonly WatcherFinalityLocalQueryService[] | null => {
  const inputs = exactArray(value);
  if (inputs === null || inputs.length > 8) {
    return null;
  }
  const services: WatcherFinalityLocalQueryService[] = [];
  const providerIds = new Set<string>();
  const endpoints = new Set<string>();
  for (const input of inputs) {
    const service = exactPlainRecord(input, ["kind", "providerId", "endpoint"]);
    if (
      service === null ||
      !["ogmios", "kupo", "kupmios", "db_sync"].includes(
        service.kind as string,
      ) ||
      typeof service.providerId !== "string" ||
      !SOURCE_AUTHORITY_ID.test(service.providerId) ||
      !isExactEndpoint(
        service.endpoint,
        service.kind === "ogmios"
          ? ["http:", "https:", "ws:", "wss:"]
          : service.kind === "kupo"
            ? ["http:", "https:"]
            : service.kind === "kupmios"
              ? ["http:", "https:", "ws:", "wss:"]
              : ["postgresql:"],
      ) ||
      providerIds.has(service.providerId)
    ) {
      return null;
    }
    const alias = endpointAlias(service.endpoint);
    if (endpoints.has(alias)) {
      return null;
    }
    providerIds.add(service.providerId);
    endpoints.add(alias);
    services.push(
      Object.freeze({
        kind: service.kind as WatcherFinalityLocalQueryService["kind"],
        providerId: service.providerId,
        endpoint: service.endpoint,
      }),
    );
  }
  return Object.freeze(
    services.sort((left, right) =>
      left.providerId.localeCompare(right.providerId),
    ),
  );
};

const makePolicy = (
  value: Omit<WatcherFinalityPolicy, "policyDigest">,
): WatcherFinalityPolicy => {
  const deploymentMarker = cloneMarker(value.deploymentMarker);
  if (deploymentMarker === null) {
    throw new Error("invalid deployment marker");
  }
  const externalProviders =
    value.externalProviders === null
      ? null
      : cloneExternalProviders(value.externalProviders);
  const localQueryServices = cloneLocalQueryServices(value.localQueryServices);
  if (
    localQueryServices === null ||
    (value.sourceMode === "external_providers" && externalProviders === null) ||
    (value.sourceMode === "local_node" && externalProviders !== null) ||
    (value.sourceMode === "external_providers" &&
      localQueryServices.length !== 0)
  ) {
    throw new Error("invalid external provider allowlist");
  }
  const canonical = {
    schemaVersion: WATCHER_FINALITY_POLICY_SCHEMA_VERSION,
    network: value.network,
    sourceMode: value.sourceMode,
    authorityNodeId: value.authorityNodeId,
    authorityGenesisIdentitySha256: value.authorityGenesisIdentitySha256,
    authorityChainSyncSocketPath: value.authorityChainSyncSocketPath,
    localQueryServices,
    externalProviders,
    confirmationDepth: value.confirmationDepth,
    maximumPreFinalityRollbackDepth: value.maximumPreFinalityRollbackDepth,
    maximumPostFinalityRecoveryDepth: value.maximumPostFinalityRecoveryDepth,
    beforeFinalityRollback: "rewind" as const,
    afterFinalityRollback: "quarantine" as const,
    releaseEvidenceDigest: value.releaseEvidenceDigest,
    deploymentMarker,
  };
  return Object.freeze({
    ...canonical,
    policyDigest: sha256Canonical(canonical),
  });
};

export const parseWatcherFinalityPolicy = (
  value: unknown,
): WatcherFinalityPolicy | null => {
  try {
    const policy = exactPlainRecord(value, [
      "schemaVersion",
      "network",
      "sourceMode",
      "authorityNodeId",
      "authorityGenesisIdentitySha256",
      "authorityChainSyncSocketPath",
      "localQueryServices",
      "externalProviders",
      "confirmationDepth",
      "maximumPreFinalityRollbackDepth",
      "maximumPostFinalityRecoveryDepth",
      "beforeFinalityRollback",
      "afterFinalityRollback",
      "releaseEvidenceDigest",
      "deploymentMarker",
      "policyDigest",
    ]);
    const marker =
      policy === null ? null : cloneMarker(policy.deploymentMarker);
    const externalProviders =
      policy === null || policy.externalProviders === null
        ? null
        : cloneExternalProviders(policy.externalProviders);
    const localQueryServices =
      policy === null
        ? null
        : cloneLocalQueryServices(policy.localQueryServices);
    if (
      policy === null ||
      policy.schemaVersion !== WATCHER_FINALITY_POLICY_SCHEMA_VERSION ||
      !isNetwork(policy.network) ||
      !["local_node", "external_providers"].includes(
        policy.sourceMode as string,
      ) ||
      !(
        (policy.sourceMode === "local_node" &&
          typeof policy.authorityNodeId === "string" &&
          SOURCE_AUTHORITY_ID.test(policy.authorityNodeId) &&
          isHex32(policy.authorityGenesisIdentitySha256) &&
          isExactAbsoluteSocketPath(policy.authorityChainSyncSocketPath) &&
          localQueryServices !== null &&
          policy.externalProviders === null) ||
        (policy.sourceMode === "external_providers" &&
          policy.authorityNodeId === null &&
          policy.authorityGenesisIdentitySha256 === null &&
          policy.authorityChainSyncSocketPath === null &&
          localQueryServices?.length === 0 &&
          externalProviders !== null)
      ) ||
      !isPositiveUint64(policy.confirmationDepth) ||
      BigInt(policy.confirmationDepth) >
        WATCHER_FINALITY_BOUNDS.confirmationDepth ||
      !isPositiveUint64(policy.maximumPreFinalityRollbackDepth) ||
      BigInt(policy.maximumPreFinalityRollbackDepth) >
        BigInt(policy.confirmationDepth) ||
      !isPositiveUint64(policy.maximumPostFinalityRecoveryDepth) ||
      BigInt(policy.maximumPostFinalityRecoveryDepth) !==
        BigInt(WATCHER_CARDANO_SECURITY_PARAMETER_K) ||
      policy.beforeFinalityRollback !== "rewind" ||
      policy.afterFinalityRollback !== "quarantine" ||
      !isHex32(policy.releaseEvidenceDigest) ||
      marker === null ||
      !isHex32(policy.policyDigest)
    ) {
      return null;
    }
    const canonical = makePolicy({
      schemaVersion: WATCHER_FINALITY_POLICY_SCHEMA_VERSION,
      network: policy.network,
      sourceMode: policy.sourceMode as WatcherFinalityPolicy["sourceMode"],
      authorityNodeId: policy.authorityNodeId as string | null,
      authorityGenesisIdentitySha256: policy.authorityGenesisIdentitySha256 as
        | string
        | null,
      authorityChainSyncSocketPath: policy.authorityChainSyncSocketPath as
        | string
        | null,
      localQueryServices:
        localQueryServices as WatcherFinalityPolicy["localQueryServices"],
      externalProviders,
      confirmationDepth: policy.confirmationDepth,
      maximumPreFinalityRollbackDepth: policy.maximumPreFinalityRollbackDepth,
      maximumPostFinalityRecoveryDepth: policy.maximumPostFinalityRecoveryDepth,
      beforeFinalityRollback: "rewind",
      afterFinalityRollback: "quarantine",
      releaseEvidenceDigest: policy.releaseEvidenceDigest,
      deploymentMarker: marker,
    });
    return canonical.policyDigest === policy.policyDigest ? canonical : null;
  } catch {
    return null;
  }
};

const parseVerifiedDeploymentIdentity = (
  value: unknown,
): VerifiedWatcherDeploymentIdentity | null => {
  try {
    const identity = exactPlainRecord(value, [
      "manifestId",
      "network",
      "trustRootId",
      "releaseEvidenceDigest",
      "ruleBundleCommitment",
      "programCommitments",
      "durableMarker",
    ]);
    if (
      identity === null ||
      !isHex32(identity.manifestId) ||
      !isNetwork(identity.network) ||
      !isHex32(identity.trustRootId) ||
      !isHex32(identity.releaseEvidenceDigest) ||
      !isHex32(identity.ruleBundleCommitment)
    ) {
      return null;
    }
    const commitments =
      typeof identity.programCommitments === "object" &&
      identity.programCommitments !== null &&
      !Array.isArray(identity.programCommitments)
        ? (identity.programCommitments as Record<string, unknown>)
        : null;
    if (
      commitments === null ||
      Reflect.ownKeys(commitments).length !== Object.keys(commitments).length ||
      Object.values(commitments).some((commitment) => !isHex32(commitment))
    ) {
      return null;
    }
    const marker = cloneMarker(identity.durableMarker);
    if (marker === null || marker.manifestId !== identity.manifestId) {
      return null;
    }
    return value as VerifiedWatcherDeploymentIdentity;
  } catch {
    return null;
  }
};

/**
 * Binds W01's strict finality configuration to W02's verified release and
 * durable deployment identity. The returned policy is the only policy shape
 * accepted by the transition engine.
 */
export const makeWatcherFinalityPolicy = (
  configInput: unknown,
  deploymentIdentityInput: unknown,
): WatcherFinalityPolicy | null => {
  try {
    const config: WatcherConfig = parseWatcherConfig(configInput);
    const identity = parseVerifiedDeploymentIdentity(deploymentIdentityInput);
    if (identity === null || identity.network !== config.targetNetwork) {
      return null;
    }
    const source = config.l1.source;
    const sourceAuthority =
      source.sourceMode === "local_node"
        ? {
            authorityNodeId: source.authorityNodeId,
            authorityGenesisIdentitySha256:
              source.chainSync.genesisIdentitySha256,
            authorityChainSyncSocketPath: source.chainSync.socketPath,
            localQueryServices: Object.freeze(
              source.queryServices.map((service) =>
                Object.freeze({
                  kind: service.kind,
                  providerId: service.identity,
                  endpoint: service.endpoint,
                }),
              ),
            ),
            externalProviders: null,
          }
        : {
            authorityNodeId: null,
            authorityGenesisIdentitySha256: null,
            authorityChainSyncSocketPath: null,
            localQueryServices: Object.freeze([]),
            externalProviders: Object.freeze(
              source.providers.map((provider) =>
                Object.freeze({
                  providerId: provider.identity,
                  operatorIdentitySha256: provider.operatorIdentitySha256,
                  endpoint: provider.endpoint,
                  authenticationKind: "https_tls_identity_v1" as const,
                }),
              ),
            ),
          };
    return makePolicy({
      schemaVersion: WATCHER_FINALITY_POLICY_SCHEMA_VERSION,
      network: config.targetNetwork,
      sourceMode: source.sourceMode,
      ...sourceAuthority,
      confirmationDepth: config.l1.finality.depth.toString(),
      maximumPreFinalityRollbackDepth:
        config.l1.finality.rollback.maxDepth.toString(),
      maximumPostFinalityRecoveryDepth:
        config.l1.finality.rollback.postFinalityRecoveryMaxDepth.toString(),
      beforeFinalityRollback: config.l1.finality.rollback.beforeFinality,
      afterFinalityRollback: config.l1.finality.rollback.afterFinality,
      releaseEvidenceDigest: identity.releaseEvidenceDigest,
      deploymentMarker: identity.durableMarker,
    });
  } catch {
    return null;
  }
};

const parseBoundObservation = (
  value: unknown,
): WatcherFinalityBoundObservation | null => {
  const bound = exactPlainRecord(value, [
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
    bound === null ||
    !isHex32(bound.pointDigest) ||
    !isHex32(bound.blockHash) ||
    !isUint64(bound.slot) ||
    !isUint64(bound.blockNo) ||
    !isHex32(bound.blockContentDigest) ||
    !isHex32(bound.firstSeenConsistencyDigest) ||
    !isHex32(bound.lastSeenConsistencyDigest) ||
    !isUint64(bound.firstSeenDepth) ||
    !isUint64(bound.currentDepth) ||
    !isPositiveUint64(bound.visibilityCount)
  ) {
    return null;
  }
  return Object.freeze({
    pointDigest: bound.pointDigest,
    blockHash: bound.blockHash,
    slot: bound.slot,
    blockNo: bound.blockNo,
    blockContentDigest: bound.blockContentDigest,
    firstSeenConsistencyDigest: bound.firstSeenConsistencyDigest,
    lastSeenConsistencyDigest: bound.lastSeenConsistencyDigest,
    firstSeenDepth: bound.firstSeenDepth,
    currentDepth: bound.currentDepth,
    visibilityCount: bound.visibilityCount,
  });
};

const INCIDENT_REASONS = ["post_finality_point_changed"] as const;

const parseIncident = (value: unknown): WatcherFinalityIncident | null => {
  const incident = exactPlainRecord(value, [
    "reasonCode",
    "triggerConsistencyDigest",
    "incidentDigest",
  ]);
  if (
    incident === null ||
    typeof incident.reasonCode !== "string" ||
    !INCIDENT_REASONS.includes(
      incident.reasonCode as WatcherFinalityIncident["reasonCode"],
    ) ||
    !(
      incident.triggerConsistencyDigest === null ||
      isHex32(incident.triggerConsistencyDigest)
    ) ||
    !isHex32(incident.incidentDigest)
  ) {
    return null;
  }
  return Object.freeze({
    reasonCode: incident.reasonCode as WatcherFinalityIncident["reasonCode"],
    triggerConsistencyDigest: incident.triggerConsistencyDigest,
    incidentDigest: incident.incidentDigest,
  });
};

const makeState = (
  value: Omit<WatcherFinalityState, "stateDigest">,
): WatcherFinalityState => {
  const canonical = {
    schemaVersion: WATCHER_FINALITY_STATE_SCHEMA_VERSION,
    policyDigest: value.policyDigest,
    network: value.network,
    releaseEvidenceDigest: value.releaseEvidenceDigest,
    deploymentMarker: value.deploymentMarker,
    phase: value.phase,
    pending: value.pending,
    finalized: value.finalized,
    incident: value.incident,
  };
  return Object.freeze({
    ...canonical,
    stateDigest: sha256Canonical(canonical),
  });
};

const stateMatchesPolicy = (
  state: WatcherFinalityState,
  policy: WatcherFinalityPolicy,
): boolean =>
  state.policyDigest === policy.policyDigest &&
  state.network === policy.network &&
  state.releaseEvidenceDigest === policy.releaseEvidenceDigest &&
  sameMarker(state.deploymentMarker, policy.deploymentMarker);

const stateSemanticsAreValid = (
  state: WatcherFinalityState,
  policy: WatcherFinalityPolicy,
): boolean => {
  if (state.phase === "unobserved") {
    return true;
  }
  const observation =
    state.phase === "pending" ? state.pending : state.finalized;
  if (observation === null) {
    return false;
  }
  const firstSeenDepth = BigInt(observation.firstSeenDepth);
  const currentDepth = BigInt(observation.currentDepth);
  const visibilityCount = BigInt(observation.visibilityCount);
  if (firstSeenDepth > currentDepth) {
    return false;
  }
  if (state.phase === "pending") {
    return (
      currentDepth < BigInt(policy.confirmationDepth) &&
      (visibilityCount === 1n || currentDepth > firstSeenDepth)
    );
  }
  return (
    currentDepth >= BigInt(policy.confirmationDepth) &&
    visibilityCount >= 2n &&
    currentDepth > firstSeenDepth
  );
};

export const parseWatcherFinalityState = (
  value: unknown,
  policyInput?: unknown,
): WatcherFinalityState | null => {
  try {
    const state = exactPlainRecord(value, [
      "schemaVersion",
      "policyDigest",
      "network",
      "releaseEvidenceDigest",
      "deploymentMarker",
      "phase",
      "pending",
      "finalized",
      "incident",
      "stateDigest",
    ]);
    if (
      state === null ||
      state.schemaVersion !== WATCHER_FINALITY_STATE_SCHEMA_VERSION ||
      !isHex32(state.policyDigest) ||
      !isNetwork(state.network) ||
      !isHex32(state.releaseEvidenceDigest) ||
      !isHex32(state.stateDigest)
    ) {
      return null;
    }
    const marker = cloneMarker(state.deploymentMarker);
    const pending =
      state.pending === null ? null : parseBoundObservation(state.pending);
    const finalized =
      state.finalized === null ? null : parseBoundObservation(state.finalized);
    const incident =
      state.incident === null ? null : parseIncident(state.incident);
    const phase = state.phase;
    const shapeValid =
      marker !== null &&
      ((phase === "unobserved" &&
        pending === null &&
        finalized === null &&
        incident === null) ||
        (phase === "pending" &&
          pending !== null &&
          finalized === null &&
          incident === null) ||
        (phase === "finalized" &&
          pending === null &&
          finalized !== null &&
          incident === null) ||
        (phase === "quarantined" &&
          pending === null &&
          finalized !== null &&
          incident !== null));
    if (!shapeValid || marker === null) {
      return null;
    }
    const canonical = makeState({
      schemaVersion: WATCHER_FINALITY_STATE_SCHEMA_VERSION,
      policyDigest: state.policyDigest,
      network: state.network,
      releaseEvidenceDigest: state.releaseEvidenceDigest,
      deploymentMarker: marker,
      phase: phase as WatcherFinalityPhase,
      pending,
      finalized,
      incident,
    });
    if (canonical.stateDigest !== state.stateDigest) {
      return null;
    }
    if (policyInput !== undefined) {
      const policy = parseWatcherFinalityPolicy(policyInput);
      if (
        policy === null ||
        !stateMatchesPolicy(canonical, policy) ||
        !stateSemanticsAreValid(canonical, policy)
      ) {
        return null;
      }
    }
    return canonical;
  } catch {
    return null;
  }
};

const initialState = (policy: WatcherFinalityPolicy): WatcherFinalityState =>
  makeState({
    schemaVersion: WATCHER_FINALITY_STATE_SCHEMA_VERSION,
    policyDigest: policy.policyDigest,
    network: policy.network,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    deploymentMarker: policy.deploymentMarker,
    phase: "unobserved",
    pending: null,
    finalized: null,
    incident: null,
  });

export const makeWatcherFinalityBootstrapState = (
  policyInput: unknown,
): WatcherFinalityState | null => {
  const policy = parseWatcherFinalityPolicy(policyInput);
  return policy === null ? null : initialState(policy);
};

const parseStringArray = (
  value: unknown,
  allowed: readonly string[],
): readonly string[] | null => {
  const values = exactArray(value);
  if (
    values === null ||
    values.some(
      (candidate) =>
        typeof candidate !== "string" || !allowed.includes(candidate),
    ) ||
    new Set(values).size !== values.length
  ) {
    return null;
  }
  return values as readonly string[];
};

const sameStringArray = (
  left: readonly string[],
  right: readonly string[],
): boolean =>
  left.length === right.length &&
  left.every((value, index) => value === right[index]);

const parseExternalProviderBindings = (
  value: unknown,
): readonly ExternalProviderBinding[] | null => {
  const candidates = exactArray(value);
  if (
    candidates === null ||
    candidates.length > WATCHER_MULTI_PROVIDER_CONSISTENCY_BOUNDS.observations
  ) {
    return null;
  }
  const bindings: ExternalProviderBinding[] = [];
  for (const candidate of candidates) {
    const binding = exactPlainRecord(candidate, [
      "providerId",
      "operatorIdentitySha256",
      "authenticationKind",
      "publicIdentitySha256",
      "endpoint",
    ]);
    if (
      binding === null ||
      typeof binding.providerId !== "string" ||
      !/^[a-z][a-z0-9-]{0,62}$/u.test(binding.providerId) ||
      !isHex32(binding.operatorIdentitySha256) ||
      binding.authenticationKind !== "https_tls_identity_v1" ||
      !isHex32(binding.publicIdentitySha256) ||
      !isExactEndpoint(binding.endpoint, ["https:"])
    ) {
      return null;
    }
    bindings.push(
      Object.freeze({
        providerId: binding.providerId,
        operatorIdentitySha256: binding.operatorIdentitySha256,
        authenticationKind: "https_tls_identity_v1",
        publicIdentitySha256: binding.publicIdentitySha256,
        endpoint: binding.endpoint,
      }),
    );
  }
  if (
    bindings.some(
      (binding, index) =>
        index > 0 &&
        binding.providerId <=
          (bindings[index - 1] as ExternalProviderBinding).providerId,
    )
  ) {
    return null;
  }
  return Object.freeze(bindings);
};

const parseLocalQueryServiceBindings = (
  value: unknown,
): readonly LocalQueryServiceBinding[] | null => {
  const inputs = exactArray(value);
  if (inputs === null || inputs.length > 8) {
    return null;
  }
  const bindings: LocalQueryServiceBinding[] = [];
  for (const input of inputs) {
    const binding = exactPlainRecord(input, [
      "kind",
      "providerId",
      "endpoint",
      "observationStatus",
      "observationDigest",
    ]);
    if (
      binding === null ||
      !["ogmios", "kupo", "kupmios", "db_sync"].includes(
        binding.kind as string,
      ) ||
      typeof binding.providerId !== "string" ||
      !SOURCE_AUTHORITY_ID.test(binding.providerId) ||
      !isExactEndpoint(
        binding.endpoint,
        binding.kind === "ogmios"
          ? ["http:", "https:", "ws:", "wss:"]
          : binding.kind === "kupo"
            ? ["http:", "https:"]
            : binding.kind === "kupmios"
              ? ["http:", "https:", "ws:", "wss:"]
              : ["postgresql:"],
      ) ||
      ![
        "aligned",
        "unavailable",
        "stale",
        "forked",
        "rollback_not_propagated",
        "content_mismatch",
      ].includes(binding.observationStatus as string) ||
      !(
        binding.observationDigest === null || isHex32(binding.observationDigest)
      ) ||
      (binding.observationStatus === "unavailable") !==
        (binding.observationDigest === null)
    ) {
      return null;
    }
    bindings.push(
      Object.freeze({
        kind: binding.kind as LocalQueryServiceBinding["kind"],
        providerId: binding.providerId,
        endpoint: binding.endpoint,
        observationStatus:
          binding.observationStatus as LocalQueryServiceBinding["observationStatus"],
        observationDigest: binding.observationDigest as string | null,
      }),
    );
  }
  if (
    bindings.some(
      (binding, index) =>
        index > 0 &&
        binding.providerId <=
          (bindings[index - 1] as LocalQueryServiceBinding).providerId,
    )
  ) {
    return null;
  }
  return Object.freeze(bindings);
};

const parseConsistency = (value: unknown): ParsedConsistency | null => {
  try {
    const result = exactPlainRecord(value, [
      "schemaVersion",
      "status",
      "protocolDecision",
      "sourceMode",
      "configuredNetwork",
      "configuredSourceDigest",
      "authorityNodeId",
      "authorityGenesisIdentitySha256",
      "authorityChainSyncSocketPath",
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
      result === null ||
      result.schemaVersion !==
        WATCHER_MULTI_PROVIDER_CONSISTENCY_SCHEMA_VERSION ||
      !["agreed", "pending", "quarantined"].includes(result.status as string) ||
      !["local_node", "external_providers"].includes(
        result.sourceMode as string,
      ) ||
      !(
        result.configuredNetwork === null || isNetwork(result.configuredNetwork)
      ) ||
      !isHex32(result.configuredSourceDigest) ||
      !Number.isSafeInteger(result.queryObservationCount) ||
      (result.queryObservationCount as number) < 0 ||
      !Number.isSafeInteger(result.observationCount) ||
      (result.observationCount as number) < 0 ||
      (result.observationCount as number) >
        WATCHER_MULTI_PROVIDER_CONSISTENCY_BOUNDS.observations ||
      !Number.isSafeInteger(result.independentProviderCount) ||
      (result.independentProviderCount as number) < 0 ||
      (result.independentProviderCount as number) >
        (result.observationCount as number) ||
      !Number.isSafeInteger(result.rejectedObservationCount) ||
      (result.rejectedObservationCount as number) < 0 ||
      (result.rejectedObservationCount as number) >
        (result.observationCount as number) ||
      !isHex32(result.consistencyDigest)
    ) {
      return null;
    }
    const reasons = parseStringArray(
      result.reasonCodes,
      WATCHER_MULTI_PROVIDER_REASON_CODES,
    );
    const alerts = parseStringArray(
      result.alertCodes,
      WATCHER_MULTI_PROVIDER_ALERT_CODES,
    );
    const externalProviderBindings = parseExternalProviderBindings(
      result.externalProviderBindings,
    );
    const localQueryServiceBindings = parseLocalQueryServiceBindings(
      result.localQueryServiceBindings,
    );
    const evidence = parseStringArray(result.observationEvidenceDigests, []);
    const evidenceArray =
      evidence === null
        ? exactArray(result.observationEvidenceDigests)
        : evidence;
    if (
      reasons === null ||
      alerts === null ||
      externalProviderBindings === null ||
      localQueryServiceBindings === null ||
      evidenceArray === null ||
      evidenceArray.some((digest) => !isHex32(digest)) ||
      new Set(evidenceArray).size !== evidenceArray.length ||
      !sameStringArray(
        reasons,
        WATCHER_MULTI_PROVIDER_REASON_CODES.filter((code) =>
          reasons.includes(code),
        ),
      ) ||
      !sameStringArray(
        alerts,
        WATCHER_MULTI_PROVIDER_ALERT_CODES.filter((code) =>
          alerts.includes(code),
        ),
      ) ||
      !sameStringArray(
        evidenceArray as readonly string[],
        [...(evidenceArray as readonly string[])].sort(),
      )
    ) {
      return null;
    }
    const sourceMode = result.sourceMode as "local_node" | "external_providers";
    const sourceShapeValid =
      sourceMode === "local_node"
        ? typeof result.authorityNodeId === "string" &&
          SOURCE_AUTHORITY_ID.test(result.authorityNodeId) &&
          isHex32(result.authorityGenesisIdentitySha256) &&
          isExactAbsoluteSocketPath(result.authorityChainSyncSocketPath) &&
          ((result.chainAuthorityObservationDigest === null &&
            result.status !== "agreed") ||
            (isHex32(result.chainAuthorityObservationDigest) &&
              evidenceArray.includes(
                result.chainAuthorityObservationDigest,
              ))) &&
          result.queryObservationCount ===
            localQueryServiceBindings.filter(
              ({ observationDigest }) => observationDigest !== null,
            ).length &&
          externalProviderBindings.length === 0 &&
          localQueryServiceBindings.length <= 8
        : result.authorityNodeId === null &&
          result.authorityGenesisIdentitySha256 === null &&
          result.authorityChainSyncSocketPath === null &&
          result.chainAuthorityObservationDigest === null &&
          result.queryObservationCount === 0 &&
          localQueryServiceBindings.length === 0 &&
          externalProviderBindings.length <=
            (result.observationCount as number);
    if (!sourceShapeValid) {
      return null;
    }

    let agreementCanonical: Record<string, unknown> | null = null;
    let agreement: Agreement | null = null;
    if (result.agreement !== null) {
      const parsed = exactPlainRecord(result.agreement, [
        "pointDigest",
        "blockHash",
        "slot",
        "blockNo",
        "minimumDepth",
        "blockContentDigest",
      ]);
      if (
        parsed === null ||
        !isHex32(parsed.pointDigest) ||
        !isHex32(parsed.blockHash) ||
        !isUint64(parsed.slot) ||
        !isUint64(parsed.blockNo) ||
        !isUint64(parsed.minimumDepth) ||
        !isHex32(parsed.blockContentDigest) ||
        !isNetwork(result.configuredNetwork)
      ) {
        return null;
      }
      agreementCanonical = {
        pointDigest: parsed.pointDigest,
        blockHash: parsed.blockHash,
        slot: parsed.slot,
        blockNo: parsed.blockNo,
        minimumDepth: parsed.minimumDepth,
        blockContentDigest: parsed.blockContentDigest,
      };
      agreement = Object.freeze({
        configuredNetwork: result.configuredNetwork,
        ...agreementCanonical,
        consistencyDigest: result.consistencyDigest,
      }) as Agreement;
    }
    const canonicalWithoutDigest = {
      schemaVersion: WATCHER_MULTI_PROVIDER_CONSISTENCY_SCHEMA_VERSION,
      status: result.status,
      protocolDecision: result.protocolDecision,
      sourceMode,
      configuredNetwork: result.configuredNetwork,
      configuredSourceDigest: result.configuredSourceDigest,
      authorityNodeId: result.authorityNodeId,
      authorityGenesisIdentitySha256: result.authorityGenesisIdentitySha256,
      authorityChainSyncSocketPath: result.authorityChainSyncSocketPath,
      chainAuthorityObservationDigest: result.chainAuthorityObservationDigest,
      queryObservationCount: result.queryObservationCount,
      observationCount: result.observationCount,
      independentProviderCount: result.independentProviderCount,
      externalProviderBindings,
      localQueryServiceBindings,
      reasonCodes: reasons,
      alertCodes: alerts,
      observationEvidenceDigests: evidenceArray,
      rejectedObservationCount: result.rejectedObservationCount,
      agreement: agreementCanonical,
    };
    if (sha256Canonical(canonicalWithoutDigest) !== result.consistencyDigest) {
      return null;
    }
    if (result.status === "agreed") {
      const agreementShapeValid =
        sourceMode === "local_node"
          ? (result.observationCount as number) >= 1 &&
            result.independentProviderCount === 1 &&
            localQueryServiceBindings.every(
              ({ observationStatus }) => observationStatus === "aligned",
            ) &&
            sameStringArray(reasons, ["local_node_consistent"])
          : (result.observationCount as number) >= 2 &&
            (result.independentProviderCount as number) >= 2 &&
            externalProviderBindings.length ===
              (result.observationCount as number) &&
            sameStringArray(reasons, ["providers_consistent"]);
      if (
        result.protocolDecision !== "allowed" ||
        agreement === null ||
        !agreementShapeValid ||
        result.rejectedObservationCount !== 0 ||
        alerts.length !== 0 ||
        evidenceArray.length !== result.observationCount
      ) {
        return null;
      }
      return Object.freeze({
        kind: "agreed",
        sourceMode,
        configuredSourceDigest: result.configuredSourceDigest,
        authorityNodeId: result.authorityNodeId as string | null,
        authorityGenesisIdentitySha256:
          result.authorityGenesisIdentitySha256 as string | null,
        authorityChainSyncSocketPath: result.authorityChainSyncSocketPath as
          | string
          | null,
        externalProviderBindings,
        localQueryServiceBindings,
        agreement,
      });
    }
    if (
      result.protocolDecision !== "quarantined" ||
      result.agreement !== null
    ) {
      return null;
    }
    return Object.freeze({
      kind: result.status as "pending" | "quarantined",
      sourceMode,
      configuredSourceDigest: result.configuredSourceDigest,
      authorityNodeId: result.authorityNodeId as string | null,
      authorityGenesisIdentitySha256: result.authorityGenesisIdentitySha256 as
        | string
        | null,
      authorityChainSyncSocketPath: result.authorityChainSyncSocketPath as
        | string
        | null,
      externalProviderBindings,
      localQueryServiceBindings,
      consistencyDigest: result.consistencyDigest,
    });
  } catch {
    return null;
  }
};

const makeBoundObservation = (
  agreement: Agreement,
  existing: WatcherFinalityBoundObservation | null,
): WatcherFinalityBoundObservation =>
  Object.freeze({
    pointDigest: agreement.pointDigest,
    blockHash: agreement.blockHash,
    slot: agreement.slot,
    blockNo: agreement.blockNo,
    blockContentDigest: agreement.blockContentDigest,
    firstSeenConsistencyDigest:
      existing?.firstSeenConsistencyDigest ?? agreement.consistencyDigest,
    lastSeenConsistencyDigest: agreement.consistencyDigest,
    firstSeenDepth: existing?.firstSeenDepth ?? agreement.minimumDepth,
    currentDepth: agreement.minimumDepth,
    visibilityCount:
      existing === null
        ? "1"
        : (BigInt(existing.visibilityCount) + 1n).toString(),
  });

const result = (
  action: WatcherFinalityAction,
  protocolDecision: WatcherFinalityResult["protocolDecision"],
  reasonCodes: readonly WatcherFinalityReasonCode[],
  alertCodes: readonly WatcherFinalityAlertCode[],
  state: WatcherFinalityState | null,
  rewindInstruction: WatcherFinalityRewindInstruction | null = null,
): WatcherFinalityResult => {
  const canonical = {
    schemaVersion: WATCHER_FINALITY_RESULT_SCHEMA_VERSION,
    action,
    protocolDecision,
    reasonCodes: freezeStrings(reasonCodes),
    alertCodes: freezeStrings(alertCodes),
    state,
    rewindInstruction,
  };
  return Object.freeze({
    ...canonical,
    resultDigest: sha256Canonical(canonical),
  });
};

const rewind = (
  kind: WatcherFinalityRewindInstruction["kind"],
  discardedStateDigest: string,
  agreement: Agreement,
): WatcherFinalityRewindInstruction => {
  const canonical = {
    schemaVersion: WATCHER_FINALITY_REWIND_INSTRUCTION_SCHEMA_VERSION,
    kind,
    discardedStateDigest,
    replacementPointDigest: agreement.pointDigest,
    replacementContentDigest: agreement.blockContentDigest,
    replacementDepth: agreement.minimumDepth,
  };
  return Object.freeze({
    ...canonical,
    instructionDigest: sha256Canonical(canonical),
  });
};

const requiredPreFinalityRollbackDepth = (
  pending: WatcherFinalityBoundObservation,
  agreement: Agreement,
  kind: WatcherFinalityRewindInstruction["kind"],
): bigint => {
  if (kind === "pending_depth_regression") {
    return BigInt(pending.currentDepth) - BigInt(agreement.minimumDepth);
  }
  // Replacing a block already observed at depth d requires invalidating that
  // block and its d descendants. W13 may prove a deeper common-ancestor
  // rewind; W12 never treats less than this release-bound minimum as safe.
  return BigInt(pending.currentDepth) + 1n;
};

const incident = (
  priorState: WatcherFinalityState,
  reasonCode: WatcherFinalityIncident["reasonCode"],
  triggerConsistencyDigest: string | null,
): WatcherFinalityIncident => {
  const canonical = {
    reasonCode,
    triggerConsistencyDigest,
    priorStateDigest: priorState.stateDigest,
  };
  return Object.freeze({
    reasonCode,
    triggerConsistencyDigest,
    incidentDigest: sha256Canonical(canonical),
  });
};

const quarantineFinalized = (
  state: WatcherFinalityState,
  reasonCode: WatcherFinalityIncident["reasonCode"],
  triggerConsistencyDigest: string | null,
): WatcherFinalityResult => {
  const next = makeState({
    schemaVersion: WATCHER_FINALITY_STATE_SCHEMA_VERSION,
    policyDigest: state.policyDigest,
    network: state.network,
    releaseEvidenceDigest: state.releaseEvidenceDigest,
    deploymentMarker: state.deploymentMarker,
    phase: "quarantined",
    pending: null,
    finalized: state.finalized,
    incident: incident(state, reasonCode, triggerConsistencyDigest),
  });
  return result(
    "quarantine_incident",
    "quarantined",
    [reasonCode, "post_finality_contradiction"],
    ["watcher_finality_post_finality_incident"],
    next,
  );
};

const bindingFailure = (
  policy: WatcherFinalityPolicy,
  state: WatcherFinalityState,
): WatcherFinalityReasonCode | null => {
  if (state.network !== policy.network) {
    return "configured_network_mismatch";
  }
  if (state.releaseEvidenceDigest !== policy.releaseEvidenceDigest) {
    return "release_evidence_mismatch";
  }
  if (!sameMarker(state.deploymentMarker, policy.deploymentMarker)) {
    return "deployment_mismatch";
  }
  return state.policyDigest === policy.policyDigest ? null : "stale_state";
};

/**
 * Compares a W11 record's external-provider bindings against the W01
 * allowlist. `identity_mismatch` means some binding names a provider the
 * policy does not configure, or contradicts the configured operator identity,
 * authentication kind, or endpoint. `binding_unrun` means every binding
 * present agrees with the policy but the policy configures a provider the
 * record never bound at all.
 *
 * #539: the identity check alone is a `.every` over a list the W11 parser
 * bounds only from above (`bindings.length <= observationCount`), so it is
 * vacuously true for any strict subset of the configured providers - and for
 * the empty list. A record binding two of three configured providers reached
 * `finality_granted` with the third provider's operator/TLS/endpoint binding
 * never evaluated. The coverage bound below is the missing lower bound, and
 * mirrors the local-node branch's `bindings.length === localQueryServices
 * .length` exact-length pin; it is set-valued rather than positional because
 * the W11 parser sorts bindings by `providerId` while the policy keeps its
 * own configured order.
 */
const externalProviderBindingsMatchPolicy = (
  policy: WatcherFinalityPolicy,
  bindings: readonly ExternalProviderBinding[],
  requireEveryConfiguredProvider: boolean,
): "matched" | "identity_mismatch" | "binding_unrun" => {
  if (policy.sourceMode !== "external_providers") {
    return bindings.length === 0 ? "matched" : "identity_mismatch";
  }
  const configured = policy.externalProviders;
  if (configured === null) {
    return "identity_mismatch";
  }
  const byProviderId = new Map(
    configured.map((provider) => [provider.providerId, provider] as const),
  );
  const identitiesMatch = bindings.every((binding) => {
    const provider = byProviderId.get(binding.providerId);
    return (
      provider !== undefined &&
      binding.operatorIdentitySha256 === provider.operatorIdentitySha256 &&
      binding.authenticationKind === provider.authenticationKind &&
      binding.endpoint === provider.endpoint
    );
  });
  if (!identitiesMatch) {
    return "identity_mismatch";
  }
  if (!requireEveryConfiguredProvider) {
    return "matched";
  }
  const boundProviderIds = new Set(
    bindings.map(({ providerId }) => providerId),
  );
  return bindings.length === configured.length &&
    boundProviderIds.size === configured.length &&
    configured.every(({ providerId }) => boundProviderIds.has(providerId))
    ? "matched"
    : "binding_unrun";
};

const localQueryServiceBindingsMatchPolicy = (
  policy: WatcherFinalityPolicy,
  bindings: readonly LocalQueryServiceBinding[],
): boolean =>
  policy.sourceMode === "local_node"
    ? bindings.length === policy.localQueryServices.length &&
      bindings.every((binding, index) => {
        const configured = policy.localQueryServices[index];
        return (
          configured !== undefined &&
          binding.providerId === configured.providerId &&
          binding.kind === configured.kind &&
          binding.endpoint === configured.endpoint
        );
      })
    : bindings.length === 0;

const configuredSourceForPolicy = (
  policy: WatcherFinalityPolicy,
):
  | Readonly<{
      sourceMode: "local_node";
      network: WatcherFinalityPolicy["network"];
      authorityNodeId: string;
      genesisIdentitySha256: string;
      chainSyncSocketPath: string;
      queryServices: readonly WatcherFinalityLocalQueryService[];
    }>
  | Readonly<{
      sourceMode: "external_providers";
      network: WatcherFinalityPolicy["network"];
      providers: readonly Readonly<{
        providerId: string;
        operatorIdentitySha256: string;
        endpoint: string;
      }>[];
    }> =>
  policy.sourceMode === "local_node"
    ? Object.freeze({
        sourceMode: "local_node",
        network: policy.network,
        authorityNodeId: policy.authorityNodeId!,
        genesisIdentitySha256: policy.authorityGenesisIdentitySha256!,
        chainSyncSocketPath: policy.authorityChainSyncSocketPath!,
        queryServices: policy.localQueryServices,
      })
    : Object.freeze({
        sourceMode: "external_providers",
        network: policy.network,
        providers: Object.freeze(
          policy.externalProviders!.map(
            ({ providerId, operatorIdentitySha256, endpoint }) =>
              Object.freeze({
                providerId,
                operatorIdentitySha256,
                endpoint,
              }),
          ),
        ),
      });

/**
 * The exact source binding a W11 record must satisfy before its verdict is
 * read at all. Provider coverage is required only of an `agreed` record: a
 * pending or quarantined record legitimately omits a provider that was
 * unavailable, and is refused a line later by its own kind.
 */
const sourceBindingFailureFor = (
  policy: WatcherFinalityPolicy,
  consistency: ParsedConsistency,
): WatcherFinalityReasonCode | null => {
  if (consistency.sourceMode !== policy.sourceMode) {
    return "source_mode_mismatch";
  }
  if (
    consistency.configuredSourceDigest !==
    sha256Canonical(configuredSourceForPolicy(policy))
  ) {
    return "source_authority_mismatch";
  }
  if (
    !localQueryServiceBindingsMatchPolicy(
      policy,
      consistency.localQueryServiceBindings,
    )
  ) {
    return "source_authority_mismatch";
  }
  if (policy.sourceMode === "external_providers") {
    const providerBinding = externalProviderBindingsMatchPolicy(
      policy,
      consistency.externalProviderBindings,
      consistency.kind === "agreed",
    );
    if (providerBinding === "identity_mismatch") {
      return "source_provider_mismatch";
    }
    if (providerBinding === "binding_unrun") {
      return "source_provider_binding_unrun";
    }
  }
  return consistency.authorityNodeId !== policy.authorityNodeId ||
    consistency.authorityGenesisIdentitySha256 !==
      policy.authorityGenesisIdentitySha256
    ? "source_authority_mismatch"
    : null;
};

export const watcherFinalityConfiguredSource = configuredSourceForPolicy;

/**
 * Advances canonical finality from one exact W11 decision. It never grants
 * finality on first visibility, never advances from W11 pending/quarantine,
 * and never removes a finalized binding when later evidence contradicts it.
 */
export const evaluateWatcherFinality = (
  policyInput: unknown,
  previousStateInput: unknown,
  consistencyInput: unknown,
): WatcherFinalityResult => {
  const policy = parseWatcherFinalityPolicy(policyInput);
  if (policy === null) {
    return result(
      "reject",
      "quarantined",
      ["malformed_policy"],
      [
        "watcher_finality_input_rejected",
        "watcher_finality_configuration_mismatch",
      ],
      null,
    );
  }

  const state =
    previousStateInput === null
      ? initialState(policy)
      : parseWatcherFinalityState(previousStateInput);
  if (state === null) {
    return result(
      "reject",
      "quarantined",
      ["malformed_state"],
      ["watcher_finality_input_rejected", "watcher_finality_state_rejected"],
      null,
    );
  }
  const failure = bindingFailure(policy, state);
  if (failure !== null) {
    return result(
      "reject",
      "quarantined",
      [failure],
      [
        "watcher_finality_input_rejected",
        failure === "stale_state"
          ? "watcher_finality_state_rejected"
          : "watcher_finality_configuration_mismatch",
      ],
      null,
    );
  }
  if (!stateSemanticsAreValid(state, policy)) {
    return result(
      "reject",
      "quarantined",
      ["invalid_state_semantics"],
      ["watcher_finality_input_rejected", "watcher_finality_state_rejected"],
      null,
    );
  }
  if (state.phase === "quarantined") {
    return result(
      "reject",
      "quarantined",
      ["state_quarantined"],
      ["watcher_finality_post_finality_incident"],
      state,
    );
  }

  const consistency = parseConsistency(consistencyInput);
  if (consistency === null) {
    return result(
      "reject",
      "quarantined",
      ["malformed_provider_result"],
      ["watcher_finality_input_rejected"],
      state,
    );
  }
  const sourceBindingFailure = sourceBindingFailureFor(policy, consistency);
  if (sourceBindingFailure !== null) {
    return result(
      "reject",
      "quarantined",
      [sourceBindingFailure],
      [
        "watcher_finality_input_rejected",
        "watcher_finality_configuration_mismatch",
      ],
      state,
    );
  }
  if (consistency.kind !== "agreed") {
    const reason =
      consistency.kind === "pending"
        ? "provider_result_pending"
        : "provider_result_quarantined";
    return result(
      "reject",
      "quarantined",
      [reason],
      ["watcher_finality_input_rejected"],
      state,
    );
  }

  const agreement = consistency.agreement;
  if (agreement.configuredNetwork !== policy.network) {
    return result(
      "reject",
      "quarantined",
      ["configured_network_mismatch"],
      [
        "watcher_finality_input_rejected",
        "watcher_finality_configuration_mismatch",
      ],
      state,
    );
  }

  if (state.phase === "unobserved") {
    const next = makeState({
      schemaVersion: WATCHER_FINALITY_STATE_SCHEMA_VERSION,
      policyDigest: policy.policyDigest,
      network: policy.network,
      releaseEvidenceDigest: policy.releaseEvidenceDigest,
      deploymentMarker: policy.deploymentMarker,
      phase: "pending",
      pending: makeBoundObservation(agreement, null),
      finalized: null,
      incident: null,
    });
    return result(
      "observe_pending",
      "hold",
      ["first_visibility_pending"],
      ["watcher_finality_pending"],
      next,
    );
  }

  if (state.phase === "finalized") {
    const finalized = state.finalized as WatcherFinalityBoundObservation;
    if (
      agreement.pointDigest !== finalized.pointDigest ||
      agreement.blockHash !== finalized.blockHash ||
      agreement.slot !== finalized.slot ||
      agreement.blockNo !== finalized.blockNo
    ) {
      return quarantineFinalized(
        state,
        "post_finality_point_changed",
        agreement.consistencyDigest,
      );
    }
    if (agreement.blockContentDigest !== finalized.blockContentDigest) {
      return result(
        "reject",
        "quarantined",
        ["post_finality_content_changed"],
        ["watcher_finality_input_rejected"],
        state,
      );
    }
    if (BigInt(agreement.minimumDepth) < BigInt(finalized.currentDepth)) {
      return result(
        "reject",
        "quarantined",
        ["post_finality_depth_regression"],
        ["watcher_finality_input_rejected"],
        state,
      );
    }
    return result(
      "duplicate",
      "hold",
      [
        agreement.consistencyDigest === finalized.lastSeenConsistencyDigest
          ? "duplicate_observation"
          : "already_finalized",
      ],
      [],
      state,
    );
  }

  const pending = state.pending as WatcherFinalityBoundObservation;
  let rewindReason: WatcherFinalityRewindInstruction["kind"] | null = null;
  if (
    agreement.pointDigest !== pending.pointDigest ||
    agreement.blockHash !== pending.blockHash ||
    agreement.slot !== pending.slot ||
    agreement.blockNo !== pending.blockNo
  ) {
    rewindReason = "pending_point_changed";
  } else if (agreement.blockContentDigest !== pending.blockContentDigest) {
    rewindReason = "pending_content_changed";
  } else if (BigInt(agreement.minimumDepth) < BigInt(pending.currentDepth)) {
    rewindReason = "pending_depth_regression";
  }
  if (rewindReason !== null) {
    const rollbackDepth = requiredPreFinalityRollbackDepth(
      pending,
      agreement,
      rewindReason,
    );
    if (rollbackDepth > BigInt(policy.maximumPreFinalityRollbackDepth)) {
      return result(
        "reject",
        "quarantined",
        ["pre_finality_rollback_depth_exceeded"],
        [
          "watcher_finality_input_rejected",
          "watcher_finality_rollback_limit_exceeded",
        ],
        state,
      );
    }
    const replacement = makeBoundObservation(agreement, null);
    const next = makeState({
      schemaVersion: WATCHER_FINALITY_STATE_SCHEMA_VERSION,
      policyDigest: policy.policyDigest,
      network: policy.network,
      releaseEvidenceDigest: policy.releaseEvidenceDigest,
      deploymentMarker: policy.deploymentMarker,
      phase: "pending",
      pending: replacement,
      finalized: null,
      incident: null,
    });
    return result(
      "rewind_pending",
      "rewind_required",
      [rewindReason],
      ["watcher_finality_rewind_required", "watcher_finality_pending"],
      next,
      rewind(rewindReason, state.stateDigest, agreement),
    );
  }
  if (agreement.consistencyDigest === pending.lastSeenConsistencyDigest) {
    return result(
      "duplicate",
      "hold",
      ["duplicate_observation"],
      ["watcher_finality_pending"],
      state,
    );
  }
  if (agreement.minimumDepth === pending.currentDepth) {
    return result(
      "reject",
      "hold",
      ["stale_observation"],
      ["watcher_finality_input_rejected", "watcher_finality_pending"],
      state,
    );
  }

  const nextBound = makeBoundObservation(agreement, pending);
  if (BigInt(agreement.minimumDepth) >= BigInt(policy.confirmationDepth)) {
    const next = makeState({
      schemaVersion: WATCHER_FINALITY_STATE_SCHEMA_VERSION,
      policyDigest: policy.policyDigest,
      network: policy.network,
      releaseEvidenceDigest: policy.releaseEvidenceDigest,
      deploymentMarker: policy.deploymentMarker,
      phase: "finalized",
      pending: null,
      finalized: nextBound,
      incident: null,
    });
    return result(
      "finalize",
      "finality_granted",
      ["confirmation_depth_reached"],
      [],
      next,
    );
  }
  const next = makeState({
    schemaVersion: WATCHER_FINALITY_STATE_SCHEMA_VERSION,
    policyDigest: policy.policyDigest,
    network: policy.network,
    releaseEvidenceDigest: policy.releaseEvidenceDigest,
    deploymentMarker: policy.deploymentMarker,
    phase: "pending",
    pending: nextBound,
    finalized: null,
    incident: null,
  });
  return result(
    "advance_pending",
    "hold",
    ["pending_depth_progress", "confirmation_depth_pending"],
    ["watcher_finality_pending"],
    next,
  );
};
