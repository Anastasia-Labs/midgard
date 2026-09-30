import { isAbsolute, normalize as normalizePath } from "node:path";

import {
  type WatcherL1Network,
  type WatcherL1SourceModeV1,
  type WatcherLocalNodeSurface,
} from "./l1-adapter.js";

export const WATCHER_MULTI_PROVIDER_CONSISTENCY_SCHEMA_VERSION =
  "midgard-watcher-multi-provider-consistency-v1" as const;

export const WATCHER_MULTI_PROVIDER_CONSISTENCY_BOUNDS = Object.freeze({
  observations: 16,
  compatibleBlockLag: 64,
});

const NETWORKS = ["Mainnet", "Preprod", "Preview", "Custom"] as const;

const HEX_32 = /^[0-9a-f]{64}$/u;

const CANONICAL_NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export const WATCHER_MULTI_PROVIDER_REASON_CODES = [
  "local_node_consistent",
  "providers_consistent",
  "insufficient_independent_providers",
  "duplicate_provider_id",
  "duplicate_trust_identity",
  "duplicate_operator_identity",
  "unconfigured_provider",
  "duplicate_local_surface",
  "unconfigured_local_query_service",
  "missing_local_query_evidence",
  "provider_transport_mismatch",
  "source_mode_mismatch",
  "local_node_authority_mismatch",
  "local_node_genesis_mismatch",
  "missing_chain_sync_authority",
  "network_mismatch",
  "bounded_provider_lag",
  "stale_provider_observation",
  "rollback_not_propagated",
  "fork_disagreement",
  "block_content_mismatch",
  "malformed_observation",
  "invalid_configured_network",
  "observation_limit_exceeded",
] as const;

export const WATCHER_MULTI_PROVIDER_ALERT_CODES = [
  "watcher_provider_quorum_unavailable",
  "watcher_provider_identity_collision",
  "watcher_provider_not_configured",
  "watcher_provider_transport_mismatch",
  "watcher_l1_source_mode_mismatch",
  "watcher_local_node_authority_mismatch",
  "watcher_local_node_chain_sync_missing",
  "watcher_local_node_query_evidence_missing",
  "watcher_provider_network_mismatch",
  "watcher_provider_lag",
  "watcher_provider_stale",
  "watcher_local_node_rollback_not_propagated",
  "watcher_provider_fork",
  "watcher_provider_content_disagreement",
  "watcher_provider_observation_rejected",
] as const;

export type WatcherMultiProviderReasonCode =
  (typeof WATCHER_MULTI_PROVIDER_REASON_CODES)[number];

export type WatcherMultiProviderAlertCode =
  (typeof WATCHER_MULTI_PROVIDER_ALERT_CODES)[number];

export type WatcherMultiProviderConsistencyStatus =
  | "agreed"
  | "pending"
  | "quarantined";

export type WatcherL1SourceConsistencyConfig =
  | Readonly<{
      sourceMode: "local_node";
      network: WatcherL1Network;
      authorityNodeId: string;
      genesisIdentitySha256: string;
      chainSyncSocketPath: string;
      queryServices: readonly WatcherConfiguredLocalQueryService[];
    }>
  | Readonly<{
      sourceMode: "external_providers";
      network: WatcherL1Network;
      providers: readonly WatcherConfiguredExternalProvider[];
    }>;

export type WatcherConfiguredLocalQueryService = Readonly<{
  kind: Exclude<WatcherLocalNodeSurface, "chain_sync">;
  providerId: string;
  endpoint: string;
}>;

export type WatcherConfiguredExternalProvider = Readonly<{
  providerId: string;
  operatorIdentitySha256: string;
  endpoint: string;
}>;

export type WatcherMultiProviderAgreement = Readonly<{
  pointDigest: string;
  blockHash: string;
  slot: string;
  blockNo: string;
  minimumDepth: string;
  blockContentDigest: string;
}>;

export type WatcherExternalProviderBinding = Readonly<{
  providerId: string;
  operatorIdentitySha256: string;
  authenticationKind: "https_tls_identity_v1";
  publicIdentitySha256: string;
  endpoint: string;
}>;

export type WatcherLocalQueryServiceBinding = Readonly<{
  kind: WatcherConfiguredLocalQueryService["kind"];
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

export type WatcherMultiProviderConsistency = Readonly<{
  schemaVersion: typeof WATCHER_MULTI_PROVIDER_CONSISTENCY_SCHEMA_VERSION;
  status: WatcherMultiProviderConsistencyStatus;
  protocolDecision: "allowed" | "quarantined";
  sourceMode: WatcherL1SourceModeV1 | null;
  configuredNetwork: WatcherL1Network | null;
  configuredSourceDigest: string | null;
  authorityNodeId: string | null;
  authorityGenesisIdentitySha256: string | null;
  authorityChainSyncSocketPath: string | null;
  chainAuthorityObservationDigest: string | null;
  queryObservationCount: number;
  observationCount: number;
  independentProviderCount: number;
  externalProviderBindings: readonly WatcherExternalProviderBinding[];
  localQueryServiceBindings: readonly WatcherLocalQueryServiceBinding[];
  reasonCodes: readonly WatcherMultiProviderReasonCode[];
  alertCodes: readonly WatcherMultiProviderAlertCode[];
  observationEvidenceDigests: readonly string[];
  rejectedObservationCount: number;
  agreement: WatcherMultiProviderAgreement | null;
  consistencyDigest: string;
}>;

export type PlainRecord = Record<string, unknown>;

export type ConsistencyResultWithoutDigest = Omit<
  WatcherMultiProviderConsistency,
  "consistencyDigest"
>;

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

export const exactObservationArray = (
  value: unknown,
): readonly unknown[] | null => {
  if (
    !Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Array.prototype
  ) {
    return null;
  }
  const keys = Reflect.ownKeys(value);
  if (
    keys.length !== value.length + 1 ||
    keys.some(
      (key) =>
        typeof key !== "string" ||
        (key !== "length" &&
          (!CANONICAL_NATURAL.test(key) ||
            BigInt(key) >= BigInt(value.length))),
    )
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

export const isNetwork = (value: unknown): value is WatcherL1Network =>
  typeof value === "string" && NETWORKS.includes(value as WatcherL1Network);

export const isHex32 = (value: unknown): value is string =>
  typeof value === "string" && HEX_32.test(value);

export const isNatural = (value: unknown): value is string =>
  typeof value === "string" && CANONICAL_NATURAL.test(value);

export const isExactEndpoint = (
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

export const isExactAbsoluteSocketPath = (value: unknown): value is string =>
  typeof value === "string" &&
  value.length > 0 &&
  value.length <= 4_096 &&
  !value.includes("\0") &&
  isAbsolute(value) &&
  normalizePath(value) === value;

export const endpointAlias = (value: string): string => {
  const endpoint = new URL(value);
  endpoint.hostname = endpoint.hostname.toLowerCase().replace(/\.$/u, "");
  return `${endpoint.protocol}//${endpoint.host.toLowerCase()}${endpoint.pathname.replace(/\/+$/u, "")}`;
};

export const sameBytes = (left: Buffer, right: Buffer): boolean =>
  left.length === right.length && left.equals(right);
