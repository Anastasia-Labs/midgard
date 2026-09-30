import { isAbsolute, normalize as normalizePath } from "node:path";

import {
  type DeploymentMarker,
  MIDGARD_DEPLOYMENT_MARKER_SCHEMA_VERSION,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import { parseWatcherCustomNetwork } from "../runtime/custom-network.js";
import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import {
  CANONICAL_NATURAL,
  exactArray,
  exactPlainRecord,
  isHex32,
  SOURCE_AUTHORITY_ID,
  WATCHER_FINALITY_BOUNDS,
  WATCHER_FINALITY_POLICY_SCHEMA_VERSION,
  type WatcherFinalityExternalProvider,
  type WatcherFinalityLocalQueryService,
  type WatcherFinalityPolicy,
} from "./finality-engine.watcher-finality-reason-codes.js";

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

const endpointAlias = (value: string): string => {
  const endpoint = new URL(value);
  endpoint.hostname = endpoint.hostname.toLowerCase().replace(/\.$/u, "");
  return `${endpoint.protocol}//${endpoint.host.toLowerCase()}${endpoint.pathname.replace(/\/+$/u, "")}`;
};

export const isUint64 = (value: unknown): value is string =>
  typeof value === "string" &&
  CANONICAL_NATURAL.test(value) &&
  value.length <= 20 &&
  BigInt(value) <= WATCHER_FINALITY_BOUNDS.uint64Maximum;

export const isPositiveUint64 = (value: unknown): value is string =>
  isUint64(value) && value !== "0";

export const sha256Canonical = watcherSha256CanonicalJson;

export const freezeStrings = <T extends string>(
  values: readonly T[],
): readonly T[] => Object.freeze([...values]);

export const cloneMarker = (value: unknown): DeploymentMarker | null => {
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

export const sameMarker = (
  left: DeploymentMarker,
  right: DeploymentMarker,
): boolean =>
  left.schemaVersion === right.schemaVersion &&
  left.manifestId === right.manifestId;

export const cloneExternalProviders = (
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

export const cloneLocalQueryServices = (
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

export const makePolicy = (
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
    ...(value.customNetwork === undefined
      ? {}
      : { customNetwork: parseWatcherCustomNetwork(value.customNetwork) }),
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
    blueprintHash: value.blueprintHash,
    deploymentMarker,
  };
  return Object.freeze({
    ...canonical,
    policyDigest: sha256Canonical(canonical),
  });
};
