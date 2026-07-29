import { createHash } from "node:crypto";

import {
  encodeWatcherNormalizedL1BlockV1,
  normalizeWatcherL1BlockV1,
  WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION,
  WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
  WATCHER_NORMALIZED_L1_BLOCK_V1_SCHEMA_VERSION,
  type WatcherL1NetworkV1,
  type WatcherL1SourceModeV1,
  type WatcherLocalNodeSurfaceV1,
  type WatcherNormalizedL1BlockV1,
} from "./l1-adapter.js";

export const WATCHER_MULTI_PROVIDER_CONSISTENCY_V1_SCHEMA_VERSION =
  "midgard-watcher-multi-provider-consistency-v1" as const;

export const WATCHER_MULTI_PROVIDER_CONSISTENCY_V1_BOUNDS = Object.freeze({
  observations: 16,
  compatibleBlockLag: 64,
});

const NETWORKS = ["Mainnet", "Preprod", "Preview"] as const;
const HEX_32 = /^[0-9a-f]{64}$/u;
const CANONICAL_NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export const WATCHER_MULTI_PROVIDER_REASON_CODES_V1 = [
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

export const WATCHER_MULTI_PROVIDER_ALERT_CODES_V1 = [
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

export type WatcherMultiProviderReasonCodeV1 =
  (typeof WATCHER_MULTI_PROVIDER_REASON_CODES_V1)[number];
export type WatcherMultiProviderAlertCodeV1 =
  (typeof WATCHER_MULTI_PROVIDER_ALERT_CODES_V1)[number];
export type WatcherMultiProviderConsistencyStatusV1 =
  | "agreed"
  | "pending"
  | "quarantined";

export type WatcherL1SourceConsistencyConfigV1 =
  | Readonly<{
      sourceMode: "local_node";
      network: WatcherL1NetworkV1;
      authorityNodeId: string;
      genesisIdentitySha256: string;
      queryServices: readonly WatcherConfiguredLocalQueryServiceV1[];
    }>
  | Readonly<{
      sourceMode: "external_providers";
      network: WatcherL1NetworkV1;
      providers: readonly WatcherConfiguredExternalProviderV1[];
    }>;

export type WatcherConfiguredLocalQueryServiceV1 = Readonly<{
  kind: Exclude<WatcherLocalNodeSurfaceV1, "chain_sync">;
  providerId: string;
}>;

export type WatcherConfiguredExternalProviderV1 = Readonly<{
  providerId: string;
  operatorIdentitySha256: string;
}>;

export type WatcherMultiProviderAgreementV1 = Readonly<{
  pointDigest: string;
  blockHash: string;
  slot: string;
  blockNo: string;
  minimumDepth: string;
  blockContentDigest: string;
}>;

export type WatcherExternalProviderBindingV1 = Readonly<{
  providerId: string;
  operatorIdentitySha256: string;
  authenticationKind: "https_tls_identity_v1";
  publicIdentitySha256: string;
}>;

export type WatcherLocalQueryServiceBindingV1 = Readonly<{
  kind: WatcherConfiguredLocalQueryServiceV1["kind"];
  providerId: string;
  observationStatus:
    | "aligned"
    | "unavailable"
    | "stale"
    | "forked"
    | "rollback_not_propagated"
    | "content_mismatch";
  observationDigest: string | null;
}>;

export type WatcherMultiProviderConsistencyV1 = Readonly<{
  schemaVersion: typeof WATCHER_MULTI_PROVIDER_CONSISTENCY_V1_SCHEMA_VERSION;
  status: WatcherMultiProviderConsistencyStatusV1;
  protocolDecision: "allowed" | "quarantined";
  sourceMode: WatcherL1SourceModeV1 | null;
  configuredNetwork: WatcherL1NetworkV1 | null;
  configuredSourceDigest: string | null;
  authorityNodeId: string | null;
  authorityGenesisIdentitySha256: string | null;
  chainAuthorityObservationDigest: string | null;
  queryObservationCount: number;
  observationCount: number;
  independentProviderCount: number;
  externalProviderBindings: readonly WatcherExternalProviderBindingV1[];
  localQueryServiceBindings: readonly WatcherLocalQueryServiceBindingV1[];
  reasonCodes: readonly WatcherMultiProviderReasonCodeV1[];
  alertCodes: readonly WatcherMultiProviderAlertCodeV1[];
  observationEvidenceDigests: readonly string[];
  rejectedObservationCount: number;
  agreement: WatcherMultiProviderAgreementV1 | null;
  consistencyDigest: string;
}>;

type PlainRecord = Record<string, unknown>;

type ConsistencyResultWithoutDigest = Omit<
  WatcherMultiProviderConsistencyV1,
  "consistencyDigest"
>;

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

const exactObservationArray = (value: unknown): readonly unknown[] | null => {
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

const isNetwork = (value: unknown): value is WatcherL1NetworkV1 =>
  typeof value === "string" && NETWORKS.includes(value as WatcherL1NetworkV1);

const isHex32 = (value: unknown): value is string =>
  typeof value === "string" && HEX_32.test(value);

const isNatural = (value: unknown): value is string =>
  typeof value === "string" && CANONICAL_NATURAL.test(value);

const sameBytes = (left: Buffer, right: Buffer): boolean =>
  left.length === right.length && left.equals(right);

/**
 * Re-validates an adapter result at this trust boundary. Re-normalization
 * prevents callers from forging provider, point, content, or observation
 * digests on an object merely asserted to have the public TypeScript type.
 */
const parseNormalizedObservation = (
  value: unknown,
): WatcherNormalizedL1BlockV1 | null => {
  const block = exactPlainRecord(value, [
    "schemaVersion",
    "network",
    "provider",
    "chainPoint",
    "transactions",
    "blockContentDigest",
    "observationDigest",
  ]);
  if (
    block === null ||
    block.schemaVersion !== WATCHER_NORMALIZED_L1_BLOCK_V1_SCHEMA_VERSION ||
    !isNetwork(block.network) ||
    !isHex32(block.blockContentDigest) ||
    !isHex32(block.observationDigest)
  ) {
    return null;
  }
  const point = exactPlainRecord(block.chainPoint, [
    "chainPointId",
    "pointDigest",
    "blockHash",
    "slot",
    "blockNo",
    "depth",
  ]);
  if (
    point === null ||
    !isHex32(point.chainPointId) ||
    !isHex32(point.pointDigest) ||
    !isHex32(point.blockHash) ||
    !isNatural(point.slot) ||
    !isNatural(point.blockNo) ||
    !isNatural(point.depth)
  ) {
    return null;
  }
  const provider = exactPlainRecord(block.provider, [
    "schemaVersion",
    "network",
    "providerId",
    "source",
    "authentication",
  ]);
  if (
    provider === null ||
    provider.schemaVersion !==
      WATCHER_AUTHENTICATED_L1_PROVIDER_V1_SCHEMA_VERSION
  ) {
    return null;
  }

  const normalized = normalizeWatcherL1BlockV1(block.provider, {
    schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_V1_SCHEMA_VERSION,
    network: block.network,
    providerId: provider.providerId,
    chainPoint: {
      blockHash: point.blockHash,
      slot: point.slot,
      blockNo: point.blockNo,
      depth: point.depth,
    },
    transactions: block.transactions,
  });
  if (
    normalized.chainPoint.chainPointId !== point.chainPointId ||
    normalized.chainPoint.pointDigest !== point.pointDigest ||
    normalized.blockContentDigest !== block.blockContentDigest ||
    normalized.observationDigest !== block.observationDigest ||
    !sameBytes(
      encodeWatcherNormalizedL1BlockV1(normalized),
      encodeWatcherNormalizedL1BlockV1(
        block as unknown as WatcherNormalizedL1BlockV1,
      ),
    )
  ) {
    return null;
  }
  return normalized;
};

const sha256Canonical = (value: unknown): string =>
  createHash("sha256").update(JSON.stringify(value), "utf8").digest("hex");

const sortCodes = <T extends string>(
  values: ReadonlySet<T>,
  order: readonly T[],
): readonly T[] =>
  Object.freeze(order.filter((candidate) => values.has(candidate)));

const minimumNatural = (values: readonly string[]): string =>
  values.reduce((minimum, candidate) =>
    BigInt(candidate) < BigInt(minimum) ? candidate : minimum,
  );

const duplicateValues = (values: readonly string[]): boolean =>
  new Set(values).size !== values.length;

const makeResult = (
  value: ConsistencyResultWithoutDigest,
): WatcherMultiProviderConsistencyV1 => {
  const consistencyDigest = sha256Canonical(value);
  return Object.freeze({
    ...value,
    consistencyDigest,
  });
};

const rejectedBoundaryResult = (
  configuredSource: WatcherL1SourceConsistencyConfigV1 | null,
  reason: WatcherMultiProviderReasonCodeV1,
): WatcherMultiProviderConsistencyV1 => {
  const reasons = new Set<WatcherMultiProviderReasonCodeV1>([reason]);
  const alerts = new Set<WatcherMultiProviderAlertCodeV1>([
    "watcher_provider_observation_rejected",
  ]);
  if (
    configuredSource === null ||
    configuredSource.sourceMode === "external_providers"
  ) {
    reasons.add("insufficient_independent_providers");
    alerts.add("watcher_provider_quorum_unavailable");
  }
  return makeResult({
    schemaVersion: WATCHER_MULTI_PROVIDER_CONSISTENCY_V1_SCHEMA_VERSION,
    status: "quarantined",
    protocolDecision: "quarantined",
    sourceMode: configuredSource?.sourceMode ?? null,
    configuredNetwork: configuredSource?.network ?? null,
    configuredSourceDigest:
      configuredSource === null ? null : sha256Canonical(configuredSource),
    authorityNodeId:
      configuredSource?.sourceMode === "local_node"
        ? configuredSource.authorityNodeId
        : null,
    authorityGenesisIdentitySha256:
      configuredSource?.sourceMode === "local_node"
        ? configuredSource.genesisIdentitySha256
        : null,
    chainAuthorityObservationDigest: null,
    queryObservationCount: 0,
    observationCount: 0,
    independentProviderCount: 0,
    externalProviderBindings: Object.freeze([]),
    localQueryServiceBindings: Object.freeze([]),
    reasonCodes: sortCodes(reasons, WATCHER_MULTI_PROVIDER_REASON_CODES_V1),
    alertCodes: sortCodes(alerts, WATCHER_MULTI_PROVIDER_ALERT_CODES_V1),
    observationEvidenceDigests: Object.freeze([]),
    rejectedObservationCount: 1,
    agreement: null,
  });
};

const parseConfiguredSource = (
  input: unknown,
): WatcherL1SourceConsistencyConfigV1 | null => {
  const candidate = exactPlainRecord(input, [
    "sourceMode",
    "network",
    "providers",
  ]);
  if (candidate !== null && candidate.sourceMode === "external_providers") {
    const providerInputs = exactObservationArray(candidate.providers);
    if (
      !isNetwork(candidate.network) ||
      providerInputs === null ||
      providerInputs.length < 2 ||
      providerInputs.length >
        WATCHER_MULTI_PROVIDER_CONSISTENCY_V1_BOUNDS.observations
    ) {
      return null;
    }
    const providers: WatcherConfiguredExternalProviderV1[] = [];
    for (const input of providerInputs) {
      const provider = exactPlainRecord(input, [
        "providerId",
        "operatorIdentitySha256",
      ]);
      if (
        provider === null ||
        typeof provider.providerId !== "string" ||
        !/^[a-z][a-z0-9-]{0,62}$/u.test(provider.providerId) ||
        !isHex32(provider.operatorIdentitySha256)
      ) {
        return null;
      }
      providers.push(
        Object.freeze({
          providerId: provider.providerId,
          operatorIdentitySha256: provider.operatorIdentitySha256,
        }),
      );
    }
    if (
      duplicateValues(providers.map(({ providerId }) => providerId)) ||
      duplicateValues(
        providers.map(({ operatorIdentitySha256 }) => operatorIdentitySha256),
      )
    ) {
      return null;
    }
    return Object.freeze({
      sourceMode: "external_providers",
      network: candidate.network,
      providers: Object.freeze(
        providers.sort((left, right) =>
          left.providerId.localeCompare(right.providerId),
        ),
      ),
    });
  }
  const local = exactPlainRecord(input, [
    "sourceMode",
    "network",
    "authorityNodeId",
    "genesisIdentitySha256",
    "queryServices",
  ]);
  const queryInputs =
    local === null ? null : exactObservationArray(local.queryServices);
  if (
    local === null ||
    local.sourceMode !== "local_node" ||
    !isNetwork(local.network) ||
    typeof local.authorityNodeId !== "string" ||
    !/^[a-z][a-z0-9-]{0,62}$/u.test(local.authorityNodeId) ||
    !isHex32(local.genesisIdentitySha256) ||
    queryInputs === null ||
    queryInputs.length > 8
  ) {
    return null;
  }
  const queryServices: WatcherConfiguredLocalQueryServiceV1[] = [];
  for (const input of queryInputs) {
    const query = exactPlainRecord(input, ["kind", "providerId"]);
    if (
      query === null ||
      !["ogmios", "kupo", "kupmios", "db_sync"].includes(
        query.kind as string,
      ) ||
      typeof query.providerId !== "string" ||
      !/^[a-z][a-z0-9-]{0,62}$/u.test(query.providerId)
    ) {
      return null;
    }
    queryServices.push(
      Object.freeze({
        kind: query.kind as WatcherConfiguredLocalQueryServiceV1["kind"],
        providerId: query.providerId,
      }),
    );
  }
  if (
    duplicateValues(queryServices.map(({ providerId }) => providerId)) ||
    duplicateValues(
      queryServices.map(({ kind, providerId }) => `${kind}:${providerId}`),
    )
  ) {
    return null;
  }
  return Object.freeze({
    sourceMode: "local_node",
    network: local.network,
    authorityNodeId: local.authorityNodeId,
    genesisIdentitySha256: local.genesisIdentitySha256,
    queryServices: Object.freeze(
      queryServices.sort((left, right) =>
        left.providerId.localeCompare(right.providerId),
      ),
    ),
  });
};

/**
 * Produces a fail-closed consistency decision from already-normalized,
 * independently authenticated provider observations. No endpoint, credential,
 * operator database, or administration value is accepted as an input.
 */
export const evaluateWatcherMultiProviderConsistencyV1 = (
  configuredSourceInput: unknown,
  observationsInput: unknown,
): WatcherMultiProviderConsistencyV1 => {
  const configuredSource = parseConfiguredSource(configuredSourceInput);
  if (configuredSource === null) {
    return rejectedBoundaryResult(null, "invalid_configured_network");
  }
  const configuredNetwork = configuredSource.network;

  let inputs: readonly unknown[];
  try {
    const parsed = exactObservationArray(observationsInput);
    if (parsed === null) {
      return rejectedBoundaryResult(configuredSource, "malformed_observation");
    }
    if (
      parsed.length > WATCHER_MULTI_PROVIDER_CONSISTENCY_V1_BOUNDS.observations
    ) {
      return rejectedBoundaryResult(
        configuredSource,
        "observation_limit_exceeded",
      );
    }
    inputs = parsed;
  } catch {
    return rejectedBoundaryResult(configuredSource, "malformed_observation");
  }

  const valid: WatcherNormalizedL1BlockV1[] = [];
  let rejectedObservationCount = 0;
  for (const input of inputs) {
    try {
      const observation = parseNormalizedObservation(input);
      if (observation === null) {
        rejectedObservationCount += 1;
      } else {
        valid.push(observation);
      }
    } catch {
      rejectedObservationCount += 1;
    }
  }

  const reasons = new Set<WatcherMultiProviderReasonCodeV1>();
  const alerts = new Set<WatcherMultiProviderAlertCodeV1>();
  if (rejectedObservationCount > 0) {
    reasons.add("malformed_observation");
    alerts.add("watcher_provider_observation_rejected");
  }

  let eligible = valid.filter(
    (observation) =>
      observation.network === configuredNetwork &&
      observation.provider.source.sourceMode === configuredSource.sourceMode,
  );
  if (valid.some((observation) => observation.network !== configuredNetwork)) {
    reasons.add("network_mismatch");
    alerts.add("watcher_provider_network_mismatch");
  }
  if (
    valid.some(
      (observation) =>
        observation.provider.source.sourceMode !== configuredSource.sourceMode,
    )
  ) {
    reasons.add("source_mode_mismatch");
    alerts.add("watcher_l1_source_mode_mismatch");
  }

  const providerIds = eligible.map(
    (observation) => observation.provider.providerId,
  );
  let agreement: WatcherMultiProviderAgreementV1 | null = null;
  let hasPendingLag = false;
  let independentProviderCount = 0;
  let chainAuthorityObservationDigest: string | null = null;
  let queryObservationCount = 0;
  let externalProviderBindings: readonly WatcherExternalProviderBindingV1[] =
    Object.freeze([]);
  let localQueryServiceBindings: readonly WatcherLocalQueryServiceBindingV1[] =
    Object.freeze([]);

  if (configuredSource.sourceMode === "local_node") {
    const local = eligible.filter(
      (
        observation,
      ): observation is WatcherNormalizedL1BlockV1 & {
        readonly provider: WatcherNormalizedL1BlockV1["provider"] & {
          readonly source: Extract<
            WatcherNormalizedL1BlockV1["provider"]["source"],
            { readonly sourceMode: "local_node" }
          >;
        };
      } => observation.provider.source.sourceMode === "local_node",
    );
    const boundToAuthority = local.filter(
      (observation) =>
        observation.provider.source.authorityNodeId ===
        configuredSource.authorityNodeId,
    );
    if (boundToAuthority.length !== local.length) {
      reasons.add("local_node_authority_mismatch");
      alerts.add("watcher_local_node_authority_mismatch");
    }
    const chainAuthorities = boundToAuthority.filter(
      (observation) =>
        observation.provider.source.surface === "chain_sync" &&
        observation.provider.authentication.kind ===
          "cardano_node_genesis_v1" &&
        observation.provider.authentication.publicIdentitySha256 ===
          configuredSource.genesisIdentitySha256,
    );
    const wrongGenesis = boundToAuthority.some(
      (observation) =>
        observation.provider.source.surface === "chain_sync" &&
        (observation.provider.authentication.kind !==
          "cardano_node_genesis_v1" ||
          observation.provider.authentication.publicIdentitySha256 !==
            configuredSource.genesisIdentitySha256),
    );
    if (wrongGenesis) {
      reasons.add("local_node_genesis_mismatch");
      alerts.add("watcher_local_node_authority_mismatch");
    }
    if (chainAuthorities.length !== 1) {
      reasons.add("missing_chain_sync_authority");
      alerts.add("watcher_local_node_chain_sync_missing");
    }
    const surfaces = boundToAuthority.map(
      (observation) => observation.provider.source.surface,
    );
    if (duplicateValues(providerIds)) {
      reasons.add("duplicate_provider_id");
      alerts.add("watcher_provider_identity_collision");
    }
    if (duplicateValues(surfaces)) {
      reasons.add("duplicate_local_surface");
      alerts.add("watcher_provider_identity_collision");
    }
    const authority = chainAuthorities[0] ?? null;
    const queries = boundToAuthority.filter(
      (observation) => observation.provider.source.surface !== "chain_sync",
    );
    const configuredQueries = new Map(
      configuredSource.queryServices.map((query) => [query.providerId, query]),
    );
    const unconfiguredQueries = queries.filter((query) => {
      const configured = configuredQueries.get(query.provider.providerId);
      return (
        configured === undefined ||
        configured.kind !== query.provider.source.surface
      );
    });
    if (unconfiguredQueries.length > 0) {
      reasons.add("unconfigured_local_query_service");
      alerts.add("watcher_local_node_query_evidence_missing");
    }
    const configuredQueryObservations = queries.filter((query) => {
      const configured = configuredQueries.get(query.provider.providerId);
      return configured?.kind === query.provider.source.surface;
    });
    queryObservationCount = configuredQueryObservations.length;
    if (authority !== null) {
      independentProviderCount = 1;
      chainAuthorityObservationDigest = authority.observationDigest;
      localQueryServiceBindings = Object.freeze(
        configuredSource.queryServices.map((configured) => {
          const matching = configuredQueryObservations.filter(
            (query) => query.provider.providerId === configured.providerId,
          );
          const query = matching.length === 1 ? matching[0]! : null;
          let observationStatus: WatcherLocalQueryServiceBindingV1["observationStatus"];
          if (query === null) {
            observationStatus = "unavailable";
            reasons.add("missing_local_query_evidence");
            alerts.add("watcher_local_node_query_evidence_missing");
          } else if (
            query.chainPoint.pointDigest === authority.chainPoint.pointDigest
          ) {
            if (query.blockContentDigest === authority.blockContentDigest) {
              observationStatus = "aligned";
            } else {
              observationStatus = "content_mismatch";
              reasons.add("block_content_mismatch");
              alerts.add("watcher_provider_content_disagreement");
            }
          } else if (
            BigInt(query.chainPoint.blockNo) >
            BigInt(authority.chainPoint.blockNo)
          ) {
            observationStatus = "rollback_not_propagated";
            reasons.add("rollback_not_propagated");
            alerts.add("watcher_local_node_rollback_not_propagated");
          } else if (
            query.chainPoint.blockNo === authority.chainPoint.blockNo
          ) {
            observationStatus = "forked";
            reasons.add("fork_disagreement");
            alerts.add("watcher_provider_fork");
          } else {
            observationStatus = "stale";
            reasons.add("stale_provider_observation");
            alerts.add("watcher_provider_stale");
          }
          return Object.freeze({
            kind: configured.kind,
            providerId: configured.providerId,
            observationStatus,
            observationDigest: query?.observationDigest ?? null,
          });
        }),
      );
      agreement = Object.freeze({
        pointDigest: authority.chainPoint.pointDigest,
        blockHash: authority.chainPoint.blockHash,
        slot: authority.chainPoint.slot,
        blockNo: authority.chainPoint.blockNo,
        minimumDepth: authority.chainPoint.depth,
        blockContentDigest: authority.blockContentDigest,
      });
    }
  } else {
    const configuredProviders = new Map(
      configuredSource.providers.map((provider) => [
        provider.providerId,
        provider.operatorIdentitySha256,
      ]),
    );
    const configuredEligible = eligible.filter((observation) => {
      const source = observation.provider.source;
      return (
        source.sourceMode === "external_providers" &&
        configuredProviders.get(observation.provider.providerId) ===
          source.operatorIdentitySha256
      );
    });
    if (configuredEligible.length !== eligible.length) {
      reasons.add("unconfigured_provider");
      alerts.add("watcher_provider_not_configured");
    }
    eligible = configuredEligible;
    const externalProviderIds = eligible.map(
      (observation) => observation.provider.providerId,
    );
    const externalTrustIdentities = eligible.map(
      (observation) =>
        `${observation.provider.authentication.kind}:${observation.provider.authentication.publicIdentitySha256}`,
    );
    const transportBound = eligible.filter(
      (observation) =>
        observation.provider.authentication.kind === "https_tls_identity_v1",
    );
    if (transportBound.length !== eligible.length) {
      reasons.add("provider_transport_mismatch");
      alerts.add("watcher_provider_transport_mismatch");
    }
    externalProviderBindings = Object.freeze(
      transportBound
        .map((observation) => {
          const source = observation.provider.source;
          if (source.sourceMode !== "external_providers") {
            throw new Error("unreachable source-mode mismatch");
          }
          return Object.freeze({
            providerId: observation.provider.providerId,
            operatorIdentitySha256: source.operatorIdentitySha256,
            authenticationKind: "https_tls_identity_v1" as const,
            publicIdentitySha256:
              observation.provider.authentication.publicIdentitySha256,
          });
        })
        .sort((left, right) =>
          left.providerId < right.providerId
            ? -1
            : left.providerId > right.providerId
              ? 1
              : 0,
        ),
    );
    if (duplicateValues(externalProviderIds)) {
      reasons.add("duplicate_provider_id");
      alerts.add("watcher_provider_identity_collision");
    }
    if (duplicateValues(externalTrustIdentities)) {
      reasons.add("duplicate_trust_identity");
      alerts.add("watcher_provider_identity_collision");
    }
    const operatorIdentities = eligible.map((observation) => {
      const source = observation.provider.source;
      return source.sourceMode === "external_providers"
        ? source.operatorIdentitySha256
        : "";
    });
    if (duplicateValues(operatorIdentities)) {
      reasons.add("duplicate_operator_identity");
      alerts.add("watcher_provider_identity_collision");
    }
    independentProviderCount = Math.min(
      new Set(externalProviderIds).size,
      new Set(externalTrustIdentities).size,
      new Set(operatorIdentities).size,
    );
    if (independentProviderCount < 2) {
      reasons.add("insufficient_independent_providers");
      alerts.add("watcher_provider_quorum_unavailable");
    }

    if (eligible.length >= 2) {
      const byHeight = new Map<string, Set<string>>();
      for (const observation of eligible) {
        const heightPoints =
          byHeight.get(observation.chainPoint.blockNo) ?? new Set<string>();
        heightPoints.add(observation.chainPoint.pointDigest);
        byHeight.set(observation.chainPoint.blockNo, heightPoints);
      }
      const pointDigests = new Set(
        eligible.map((observation) => observation.chainPoint.pointDigest),
      );
      if ([...byHeight.values()].some((points) => points.size > 1)) {
        reasons.add("fork_disagreement");
        alerts.add("watcher_provider_fork");
      } else if (pointDigests.size === 1) {
        const contentDigests = new Set(
          eligible.map((observation) => observation.blockContentDigest),
        );
        if (contentDigests.size > 1) {
          reasons.add("block_content_mismatch");
          alerts.add("watcher_provider_content_disagreement");
        } else {
          const first = eligible[0] as WatcherNormalizedL1BlockV1;
          agreement = Object.freeze({
            pointDigest: first.chainPoint.pointDigest,
            blockHash: first.chainPoint.blockHash,
            slot: first.chainPoint.slot,
            blockNo: first.chainPoint.blockNo,
            minimumDepth: minimumNatural(
              eligible.map((observation) => observation.chainPoint.depth),
            ),
            blockContentDigest: first.blockContentDigest,
          });
        }
      } else {
        const ordered = [...eligible].sort((left, right) => {
          const heightDifference =
            BigInt(left.chainPoint.blockNo) - BigInt(right.chainPoint.blockNo);
          return heightDifference < 0n ? -1 : heightDifference > 0n ? 1 : 0;
        });
        const nonMonotonic = ordered.some(
          (observation, index) =>
            index > 0 &&
            BigInt(observation.chainPoint.slot) <=
              BigInt(
                (ordered[index - 1] as WatcherNormalizedL1BlockV1).chainPoint
                  .slot,
              ),
        );
        const repeatedHash = duplicateValues(
          ordered.map((observation) => observation.chainPoint.blockHash),
        );
        if (nonMonotonic || repeatedHash) {
          reasons.add("fork_disagreement");
          alerts.add("watcher_provider_fork");
        } else {
          const lowest = BigInt(
            (ordered[0] as WatcherNormalizedL1BlockV1).chainPoint.blockNo,
          );
          const highest = BigInt(
            (ordered[ordered.length - 1] as WatcherNormalizedL1BlockV1)
              .chainPoint.blockNo,
          );
          if (
            highest - lowest <=
            BigInt(
              WATCHER_MULTI_PROVIDER_CONSISTENCY_V1_BOUNDS.compatibleBlockLag,
            )
          ) {
            hasPendingLag = true;
            reasons.add("bounded_provider_lag");
            alerts.add("watcher_provider_lag");
          } else {
            reasons.add("stale_provider_observation");
            alerts.add("watcher_provider_stale");
          }
        }
      }
    }
  }

  const quarantineReasons: ReadonlySet<WatcherMultiProviderReasonCodeV1> =
    new Set([
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
      "stale_provider_observation",
      "rollback_not_propagated",
      "fork_disagreement",
      "block_content_mismatch",
      "malformed_observation",
      "invalid_configured_network",
      "observation_limit_exceeded",
    ]);
  const mustQuarantine = [...reasons].some((reason) =>
    quarantineReasons.has(reason),
  );
  const status: WatcherMultiProviderConsistencyStatusV1 = mustQuarantine
    ? "quarantined"
    : hasPendingLag
      ? "pending"
      : agreement !== null
        ? "agreed"
        : "quarantined";
  if (status === "agreed") {
    reasons.add(
      configuredSource.sourceMode === "local_node"
        ? "local_node_consistent"
        : "providers_consistent",
    );
  } else {
    agreement = null;
  }

  const reasonCodes = sortCodes(
    reasons,
    WATCHER_MULTI_PROVIDER_REASON_CODES_V1,
  );
  const alertCodes = sortCodes(alerts, WATCHER_MULTI_PROVIDER_ALERT_CODES_V1);
  const observationEvidenceDigests = Object.freeze(
    valid.map(({ observationDigest }) => observationDigest).sort(),
  );

  return makeResult({
    schemaVersion: WATCHER_MULTI_PROVIDER_CONSISTENCY_V1_SCHEMA_VERSION,
    status,
    protocolDecision: status === "agreed" ? "allowed" : "quarantined",
    sourceMode: configuredSource.sourceMode,
    configuredNetwork,
    configuredSourceDigest: sha256Canonical(configuredSource),
    authorityNodeId:
      configuredSource.sourceMode === "local_node"
        ? configuredSource.authorityNodeId
        : null,
    authorityGenesisIdentitySha256:
      configuredSource.sourceMode === "local_node"
        ? configuredSource.genesisIdentitySha256
        : null,
    chainAuthorityObservationDigest,
    queryObservationCount,
    observationCount: inputs.length,
    independentProviderCount,
    externalProviderBindings,
    localQueryServiceBindings,
    reasonCodes,
    alertCodes,
    observationEvidenceDigests,
    rejectedObservationCount,
    agreement,
  });
};
