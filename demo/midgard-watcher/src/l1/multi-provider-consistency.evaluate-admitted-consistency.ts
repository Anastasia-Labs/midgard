import {
  isWatcherL1BlockAttestedBy,
  type WatcherL1TransportAttestationContext,
  watcherL1TransportAttestationDetails,
  type WatcherNormalizedL1Block,
} from "./l1-adapter.js";
import {
  WATCHER_MULTI_PROVIDER_ALERT_CODES,
  WATCHER_MULTI_PROVIDER_CONSISTENCY_BOUNDS,
  WATCHER_MULTI_PROVIDER_CONSISTENCY_SCHEMA_VERSION,
  WATCHER_MULTI_PROVIDER_REASON_CODES,
  type WatcherExternalProviderBinding,
  type WatcherL1SourceConsistencyConfig,
  type WatcherLocalQueryServiceBinding,
  type WatcherMultiProviderAgreement,
  type WatcherMultiProviderAlertCode,
  type WatcherMultiProviderConsistency,
  type WatcherMultiProviderConsistencyStatus,
  type WatcherMultiProviderReasonCode,
} from "./multi-provider-consistency.exact-observation-array.js";
import { type LocalObservationIdentity } from "./multi-provider-consistency.parse-configured-source.js";
import {
  compareExternalProviderBindings,
  duplicateValues,
  makeResult,
  minimumNatural,
  sha256Canonical,
  sortCodes,
} from "./multi-provider-consistency.parse-normalized-observation.js";

// Both callers own the admitted observations. The lookup is never public.
export const evaluateAdmittedConsistency = (
  configuredSource: WatcherL1SourceConsistencyConfig,
  valid: readonly WatcherNormalizedL1Block[],
  rejectedObservationCount: number,
  transportAttestations: readonly WatcherL1TransportAttestationContext[],
  localIdentities: ReadonlyMap<
    WatcherNormalizedL1Block,
    LocalObservationIdentity
  >,
): WatcherMultiProviderConsistency => {
  const configuredNetwork = configuredSource.network;
  const reasons = new Set<WatcherMultiProviderReasonCode>();
  const alerts = new Set<WatcherMultiProviderAlertCode>();
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
  let agreement: WatcherMultiProviderAgreement | null = null;
  let hasPendingLag = false;
  let independentProviderCount = 0;
  let chainAuthorityObservationDigest: string | null = null;
  let queryObservationCount = 0;
  let externalProviderBindings: readonly WatcherExternalProviderBinding[] =
    Object.freeze([]);
  let localQueryServiceBindings: readonly WatcherLocalQueryServiceBinding[] =
    Object.freeze([]);

  if (configuredSource.sourceMode === "local_node") {
    const local = eligible.filter(
      (
        observation,
      ): observation is WatcherNormalizedL1Block & {
        readonly provider: WatcherNormalizedL1Block["provider"] & {
          readonly source: Extract<
            WatcherNormalizedL1Block["provider"]["source"],
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
    if (duplicateValues(providerIds)) {
      reasons.add("duplicate_provider_id");
      alerts.add("watcher_provider_identity_collision");
    }
    const authority = chainAuthorities[0] ?? null;
    const authorityIdentity =
      authority === null ? undefined : localIdentities.get(authority);
    const authorityBinding = authorityIdentity?.authorityBindingSha256 ?? null;
    const authorityTransportEndpoint = authorityIdentity?.endpoint ?? null;
    if (
      authorityTransportEndpoint !== null &&
      authorityTransportEndpoint !== configuredSource.chainSyncSocketPath
    ) {
      reasons.add("provider_transport_mismatch");
      alerts.add("watcher_provider_transport_mismatch");
    }
    const queries = boundToAuthority.filter(
      (observation) => observation.provider.source.surface !== "chain_sync",
    );
    const queryTransportMismatch = queries.some((observation) => {
      const configured = configuredSource.queryServices.find(
        ({ providerId }) => providerId === observation.provider.providerId,
      );
      const endpoint = localIdentities.get(observation)?.endpoint ?? null;
      return configured !== undefined && endpoint !== configured.endpoint;
    });
    if (queryTransportMismatch) {
      reasons.add("provider_transport_mismatch");
      alerts.add("watcher_provider_transport_mismatch");
    }
    const queriesBoundToAuthority = queries.filter((observation) => {
      const identity = localIdentities.get(observation);
      const configured = configuredSource.queryServices.find(
        ({ providerId }) => providerId === observation.provider.providerId,
      );
      return (
        authorityBinding !== null &&
        (identity?.authorityBindingSha256 ?? null) === authorityBinding &&
        configured !== undefined &&
        (identity?.endpoint ?? null) === configured.endpoint
      );
    });
    if (queriesBoundToAuthority.length !== queries.length) {
      reasons.add("local_node_authority_mismatch");
      alerts.add("watcher_local_node_authority_mismatch");
    }
    const configuredQueries = new Map(
      configuredSource.queryServices.map((query) => [query.providerId, query]),
    );
    const unconfiguredQueries = queriesBoundToAuthority.filter((query) => {
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
    const configuredQueryObservations = queriesBoundToAuthority.filter(
      (query) => {
        const configured = configuredQueries.get(query.provider.providerId);
        return configured?.kind === query.provider.source.surface;
      },
    );
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
          let observationStatus: WatcherLocalQueryServiceBinding["observationStatus"];
          if (query === null) {
            observationStatus = "unavailable";
            reasons.add("missing_local_query_evidence");
            alerts.add("watcher_local_node_query_evidence_missing");
          } else if (
            query.chainPoint.pointDigest === authority.chainPoint.pointDigest
          ) {
            /*
             * Kupo's authenticated surface is its exact checkpoint, not a
             * Cardano block-body API. A Kupo observation is therefore
             * intentionally point-only (zero claimed transactions). The
             * paired Ogmios observation must still reproduce the native
             * chain-sync block content exactly. Treating Ogmios bytes as if
             * Kupo supplied them would manufacture cross-source agreement.
             */
            if (configured.kind === "kupo" && query.transactions.length === 0) {
              observationStatus = "aligned";
            } else if (
              configured.kind !== "kupo" &&
              query.blockContentDigest === authority.blockContentDigest
            ) {
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
            endpoint: configured.endpoint,
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
        provider,
      ]),
    );
    const configuredEligible = eligible.filter((observation) => {
      const source = observation.provider.source;
      const configured = configuredProviders.get(
        observation.provider.providerId,
      );
      const transport = transportAttestations.find((attestation) =>
        isWatcherL1BlockAttestedBy(observation, attestation),
      );
      const transportEndpoint =
        transport === undefined
          ? null
          : (watcherL1TransportAttestationDetails(transport)
              ?.transportEndpoint ?? null);
      return (
        source.sourceMode === "external_providers" &&
        configured?.operatorIdentitySha256 === source.operatorIdentitySha256 &&
        configured.endpoint === transportEndpoint
      );
    });
    const configuredIdentityButWrongEndpoint = eligible.some((observation) => {
      const source = observation.provider.source;
      const configured = configuredProviders.get(
        observation.provider.providerId,
      );
      if (
        source.sourceMode !== "external_providers" ||
        configured?.operatorIdentitySha256 !== source.operatorIdentitySha256
      ) {
        return false;
      }
      const transport = transportAttestations.find((attestation) =>
        isWatcherL1BlockAttestedBy(observation, attestation),
      );
      return (
        watcherL1TransportAttestationDetails(transport)?.transportEndpoint !==
        configured.endpoint
      );
    });
    if (configuredIdentityButWrongEndpoint) {
      reasons.add("provider_transport_mismatch");
      alerts.add("watcher_provider_transport_mismatch");
    }
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
            endpoint: configuredProviders.get(observation.provider.providerId)!
              .endpoint,
          });
        })
        .sort(compareExternalProviderBindings),
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
      const observationsByPoint = new Map<
        string,
        {
          readonly observation: WatcherNormalizedL1Block;
          readonly contentDigests: Set<string>;
        }
      >();
      for (const observation of eligible) {
        const pointDigest = observation.chainPoint.pointDigest;
        const existing = observationsByPoint.get(pointDigest);
        if (existing === undefined) {
          observationsByPoint.set(pointDigest, {
            observation,
            contentDigests: new Set([observation.blockContentDigest]),
          });
        } else {
          existing.contentDigests.add(observation.blockContentDigest);
        }
      }
      const hasPointContentMismatch = [...observationsByPoint.values()].some(
        ({ contentDigests }) => contentDigests.size > 1,
      );
      if (hasPointContentMismatch) {
        reasons.add("block_content_mismatch");
        alerts.add("watcher_provider_content_disagreement");
      }
      if ([...byHeight.values()].some((points) => points.size > 1)) {
        reasons.add("fork_disagreement");
        alerts.add("watcher_provider_fork");
      } else if (pointDigests.size === 1) {
        if (!hasPointContentMismatch) {
          const first = eligible[0] as WatcherNormalizedL1Block;
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
        const ordered = [...observationsByPoint.values()]
          .map(({ observation }) => observation)
          .sort((left, right) => {
            const heightDifference =
              BigInt(left.chainPoint.blockNo) -
              BigInt(right.chainPoint.blockNo);
            return heightDifference < 0n ? -1 : heightDifference > 0n ? 1 : 0;
          });
        const nonMonotonic = ordered.some(
          (observation, index) =>
            index > 0 &&
            BigInt(observation.chainPoint.slot) <=
              BigInt(
                (ordered[index - 1] as WatcherNormalizedL1Block).chainPoint
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
            (ordered[0] as WatcherNormalizedL1Block).chainPoint.blockNo,
          );
          const highest = BigInt(
            (ordered[ordered.length - 1] as WatcherNormalizedL1Block).chainPoint
              .blockNo,
          );
          if (
            highest - lowest <=
            BigInt(WATCHER_MULTI_PROVIDER_CONSISTENCY_BOUNDS.compatibleBlockLag)
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

  const quarantineReasons: ReadonlySet<WatcherMultiProviderReasonCode> =
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
  const status: WatcherMultiProviderConsistencyStatus = mustQuarantine
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

  const reasonCodes = sortCodes(reasons, WATCHER_MULTI_PROVIDER_REASON_CODES);
  const alertCodes = sortCodes(alerts, WATCHER_MULTI_PROVIDER_ALERT_CODES);
  const observationEvidenceDigests = Object.freeze(
    valid.map(({ observationDigest }) => observationDigest).sort(),
  );

  return makeResult({
    schemaVersion: WATCHER_MULTI_PROVIDER_CONSISTENCY_SCHEMA_VERSION,
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
    authorityChainSyncSocketPath:
      configuredSource.sourceMode === "local_node"
        ? configuredSource.chainSyncSocketPath
        : null,
    chainAuthorityObservationDigest,
    queryObservationCount,
    observationCount: valid.length + rejectedObservationCount,
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
