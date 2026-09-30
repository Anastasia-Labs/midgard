import {
  endpointAlias,
  exactObservationArray,
  exactPlainRecord,
  isExactAbsoluteSocketPath,
  isExactEndpoint,
  isHex32,
  isNetwork,
  WATCHER_MULTI_PROVIDER_CONSISTENCY_BOUNDS,
  type WatcherConfiguredExternalProvider,
  type WatcherConfiguredLocalQueryService,
  type WatcherL1SourceConsistencyConfig,
} from "./multi-provider-consistency.exact-observation-array.js";
import { duplicateValues } from "./multi-provider-consistency.parse-normalized-observation.js";

export const parseConfiguredSource = (
  input: unknown,
): WatcherL1SourceConsistencyConfig | null => {
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
        WATCHER_MULTI_PROVIDER_CONSISTENCY_BOUNDS.observations
    ) {
      return null;
    }
    const providers: WatcherConfiguredExternalProvider[] = [];
    for (const input of providerInputs) {
      const provider = exactPlainRecord(input, [
        "providerId",
        "operatorIdentitySha256",
        "endpoint",
      ]);
      if (
        provider === null ||
        typeof provider.providerId !== "string" ||
        !/^[a-z][a-z0-9-]{0,62}$/u.test(provider.providerId) ||
        !isHex32(provider.operatorIdentitySha256) ||
        !isExactEndpoint(provider.endpoint, ["https:"])
      ) {
        return null;
      }
      providers.push(
        Object.freeze({
          providerId: provider.providerId,
          operatorIdentitySha256: provider.operatorIdentitySha256,
          endpoint: provider.endpoint,
        }),
      );
    }
    if (
      duplicateValues(providers.map(({ providerId }) => providerId)) ||
      duplicateValues(
        providers.map(({ operatorIdentitySha256 }) => operatorIdentitySha256),
      ) ||
      duplicateValues(providers.map(({ endpoint }) => endpointAlias(endpoint)))
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
    "chainSyncSocketPath",
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
    !isExactAbsoluteSocketPath(local.chainSyncSocketPath) ||
    queryInputs === null ||
    queryInputs.length > 8
  ) {
    return null;
  }
  const queryServices: WatcherConfiguredLocalQueryService[] = [];
  for (const input of queryInputs) {
    const query = exactPlainRecord(input, ["kind", "providerId", "endpoint"]);
    if (
      query === null ||
      !["ogmios", "kupo", "kupmios", "db_sync"].includes(
        query.kind as string,
      ) ||
      typeof query.providerId !== "string" ||
      !/^[a-z][a-z0-9-]{0,62}$/u.test(query.providerId) ||
      !isExactEndpoint(
        query.endpoint,
        query.kind === "ogmios"
          ? ["http:", "https:", "ws:", "wss:"]
          : query.kind === "kupo"
            ? ["http:", "https:"]
            : query.kind === "kupmios"
              ? ["http:", "https:", "ws:", "wss:"]
              : ["postgresql:"],
      )
    ) {
      return null;
    }
    queryServices.push(
      Object.freeze({
        kind: query.kind as WatcherConfiguredLocalQueryService["kind"],
        providerId: query.providerId,
        endpoint: query.endpoint,
      }),
    );
  }
  if (
    duplicateValues(queryServices.map(({ providerId }) => providerId)) ||
    duplicateValues(
      queryServices.map(({ kind, providerId }) => `${kind}:${providerId}`),
    ) ||
    duplicateValues(
      queryServices.map(({ endpoint }) => {
        return endpointAlias(endpoint);
      }),
    )
  ) {
    return null;
  }
  return Object.freeze({
    sourceMode: "local_node",
    network: local.network,
    authorityNodeId: local.authorityNodeId,
    genesisIdentitySha256: local.genesisIdentitySha256,
    chainSyncSocketPath: local.chainSyncSocketPath,
    queryServices: Object.freeze(
      queryServices.sort((left, right) =>
        left.providerId.localeCompare(right.providerId),
      ),
    ),
  });
};

export type LocalObservationIdentity = Readonly<{
  authorityBindingSha256: string | null;
  endpoint: string;
}>;
