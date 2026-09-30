import { watcherSha256CanonicalJson } from "../storage/durable-store.js";
import {
  encodeWatcherNormalizedL1Block,
  isWatcherL1BlockAttestedBy,
  normalizeWatcherL1Block,
  WATCHER_AUTHENTICATED_L1_PROVIDER_SCHEMA_VERSION,
  WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
  WATCHER_NORMALIZED_L1_BLOCK_SCHEMA_VERSION,
  type WatcherL1SourceModeV1,
  type WatcherL1TransportAttestationContext,
  watcherL1TransportAttestationDetails,
  type WatcherNormalizedL1Block,
} from "./l1-adapter.js";
import {
  type ConsistencyResultWithoutDigest,
  exactObservationArray,
  exactPlainRecord,
  isHex32,
  isNatural,
  isNetwork,
  type PlainRecord,
  sameBytes,
  WATCHER_MULTI_PROVIDER_ALERT_CODES,
  WATCHER_MULTI_PROVIDER_CONSISTENCY_SCHEMA_VERSION,
  WATCHER_MULTI_PROVIDER_REASON_CODES,
  type WatcherExternalProviderBinding,
  type WatcherL1SourceConsistencyConfig,
  type WatcherMultiProviderAlertCode,
  type WatcherMultiProviderConsistency,
  type WatcherMultiProviderReasonCode,
} from "./multi-provider-consistency.exact-observation-array.js";

const rawTransactionsFromNormalized = (
  value: unknown,
): readonly PlainRecord[] | null => {
  const inputs = exactObservationArray(value);
  if (inputs === null) {
    return null;
  }
  const transactions: PlainRecord[] = [];
  for (const input of inputs) {
    const withoutIndex = exactPlainRecord(input, [
      "txHash",
      "isValid",
      "fullTransaction",
      "body",
      "witnessSet",
      "utxos",
      "scripts",
      "datums",
      "redeemers",
    ]);
    const withIndex =
      withoutIndex === null
        ? exactPlainRecord(input, [
            "txHash",
            "transactionIndex",
            "isValid",
            "fullTransaction",
            "body",
            "witnessSet",
            "utxos",
            "scripts",
            "datums",
            "redeemers",
          ])
        : null;
    const transaction = withoutIndex ?? withIndex;
    if (transaction === null || typeof transaction.isValid !== "boolean") {
      return null;
    }
    transactions.push({
      txHash: transaction.txHash,
      ...(withIndex === null
        ? {}
        : { transactionIndex: transaction.transactionIndex }),
      fullTransaction: transaction.fullTransaction,
      body: transaction.body,
      witnessSet: transaction.witnessSet,
      utxos: transaction.utxos,
      scripts: transaction.scripts,
      datums: transaction.datums,
      redeemers: transaction.redeemers,
    });
  }
  return transactions;
};

/**
 * Re-validates an adapter result at this trust boundary. Re-normalization
 * prevents callers from forging provider, point, content, or observation
 * digests on an object merely asserted to have the public TypeScript type.
 */
export const parseNormalizedObservation = (
  value: unknown,
  transportAttestations: readonly WatcherL1TransportAttestationContext[],
): WatcherNormalizedL1Block | null => {
  for (const attestation of transportAttestations) {
    if (isWatcherL1BlockAttestedBy(value, attestation)) {
      return value as WatcherNormalizedL1Block;
    }
  }
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
    block.schemaVersion !== WATCHER_NORMALIZED_L1_BLOCK_SCHEMA_VERSION ||
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
    "parentBlockHash",
    "slot",
    "blockNo",
    "depth",
  ]);
  if (
    point === null ||
    !isHex32(point.chainPointId) ||
    !isHex32(point.pointDigest) ||
    !isHex32(point.blockHash) ||
    (point.parentBlockHash !== null && !isHex32(point.parentBlockHash)) ||
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
    provider.schemaVersion !== WATCHER_AUTHENTICATED_L1_PROVIDER_SCHEMA_VERSION
  ) {
    return null;
  }
  const rawTransactions = rawTransactionsFromNormalized(block.transactions);
  if (rawTransactions === null) {
    return null;
  }

  for (const attestation of transportAttestations) {
    const details = watcherL1TransportAttestationDetails(attestation);
    if (
      details === null ||
      details.provider.providerId !== provider.providerId ||
      details.provider.network !== block.network
    ) {
      continue;
    }
    const normalized = normalizeWatcherL1Block(attestation, {
      schemaVersion: WATCHER_L1_BLOCK_OBSERVATION_SCHEMA_VERSION,
      network: block.network,
      providerId: provider.providerId,
      chainPoint: {
        blockHash: point.blockHash,
        parentBlockHash: point.parentBlockHash,
        slot: point.slot,
        blockNo: point.blockNo,
        depth: point.depth,
      },
      transactions: rawTransactions,
    });
    if (
      normalized.chainPoint.chainPointId === point.chainPointId &&
      normalized.chainPoint.pointDigest === point.pointDigest &&
      normalized.blockContentDigest === block.blockContentDigest &&
      normalized.observationDigest === block.observationDigest &&
      sameBytes(
        encodeWatcherNormalizedL1Block(normalized),
        encodeWatcherNormalizedL1Block(
          block as unknown as WatcherNormalizedL1Block,
        ),
      )
    ) {
      return normalized;
    }
  }
  return null;
};

export const sha256Canonical = watcherSha256CanonicalJson;

export const sortCodes = <T extends string>(
  values: ReadonlySet<T>,
  order: readonly T[],
): readonly T[] =>
  Object.freeze(order.filter((candidate) => values.has(candidate)));

export const minimumNatural = (values: readonly string[]): string =>
  values.reduce((minimum, candidate) =>
    BigInt(candidate) < BigInt(minimum) ? candidate : minimum,
  );

export const duplicateValues = (values: readonly string[]): boolean =>
  new Set(values).size !== values.length;

export const compareExternalProviderBindings = (
  left: WatcherExternalProviderBinding,
  right: WatcherExternalProviderBinding,
): number => {
  for (const key of [
    "providerId",
    "operatorIdentitySha256",
    "authenticationKind",
    "publicIdentitySha256",
    "endpoint",
  ] as const) {
    if (left[key] < right[key]) {
      return -1;
    }
    if (left[key] > right[key]) {
      return 1;
    }
  }
  return 0;
};

export const makeResult = (
  value: ConsistencyResultWithoutDigest,
): WatcherMultiProviderConsistency => {
  const consistencyDigest = sha256Canonical(value);
  return Object.freeze({
    ...value,
    consistencyDigest,
  });
};

export const rejectedBoundaryResult = (
  configuredSource: WatcherL1SourceConsistencyConfig | null,
  reason: WatcherMultiProviderReasonCode,
  recognizedSourceMode: WatcherL1SourceModeV1 | null = configuredSource?.sourceMode ??
    null,
): WatcherMultiProviderConsistency => {
  const reasons = new Set<WatcherMultiProviderReasonCode>([reason]);
  const alerts = new Set<WatcherMultiProviderAlertCode>([
    "watcher_provider_observation_rejected",
  ]);
  if (recognizedSourceMode === "external_providers") {
    reasons.add("insufficient_independent_providers");
    alerts.add("watcher_provider_quorum_unavailable");
  }
  return makeResult({
    schemaVersion: WATCHER_MULTI_PROVIDER_CONSISTENCY_SCHEMA_VERSION,
    status: "quarantined",
    protocolDecision: "quarantined",
    sourceMode: recognizedSourceMode,
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
    authorityChainSyncSocketPath:
      configuredSource?.sourceMode === "local_node"
        ? configuredSource.chainSyncSocketPath
        : null,
    chainAuthorityObservationDigest: null,
    queryObservationCount: 0,
    observationCount: 0,
    independentProviderCount: 0,
    externalProviderBindings: Object.freeze([]),
    localQueryServiceBindings: Object.freeze([]),
    reasonCodes: sortCodes(reasons, WATCHER_MULTI_PROVIDER_REASON_CODES),
    alertCodes: sortCodes(alerts, WATCHER_MULTI_PROVIDER_ALERT_CODES),
    observationEvidenceDigests: Object.freeze([]),
    rejectedObservationCount: 1,
    agreement: null,
  });
};

export const recognizedConfiguredSourceMode = (
  input: unknown,
): WatcherL1SourceModeV1 | null => {
  try {
    if (typeof input !== "object" || input === null || Array.isArray(input)) {
      return null;
    }
    const descriptor = Object.getOwnPropertyDescriptor(input, "sourceMode");
    return descriptor?.enumerable === true &&
      descriptor.get === undefined &&
      descriptor.set === undefined &&
      (descriptor.value === "local_node" ||
        descriptor.value === "external_providers")
      ? descriptor.value
      : null;
  } catch {
    return null;
  }
};
