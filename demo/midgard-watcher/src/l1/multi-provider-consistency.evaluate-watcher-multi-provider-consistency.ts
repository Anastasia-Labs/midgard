import {
  isWatcherL1BlockAttestedBy,
  readWatcherLocalBackfillObservation,
  type WatcherL1TransportAttestationContext,
  watcherL1TransportAttestationDetails,
  type WatcherLocalBackfillObservationReceipt,
  type WatcherNormalizedL1Block,
} from "./l1-adapter.js";
import { evaluateAdmittedConsistency } from "./multi-provider-consistency.evaluate-admitted-consistency.js";
import {
  exactObservationArray,
  WATCHER_MULTI_PROVIDER_CONSISTENCY_BOUNDS,
  type WatcherL1SourceConsistencyConfig,
  type WatcherMultiProviderConsistency,
} from "./multi-provider-consistency.exact-observation-array.js";
import {
  type LocalObservationIdentity,
  parseConfiguredSource,
} from "./multi-provider-consistency.parse-configured-source.js";
import {
  parseNormalizedObservation,
  recognizedConfiguredSourceMode,
  rejectedBoundaryResult,
} from "./multi-provider-consistency.parse-normalized-observation.js";

/**
 * Produces a fail-closed consistency decision from already-normalized,
 * independently authenticated provider observations. No endpoint, credential,
 * operator database, or administration value is accepted as an input.
 */
export const evaluateWatcherMultiProviderConsistency = (
  configuredSourceInput: unknown,
  observationsInput: unknown,
  transportAttestationsInput: readonly WatcherL1TransportAttestationContext[],
): WatcherMultiProviderConsistency => {
  const sourceMode = recognizedConfiguredSourceMode(configuredSourceInput);
  let configuredSource: WatcherL1SourceConsistencyConfig | null;
  try {
    configuredSource = parseConfiguredSource(configuredSourceInput);
  } catch {
    return rejectedBoundaryResult(
      null,
      "invalid_configured_network",
      sourceMode,
    );
  }
  if (configuredSource === null) {
    return rejectedBoundaryResult(
      null,
      "invalid_configured_network",
      sourceMode,
    );
  }
  let transportAttestations: readonly WatcherL1TransportAttestationContext[];
  try {
    const parsed = exactObservationArray(transportAttestationsInput);
    if (
      parsed === null ||
      parsed.length > WATCHER_MULTI_PROVIDER_CONSISTENCY_BOUNDS.observations ||
      parsed.some(
        (candidate) => watcherL1TransportAttestationDetails(candidate) === null,
      )
    ) {
      return rejectedBoundaryResult(configuredSource, "malformed_observation");
    }
    transportAttestations =
      parsed as readonly WatcherL1TransportAttestationContext[];
  } catch {
    return rejectedBoundaryResult(configuredSource, "malformed_observation");
  }

  let inputs: readonly unknown[];
  try {
    const parsed = exactObservationArray(observationsInput);
    if (parsed === null) {
      return rejectedBoundaryResult(configuredSource, "malformed_observation");
    }
    if (
      parsed.length > WATCHER_MULTI_PROVIDER_CONSISTENCY_BOUNDS.observations
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

  const valid: WatcherNormalizedL1Block[] = [];
  let rejectedObservationCount = 0;
  for (const input of inputs) {
    try {
      const observation = parseNormalizedObservation(
        input,
        transportAttestations,
      );
      if (observation === null) {
        rejectedObservationCount += 1;
      } else {
        valid.push(observation);
      }
    } catch {
      rejectedObservationCount += 1;
    }
  }

  const localIdentities = new Map<
    WatcherNormalizedL1Block,
    LocalObservationIdentity
  >();
  for (const observation of valid) {
    const transport = transportAttestations.find((attestation) =>
      isWatcherL1BlockAttestedBy(observation, attestation),
    );
    const details = watcherL1TransportAttestationDetails(transport);
    if (details !== null)
      localIdentities.set(observation, {
        authorityBindingSha256: details.authorityBindingSha256,
        endpoint: details.transportEndpoint,
      });
  }
  return evaluateAdmittedConsistency(
    configuredSource,
    valid,
    rejectedObservationCount,
    transportAttestations,
    localIdentities,
  );
};

/** Evaluates only the three observations owned by a current local capture. */
export const evaluateWatcherLocalBackfillConsistency = (
  receipt: WatcherLocalBackfillObservationReceipt,
): WatcherMultiProviderConsistency => {
  const observation = readWatcherLocalBackfillObservation(receipt);
  const binding = observation.capture.sourceBinding;
  const configuredSource = parseConfiguredSource({
    ...binding,
    queryServices: binding.queryServices.map(
      ({ kind, providerId, endpoint }) => ({ kind, providerId, endpoint }),
    ),
  });
  if (configuredSource === null || configuredSource.sourceMode !== "local_node")
    throw new Error("local backfill configured source is invalid");
  const identities = new Map<
    WatcherNormalizedL1Block,
    LocalObservationIdentity
  >();
  identities.set(observation.native, {
    authorityBindingSha256: observation.acquisitionDigest,
    endpoint: binding.chainSyncSocketPath,
  });
  for (const block of [observation.ogmios, observation.kupo]) {
    const service = binding.queryServices.find(
      ({ providerId }) => providerId === block.provider.providerId,
    );
    if (service === undefined)
      throw new Error("local backfill configured query is absent");
    identities.set(block, {
      authorityBindingSha256: observation.acquisitionDigest,
      endpoint: service.endpoint,
    });
  }
  const result = evaluateAdmittedConsistency(
    configuredSource,
    [observation.native, observation.ogmios, observation.kupo],
    0,
    [],
    identities,
  );
  if (readWatcherLocalBackfillObservation(receipt) !== observation)
    throw new Error(
      "local backfill observation changed during consistency evaluation",
    );
  return result;
};
