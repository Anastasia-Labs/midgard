import {
  cloneMarker,
  isExactEndpoint,
  sameMarker,
  sha256Canonical,
} from "./finality-engine.clone-external-providers.js";
import {
  INCIDENT_REASONS,
  parseBoundObservation,
  parseWatcherFinalityPolicy,
} from "./finality-engine.parse-watcher-finality-policy.js";
import {
  exactArray,
  exactPlainRecord,
  type ExternalProviderBinding,
  isHex32,
  isNetwork,
  WATCHER_FINALITY_STATE_SCHEMA_VERSION,
  type WatcherFinalityIncident,
  type WatcherFinalityPhase,
  type WatcherFinalityPolicy,
  type WatcherFinalityState,
} from "./finality-engine.watcher-finality-reason-codes.js";
import { WATCHER_MULTI_PROVIDER_CONSISTENCY_BOUNDS } from "./multi-provider-consistency.js";

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

export const makeState = (
  value: Omit<WatcherFinalityState, "stateDigest">,
): WatcherFinalityState => {
  const canonical = {
    schemaVersion: WATCHER_FINALITY_STATE_SCHEMA_VERSION,
    policyDigest: value.policyDigest,
    network: value.network,
    blueprintHash: value.blueprintHash,
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
  state.blueprintHash === policy.blueprintHash &&
  sameMarker(state.deploymentMarker, policy.deploymentMarker);

export const stateSemanticsAreValid = (
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
    if (visibilityCount === 1n) {
      return (
        currentDepth === firstSeenDepth &&
        observation.firstSeenConsistencyDigest ===
          observation.lastSeenConsistencyDigest
      );
    }
    return (
      currentDepth < BigInt(policy.confirmationDepth) &&
      currentDepth > firstSeenDepth
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
      "blueprintHash",
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
      !isHex32(state.blueprintHash) ||
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
      blueprintHash: state.blueprintHash,
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

export const initialState = (
  policy: WatcherFinalityPolicy,
): WatcherFinalityState =>
  makeState({
    schemaVersion: WATCHER_FINALITY_STATE_SCHEMA_VERSION,
    policyDigest: policy.policyDigest,
    network: policy.network,
    blueprintHash: policy.blueprintHash,
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

export const parseStringArray = (
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

export const sameStringArray = (
  left: readonly string[],
  right: readonly string[],
): boolean =>
  left.length === right.length &&
  left.every((value, index) => value === right[index]);

export const parseExternalProviderBindings = (
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
