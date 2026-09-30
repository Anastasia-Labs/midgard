import "./finality-engine.parse-watcher-finality-policy.js";

import {
  freezeStrings,
  sameMarker,
  sha256Canonical,
} from "./finality-engine.clone-external-providers.js";
import { makeState } from "./finality-engine.parse-watcher-finality-state.js";
import {
  type Agreement,
  type ExternalProviderBinding,
  type LocalQueryServiceBinding,
  type ParsedConsistency,
  WATCHER_FINALITY_RESULT_SCHEMA_VERSION,
  WATCHER_FINALITY_REWIND_INSTRUCTION_SCHEMA_VERSION,
  WATCHER_FINALITY_STATE_SCHEMA_VERSION,
  type WatcherFinalityAction,
  type WatcherFinalityAlertCode,
  type WatcherFinalityBoundObservation,
  type WatcherFinalityIncident,
  type WatcherFinalityLocalQueryService,
  type WatcherFinalityPolicy,
  type WatcherFinalityReasonCode,
  type WatcherFinalityResult,
  type WatcherFinalityRewindInstruction,
  type WatcherFinalityState,
} from "./finality-engine.watcher-finality-reason-codes.js";

export const result = (
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

export const rewind = (
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

export const requiredPreFinalityRollbackDepth = (
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

export const quarantineFinalized = (
  state: WatcherFinalityState,
  reasonCode: WatcherFinalityIncident["reasonCode"],
  triggerConsistencyDigest: string | null,
): WatcherFinalityResult => {
  const next = makeState({
    schemaVersion: WATCHER_FINALITY_STATE_SCHEMA_VERSION,
    policyDigest: state.policyDigest,
    network: state.network,
    blueprintHash: state.blueprintHash,
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

export const bindingFailure = (
  policy: WatcherFinalityPolicy,
  state: WatcherFinalityState,
): WatcherFinalityReasonCode | null => {
  if (state.network !== policy.network) {
    return "configured_network_mismatch";
  }
  if (state.blueprintHash !== policy.blueprintHash) {
    return "blueprint_mismatch";
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
export const sourceBindingFailureFor = (
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
