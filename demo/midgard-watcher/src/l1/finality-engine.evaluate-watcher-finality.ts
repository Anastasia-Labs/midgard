import {
  bindingFailure,
  quarantineFinalized,
  requiredPreFinalityRollbackDepth,
  result,
  rewind,
  sourceBindingFailureFor,
} from "./finality-engine.external-provider-bindings-match-policy.js";
import {
  makeBoundObservation,
  parseConsistency,
} from "./finality-engine.parse-consistency.js";
import { parseWatcherFinalityPolicy } from "./finality-engine.parse-watcher-finality-policy.js";
import {
  initialState,
  makeState,
  parseWatcherFinalityState,
  stateSemanticsAreValid,
} from "./finality-engine.parse-watcher-finality-state.js";
import {
  WATCHER_FINALITY_STATE_SCHEMA_VERSION,
  type WatcherFinalityBoundObservation,
  type WatcherFinalityResult,
  type WatcherFinalityRewindInstruction,
} from "./finality-engine.watcher-finality-reason-codes.js";

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
      blueprintHash: policy.blueprintHash,
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
      blueprintHash: policy.blueprintHash,
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
      blueprintHash: policy.blueprintHash,
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
    blueprintHash: policy.blueprintHash,
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
