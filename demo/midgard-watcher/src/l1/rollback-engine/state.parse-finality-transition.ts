import {
  encodeWatcherDurableStore,
  type WatcherDurableStore,
  watcherDurableStoreBytesSha256,
  watcherSameCanonicalJson,
} from "../../storage/durable-store.js";
import {
  evaluateWatcherFinality,
  parseWatcherFinalityState,
  WATCHER_FINALITY_RESULT_SCHEMA_VERSION,
  type WatcherFinalityBoundObservation,
  type WatcherFinalityIncident,
  type WatcherFinalityPolicy,
  type WatcherFinalityState,
} from ".././finality-engine.js";
import { type WatcherMultiProviderConsistency } from ".././multi-provider-consistency.js";
import {
  exactPlainRecord,
  exactStringArray,
  parseInstruction,
  sameBinding,
  sameStrings,
  stateBindingFailure,
} from "./records.js";
import {
  HEX_32,
  type ParsedFinalityTransition,
  sha256Canonical,
  WATCHER_ROLLBACK_INCIDENT_SCHEMA_VERSION,
  WATCHER_ROLLBACK_STATE_SCHEMA_VERSION,
  type WatcherRollbackEpochCheckpoint,
  type WatcherRollbackIncident,
  type WatcherRollbackReasonCode,
  type WatcherRollbackState,
  type WatcherRollbackTransition,
} from "./types.js";

export const parseFinalityTransition = (
  policy: WatcherFinalityPolicy,
  previousInput: unknown,
  consistencyInput: unknown,
  resultInput: unknown,
): ParsedFinalityTransition | WatcherRollbackReasonCode => {
  const previous = parseWatcherFinalityState(previousInput);
  if (previous === null) {
    return "malformed_previous_finality_state";
  }
  const previousBindingFailure = stateBindingFailure(policy, previous);
  if (previousBindingFailure !== null) {
    return previousBindingFailure;
  }
  if (parseWatcherFinalityState(previous, policy) === null) {
    return "malformed_previous_finality_state";
  }
  const consistencyRecord = exactPlainRecord(consistencyInput, [
    "schemaVersion",
    "status",
    "protocolDecision",
    "sourceMode",
    "configuredNetwork",
    "configuredSourceDigest",
    "authorityNodeId",
    "authorityGenesisIdentitySha256",
    "authorityChainSyncSocketPath",
    "chainAuthorityObservationDigest",
    "queryObservationCount",
    "observationCount",
    "independentProviderCount",
    "externalProviderBindings",
    "localQueryServiceBindings",
    "reasonCodes",
    "alertCodes",
    "observationEvidenceDigests",
    "rejectedObservationCount",
    "agreement",
    "consistencyDigest",
  ]);
  if (
    consistencyRecord === null ||
    consistencyRecord.schemaVersion !==
      "midgard-watcher-multi-provider-consistency-v1" ||
    typeof consistencyRecord.consistencyDigest !== "string" ||
    !HEX_32.test(consistencyRecord.consistencyDigest)
  ) {
    return "finality_provenance_mismatch";
  }
  const recomputed = evaluateWatcherFinality(
    policy,
    previous,
    consistencyInput,
  );
  if (!watcherSameCanonicalJson(recomputed, resultInput)) {
    return "finality_provenance_mismatch";
  }
  const consistency = consistencyInput as WatcherMultiProviderConsistency;

  const result = exactPlainRecord(resultInput, [
    "schemaVersion",
    "action",
    "protocolDecision",
    "reasonCodes",
    "alertCodes",
    "state",
    "rewindInstruction",
    "resultDigest",
  ]);
  if (
    result === null ||
    result.schemaVersion !== WATCHER_FINALITY_RESULT_SCHEMA_VERSION ||
    typeof result.resultDigest !== "string" ||
    !HEX_32.test(result.resultDigest)
  ) {
    return "malformed_finality_result";
  }
  const reasons = exactStringArray(result.reasonCodes, [
    "pending_depth_regression",
    "pending_point_changed",
    "pending_content_changed",
    "post_finality_point_changed",
    "post_finality_content_changed",
    "post_finality_contradiction",
  ]);
  const alerts = exactStringArray(result.alertCodes, [
    "watcher_finality_rewind_required",
    "watcher_finality_pending",
    "watcher_finality_post_finality_incident",
  ]);
  const next = parseWatcherFinalityState(result.state);
  if (reasons === null || alerts === null || next === null) {
    return "malformed_finality_result";
  }
  const nextBindingFailure = stateBindingFailure(policy, next);
  if (nextBindingFailure !== null) {
    return nextBindingFailure;
  }
  if (parseWatcherFinalityState(next, policy) === null) {
    return "malformed_finality_result";
  }
  const instruction =
    result.rewindInstruction === null
      ? null
      : parseInstruction(result.rewindInstruction);
  if (result.rewindInstruction !== null && instruction === null) {
    return "malformed_finality_result";
  }
  const canonical = {
    schemaVersion: WATCHER_FINALITY_RESULT_SCHEMA_VERSION,
    action: result.action,
    protocolDecision: result.protocolDecision,
    reasonCodes: reasons,
    alertCodes: alerts,
    state: next,
    rewindInstruction: instruction,
  };
  if (sha256Canonical(canonical) !== result.resultDigest) {
    return "malformed_finality_result";
  }

  if (
    result.action === "rewind_pending" &&
    result.protocolDecision === "rewind_required" &&
    previous.phase === "pending" &&
    previous.pending !== null &&
    next.phase === "pending" &&
    next.pending !== null &&
    instruction !== null &&
    instruction.discardedStateDigest === previous.stateDigest &&
    instruction.replacementPointDigest === next.pending.pointDigest &&
    instruction.replacementContentDigest === next.pending.blockContentDigest &&
    instruction.replacementDepth === next.pending.currentDepth &&
    sameStrings(reasons, [instruction.kind]) &&
    sameStrings(alerts, [
      "watcher_finality_rewind_required",
      "watcher_finality_pending",
    ])
  ) {
    return Object.freeze({
      kind: "rewind",
      previous,
      next,
      instruction,
      consistency,
      finalityResult: recomputed,
    });
  }

  const finalityIncident = next.incident;
  if (
    result.action === "quarantine_incident" &&
    result.protocolDecision === "quarantined" &&
    previous.phase === "finalized" &&
    previous.finalized !== null &&
    next.phase === "quarantined" &&
    next.pending === null &&
    next.finalized !== null &&
    sameBinding(previous.finalized, next.finalized) &&
    finalityIncident !== null &&
    finalityIncident.triggerConsistencyDigest ===
      consistency.consistencyDigest &&
    instruction === null &&
    sameStrings(reasons, [
      finalityIncident.reasonCode,
      "post_finality_contradiction",
    ]) &&
    sameStrings(alerts, ["watcher_finality_post_finality_incident"]) &&
    finalityIncident.incidentDigest ===
      sha256Canonical({
        reasonCode: finalityIncident.reasonCode,
        triggerConsistencyDigest: finalityIncident.triggerConsistencyDigest,
        priorStateDigest: previous.stateDigest,
      })
  ) {
    return Object.freeze({
      kind: "incident",
      previous,
      next,
      consistency,
      finalityResult: recomputed,
    });
  }
  return "invalid_finality_transition";
};

export const makeIncident = (
  policy: WatcherFinalityPolicy,
  transition: Extract<ParsedFinalityTransition, { readonly kind: "incident" }>,
  sourceStoreDigest: string,
  nextStoreDigest: string,
  transitionCount: string,
): WatcherRollbackIncident => {
  const finalityIncident = transition.next.incident as WatcherFinalityIncident;
  const finalizedBinding = transition.next
    .finalized as WatcherFinalityBoundObservation;
  const canonical = {
    schemaVersion: WATCHER_ROLLBACK_INCIDENT_SCHEMA_VERSION,
    reasonCode: finalityIncident.reasonCode,
    policyDigest: policy.policyDigest,
    blueprintHash: policy.blueprintHash,
    deploymentMarker: policy.deploymentMarker,
    finalityStateDigest: transition.next.stateDigest,
    finalityIncidentDigest: finalityIncident.incidentDigest,
    triggerConsistencyDigest: finalityIncident.triggerConsistencyDigest,
    finalizedBinding,
    sourceStoreDigest,
    nextStoreDigest,
    transitionCount,
    previousFinalityStateDigest: transition.previous.stateDigest,
    consistencyDigest: transition.consistency.consistencyDigest,
    finalityResultDigest: transition.finalityResult.resultDigest,
  };
  return Object.freeze({
    ...canonical,
    incidentDigest: sha256Canonical(canonical),
  });
};

export const makeRollbackState = (
  policy: WatcherFinalityPolicy,
  value: Readonly<{
    bootstrapStore: WatcherDurableStore;
    bootstrapFinalityState: WatcherFinalityState;
    epoch: string;
    epochCheckpoint: WatcherRollbackEpochCheckpoint | null;
    transitions: readonly WatcherRollbackTransition[];
    storeDigest: string;
    transitionCount: string;
    currentFinalityStateDigest: string | null;
    lastPreviousFinalityStateDigest: string | null;
    lastConsistencyDigest: string | null;
    lastFinalityResultDigest: string | null;
    lastInstructionDigest: string | null;
    transitionLineageDigest: string;
    incident: WatcherRollbackIncident | null;
  }>,
): WatcherRollbackState => {
  const canonical = {
    schemaVersion: WATCHER_ROLLBACK_STATE_SCHEMA_VERSION,
    policyDigest: policy.policyDigest,
    network: policy.network,
    blueprintHash: policy.blueprintHash,
    deploymentMarker: policy.deploymentMarker,
    bootstrapStore: value.bootstrapStore,
    bootstrapFinalityState: value.bootstrapFinalityState,
    epoch: value.epoch,
    epochCheckpoint: value.epochCheckpoint,
    transitions: Object.freeze([...value.transitions]),
    storeDigest: value.storeDigest,
    transitionCount: value.transitionCount,
    currentFinalityStateDigest: value.currentFinalityStateDigest,
    lastPreviousFinalityStateDigest: value.lastPreviousFinalityStateDigest,
    lastConsistencyDigest: value.lastConsistencyDigest,
    lastFinalityResultDigest: value.lastFinalityResultDigest,
    lastInstructionDigest: value.lastInstructionDigest,
    transitionLineageDigest: value.transitionLineageDigest,
    incident: value.incident,
  };
  return Object.freeze({
    ...canonical,
    stateDigest: sha256Canonical(canonical),
  });
};

const genesisLineageDigest = (policy: WatcherFinalityPolicy): string =>
  sha256Canonical({
    schemaVersion: WATCHER_ROLLBACK_STATE_SCHEMA_VERSION,
    kind: "genesis",
    policyDigest: policy.policyDigest,
    network: policy.network,
    blueprintHash: policy.blueprintHash,
    deploymentMarker: policy.deploymentMarker,
  });

export const initialRollbackState = (
  policy: WatcherFinalityPolicy,
  bootstrapStore: WatcherDurableStore,
  bootstrapFinalityState: WatcherFinalityState,
): WatcherRollbackState =>
  makeRollbackState(policy, {
    bootstrapStore,
    bootstrapFinalityState,
    epoch: "0",
    epochCheckpoint: null,
    transitions: [],
    storeDigest: storeDigest(bootstrapStore),
    transitionCount: "0",
    currentFinalityStateDigest: null,
    lastPreviousFinalityStateDigest: null,
    lastConsistencyDigest: null,
    lastFinalityResultDigest: null,
    lastInstructionDigest: null,
    transitionLineageDigest: genesisLineageDigest(policy),
    incident: null,
  });

export const storeDigest = (store: WatcherDurableStore): string =>
  watcherDurableStoreBytesSha256(encodeWatcherDurableStore(store));
