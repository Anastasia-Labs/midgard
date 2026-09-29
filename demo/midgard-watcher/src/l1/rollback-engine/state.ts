import {
  encodeWatcherDurableStore,
  makeWatcherDurableStore,
  parseWatcherDurableStore,
  type WatcherDurableRecords,
  type WatcherDurableStore,
  watcherDurableStoreBytesSha256,
  watcherSameCanonicalJson,
} from "../../storage/durable-store.js";
import {
  evaluateWatcherFinality,
  parseWatcherFinalityPolicy,
  parseWatcherFinalityState,
  WATCHER_FINALITY_RESULT_SCHEMA_VERSION,
  type WatcherFinalityBoundObservation,
  watcherFinalityConfiguredSource,
  type WatcherFinalityIncident,
  type WatcherFinalityPolicy,
  type WatcherFinalityResult,
  type WatcherFinalityState,
} from ".././finality-engine.js";
import {
  encodeWatcherNormalizedL1Block,
  type WatcherL1TransportAttestationContext,
  type WatcherNormalizedL1Block,
} from ".././l1-adapter.js";
import {
  evaluateWatcherMultiProviderConsistency,
  type WatcherMultiProviderConsistency,
} from ".././multi-provider-consistency.js";
import {
  emptyRemovedRecords,
  exactArray,
  exactPlainRecord,
  exactStringArray,
  exactUnrestrictedStringArray,
  makeResult,
  marker,
  parseInstruction,
  reject,
  sameBinding,
  sameMarker,
  sameStrings,
  stateBindingFailure,
} from "./records.js";
import {
  CANONICAL_NATURAL,
  HEX_32,
  NETWORKS,
  type ParsedFinalityTransition,
  rollbackDurableAuthorityRuntime,
  sha256Canonical,
  WATCHER_ROLLBACK_BOUNDS,
  WATCHER_ROLLBACK_EPOCH_CHECKPOINT_SCHEMA_VERSION,
  WATCHER_ROLLBACK_INCIDENT_SCHEMA_VERSION,
  WATCHER_ROLLBACK_STATE_SCHEMA_VERSION,
  WATCHER_ROLLBACK_TRANSITION_SCHEMA_VERSION,
  type WatcherRollbackDurableAuthority,
  type WatcherRollbackEpochCheckpoint,
  type WatcherRollbackIncident,
  type WatcherRollbackReasonCode,
  type WatcherRollbackRemovedRecords,
  type WatcherRollbackResult,
  type WatcherRollbackState,
  type WatcherRollbackStateVerificationContext,
  type WatcherRollbackTransition,
} from "./types.js";

const parseFinalityTransition = (
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

const makeIncident = (
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

const makeRollbackState = (
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

const initialRollbackState = (
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

const rootBootstrapStateDigest = (
  policy: WatcherFinalityPolicy,
  state: WatcherRollbackState,
): string =>
  state.epochCheckpoint?.rootBootstrapStateDigest ??
  initialRollbackState(
    policy,
    state.bootstrapStore,
    state.bootstrapFinalityState,
  ).stateDigest;

const makeEpochCheckpoint = (
  policy: WatcherFinalityPolicy,
  prior: WatcherRollbackState,
  checkpointStore: WatcherDurableStore,
  checkpointFinalityState: WatcherFinalityState,
  recovery: Readonly<{
    stateDigest: string;
    lifecycleDigest: string;
  }> | null = null,
  operation: WatcherRollbackEpochCheckpoint["operation"] = recovery === null
    ? "compaction"
    : "recovery",
): WatcherRollbackEpochCheckpoint => {
  const canonical = {
    schemaVersion: WATCHER_ROLLBACK_EPOCH_CHECKPOINT_SCHEMA_VERSION,
    operation,
    epoch: (BigInt(prior.epoch) + 1n).toString(),
    rootBootstrapStateDigest: rootBootstrapStateDigest(policy, prior),
    priorCheckpointDigest: prior.epochCheckpoint?.checkpointDigest ?? null,
    priorTerminalStateDigest: prior.stateDigest,
    priorTerminalTransitionCount: prior.transitionCount,
    priorTerminalTransitionLineageDigest: prior.transitionLineageDigest,
    priorTerminalStoreDigest: prior.storeDigest,
    priorTerminalFinalityStateDigest:
      prior.currentFinalityStateDigest ??
      prior.bootstrapFinalityState.stateDigest,
    priorTerminalIncidentDigest: prior.incident?.incidentDigest ?? null,
    recoveryStateDigest: recovery?.stateDigest ?? null,
    recoveryLifecycleDigest: recovery?.lifecycleDigest ?? null,
    checkpointStoreDigest: storeDigest(checkpointStore),
    checkpointFinalityStateDigest: checkpointFinalityState.stateDigest,
  };
  return Object.freeze({
    ...canonical,
    checkpointDigest: sha256Canonical(canonical),
  });
};

export const makeEpochBootstrapState = (
  policy: WatcherFinalityPolicy,
  prior: WatcherRollbackState,
  checkpointStore: WatcherDurableStore,
  checkpointFinalityState: WatcherFinalityState,
  recovery: Readonly<{
    stateDigest: string;
    lifecycleDigest: string;
  }> | null = null,
  operation: WatcherRollbackEpochCheckpoint["operation"] = recovery === null
    ? "compaction"
    : "recovery",
): WatcherRollbackState => {
  const epochCheckpoint = makeEpochCheckpoint(
    policy,
    prior,
    checkpointStore,
    checkpointFinalityState,
    recovery,
    operation,
  );
  return makeRollbackState(policy, {
    bootstrapStore: checkpointStore,
    bootstrapFinalityState: checkpointFinalityState,
    epoch: epochCheckpoint.epoch,
    epochCheckpoint,
    transitions: [],
    storeDigest: epochCheckpoint.checkpointStoreDigest,
    transitionCount: prior.transitionCount,
    currentFinalityStateDigest: epochCheckpoint.checkpointFinalityStateDigest,
    lastPreviousFinalityStateDigest: null,
    lastConsistencyDigest: null,
    lastFinalityResultDigest: null,
    lastInstructionDigest: null,
    transitionLineageDigest:
      epochCheckpoint.priorTerminalTransitionLineageDigest,
    incident: null,
  });
};

const makeTransitionRecord = (
  transition: ParsedFinalityTransition,
): WatcherRollbackTransition => {
  const canonical = {
    schemaVersion: WATCHER_ROLLBACK_TRANSITION_SCHEMA_VERSION,
    previousFinalityState: transition.previous,
    consistency: transition.consistency,
    finalityResult: transition.finalityResult,
  };
  return Object.freeze({
    ...canonical,
    transitionDigest: sha256Canonical(canonical),
  });
};

const advanceRollbackState = (
  policy: WatcherFinalityPolicy,
  previous: WatcherRollbackState,
  activeBootstrapState: WatcherRollbackState,
  transition: ParsedFinalityTransition,
  sourceStore: WatcherDurableStore,
  durableStoreDigest: string,
  incident: WatcherRollbackIncident | null,
): Readonly<{
  state: WatcherRollbackState;
  bootstrapState: WatcherRollbackState;
}> => {
  const epochState =
    previous.transitions.length >= WATCHER_ROLLBACK_BOUNDS.transitionHistory
      ? makeEpochBootstrapState(
          policy,
          previous,
          sourceStore,
          transition.previous,
        )
      : previous;
  const transitionCount = (BigInt(epochState.transitionCount) + 1n).toString();
  const lastInstructionDigest =
    transition.kind === "rewind"
      ? transition.instruction.instructionDigest
      : null;
  const transitionLineageDigest = sha256Canonical({
    priorLineageDigest: epochState.transitionLineageDigest,
    transitionCount,
    previousFinalityStateDigest: transition.previous.stateDigest,
    consistencyDigest: transition.consistency.consistencyDigest,
    finalityResultDigest: transition.finalityResult.resultDigest,
    currentFinalityStateDigest: transition.next.stateDigest,
    instructionDigest: lastInstructionDigest,
    storeDigest: durableStoreDigest,
  });
  const transitions = Object.freeze([
    ...epochState.transitions,
    makeTransitionRecord(transition),
  ]);
  const state = makeRollbackState(policy, {
    bootstrapStore: epochState.bootstrapStore,
    bootstrapFinalityState: epochState.bootstrapFinalityState,
    epoch: epochState.epoch,
    epochCheckpoint: epochState.epochCheckpoint,
    transitions,
    storeDigest: durableStoreDigest,
    transitionCount,
    currentFinalityStateDigest: transition.next.stateDigest,
    lastPreviousFinalityStateDigest: transition.previous.stateDigest,
    lastConsistencyDigest: transition.consistency.consistencyDigest,
    lastFinalityResultDigest: transition.finalityResult.resultDigest,
    lastInstructionDigest,
    transitionLineageDigest,
    incident,
  });
  return Object.freeze({
    state,
    bootstrapState: epochState === previous ? activeBootstrapState : epochState,
  });
};

const parseBoundObservation = (
  value: unknown,
): WatcherFinalityBoundObservation | null => {
  const record = exactPlainRecord(value, [
    "pointDigest",
    "blockHash",
    "slot",
    "blockNo",
    "blockContentDigest",
    "firstSeenConsistencyDigest",
    "lastSeenConsistencyDigest",
    "firstSeenDepth",
    "currentDepth",
    "visibilityCount",
  ]);
  if (
    record === null ||
    [
      record.pointDigest,
      record.blockHash,
      record.blockContentDigest,
      record.firstSeenConsistencyDigest,
      record.lastSeenConsistencyDigest,
    ].some((member) => typeof member !== "string" || !HEX_32.test(member)) ||
    [
      record.slot,
      record.blockNo,
      record.firstSeenDepth,
      record.currentDepth,
    ].some(
      (member) => typeof member !== "string" || !CANONICAL_NATURAL.test(member),
    ) ||
    typeof record.visibilityCount !== "string" ||
    !/^[1-9][0-9]*$/u.test(record.visibilityCount)
  ) {
    return null;
  }
  return Object.freeze({
    pointDigest: record.pointDigest as string,
    blockHash: record.blockHash as string,
    slot: record.slot as string,
    blockNo: record.blockNo as string,
    blockContentDigest: record.blockContentDigest as string,
    firstSeenConsistencyDigest: record.firstSeenConsistencyDigest as string,
    lastSeenConsistencyDigest: record.lastSeenConsistencyDigest as string,
    firstSeenDepth: record.firstSeenDepth as string,
    currentDepth: record.currentDepth as string,
    visibilityCount: record.visibilityCount,
  });
};

const parseRollbackIncident = (
  value: unknown,
): WatcherRollbackIncident | null => {
  const record = exactPlainRecord(value, [
    "schemaVersion",
    "reasonCode",
    "policyDigest",
    "blueprintHash",
    "deploymentMarker",
    "finalityStateDigest",
    "finalityIncidentDigest",
    "triggerConsistencyDigest",
    "finalizedBinding",
    "sourceStoreDigest",
    "nextStoreDigest",
    "transitionCount",
    "previousFinalityStateDigest",
    "consistencyDigest",
    "finalityResultDigest",
    "incidentDigest",
  ]);
  if (
    record === null ||
    record.schemaVersion !== WATCHER_ROLLBACK_INCIDENT_SCHEMA_VERSION ||
    record.reasonCode !== "post_finality_point_changed"
  ) {
    return null;
  }
  const deploymentMarker = marker(record.deploymentMarker);
  const finalizedBinding = parseBoundObservation(record.finalizedBinding);
  if (
    deploymentMarker === null ||
    finalizedBinding === null ||
    [
      record.policyDigest,
      record.blueprintHash,
      record.finalityStateDigest,
      record.finalityIncidentDigest,
      record.sourceStoreDigest,
      record.nextStoreDigest,
      record.previousFinalityStateDigest,
      record.consistencyDigest,
      record.finalityResultDigest,
      record.incidentDigest,
    ].some((member) => typeof member !== "string" || !HEX_32.test(member)) ||
    typeof record.transitionCount !== "string" ||
    !/^[1-9][0-9]*$/u.test(record.transitionCount) ||
    !(
      record.triggerConsistencyDigest === null ||
      (typeof record.triggerConsistencyDigest === "string" &&
        HEX_32.test(record.triggerConsistencyDigest))
    )
  ) {
    return null;
  }
  const canonical = {
    schemaVersion: WATCHER_ROLLBACK_INCIDENT_SCHEMA_VERSION,
    reasonCode: record.reasonCode as WatcherFinalityIncident["reasonCode"],
    policyDigest: record.policyDigest as string,
    blueprintHash: record.blueprintHash as string,
    deploymentMarker,
    finalityStateDigest: record.finalityStateDigest as string,
    finalityIncidentDigest: record.finalityIncidentDigest as string,
    triggerConsistencyDigest: record.triggerConsistencyDigest as string | null,
    finalizedBinding,
    sourceStoreDigest: record.sourceStoreDigest as string,
    nextStoreDigest: record.nextStoreDigest as string,
    transitionCount: record.transitionCount,
    previousFinalityStateDigest: record.previousFinalityStateDigest as string,
    consistencyDigest: record.consistencyDigest as string,
    finalityResultDigest: record.finalityResultDigest as string,
  };
  if (
    canonical.triggerConsistencyDigest !== canonical.consistencyDigest ||
    canonical.finalityIncidentDigest !==
      sha256Canonical({
        reasonCode: canonical.reasonCode,
        triggerConsistencyDigest: canonical.triggerConsistencyDigest,
        priorStateDigest: canonical.previousFinalityStateDigest,
      }) ||
    sha256Canonical(canonical) !== record.incidentDigest
  ) {
    return null;
  }
  return Object.freeze({
    ...canonical,
    incidentDigest: record.incidentDigest as string,
  });
};

const parseTransitionRecord = (
  value: unknown,
): WatcherRollbackTransition | null => {
  const record = exactPlainRecord(value, [
    "schemaVersion",
    "previousFinalityState",
    "consistency",
    "finalityResult",
    "transitionDigest",
  ]);
  if (
    record === null ||
    record.schemaVersion !== WATCHER_ROLLBACK_TRANSITION_SCHEMA_VERSION ||
    typeof record.transitionDigest !== "string" ||
    !HEX_32.test(record.transitionDigest)
  ) {
    return null;
  }
  const previousFinalityState = parseWatcherFinalityState(
    record.previousFinalityState,
  );
  const consistency = exactPlainRecord(record.consistency, [
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
  const finalityResult = exactPlainRecord(record.finalityResult, [
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
    previousFinalityState === null ||
    consistency === null ||
    finalityResult === null
  ) {
    return null;
  }
  const canonical = {
    schemaVersion: WATCHER_ROLLBACK_TRANSITION_SCHEMA_VERSION,
    previousFinalityState,
    consistency: consistency as WatcherMultiProviderConsistency,
    finalityResult: finalityResult as unknown as WatcherFinalityResult,
  };
  if (sha256Canonical(canonical) !== record.transitionDigest) {
    return null;
  }
  return Object.freeze({
    ...canonical,
    transitionDigest: record.transitionDigest,
  });
};

const parseEpochCheckpoint = (
  value: unknown,
): WatcherRollbackEpochCheckpoint | null => {
  const record = exactPlainRecord(value, [
    "schemaVersion",
    "operation",
    "epoch",
    "rootBootstrapStateDigest",
    "priorCheckpointDigest",
    "priorTerminalStateDigest",
    "priorTerminalTransitionCount",
    "priorTerminalTransitionLineageDigest",
    "priorTerminalStoreDigest",
    "priorTerminalFinalityStateDigest",
    "priorTerminalIncidentDigest",
    "recoveryStateDigest",
    "recoveryLifecycleDigest",
    "checkpointStoreDigest",
    "checkpointFinalityStateDigest",
    "checkpointDigest",
  ]);
  if (
    record === null ||
    record.schemaVersion !== WATCHER_ROLLBACK_EPOCH_CHECKPOINT_SCHEMA_VERSION ||
    typeof record.operation !== "string" ||
    !["compaction", "observation", "canonical_progress", "recovery"].includes(
      record.operation,
    ) ||
    (record.operation === "recovery") !==
      (record.recoveryStateDigest !== null) ||
    typeof record.epoch !== "string" ||
    !/^[1-9][0-9]*$/u.test(record.epoch) ||
    typeof record.priorTerminalTransitionCount !== "string" ||
    !CANONICAL_NATURAL.test(record.priorTerminalTransitionCount) ||
    ![
      record.rootBootstrapStateDigest,
      record.priorTerminalStateDigest,
      record.priorTerminalTransitionLineageDigest,
      record.priorTerminalStoreDigest,
      record.priorTerminalFinalityStateDigest,
      record.checkpointStoreDigest,
      record.checkpointFinalityStateDigest,
      record.checkpointDigest,
    ].every((digest) => typeof digest === "string" && HEX_32.test(digest)) ||
    ![
      record.priorCheckpointDigest,
      record.priorTerminalIncidentDigest,
      record.recoveryStateDigest,
      record.recoveryLifecycleDigest,
    ].every(
      (digest) =>
        digest === null || (typeof digest === "string" && HEX_32.test(digest)),
    ) ||
    (record.recoveryStateDigest === null) !==
      (record.recoveryLifecycleDigest === null) ||
    (record.priorCheckpointDigest === null) !== (record.epoch === "1") ||
    (record.recoveryStateDigest === null
      ? record.priorTerminalIncidentDigest !== null ||
        (record.operation === "compaction" &&
          record.checkpointStoreDigest !== record.priorTerminalStoreDigest) ||
        (record.operation !== "canonical_progress" &&
          record.checkpointFinalityStateDigest !==
            record.priorTerminalFinalityStateDigest)
      : record.priorTerminalIncidentDigest === null)
  ) {
    return null;
  }
  const canonical = {
    schemaVersion: WATCHER_ROLLBACK_EPOCH_CHECKPOINT_SCHEMA_VERSION,
    operation: record.operation as WatcherRollbackEpochCheckpoint["operation"],
    epoch: record.epoch,
    rootBootstrapStateDigest: record.rootBootstrapStateDigest as string,
    priorCheckpointDigest: record.priorCheckpointDigest as string | null,
    priorTerminalStateDigest: record.priorTerminalStateDigest as string,
    priorTerminalTransitionCount: record.priorTerminalTransitionCount,
    priorTerminalTransitionLineageDigest:
      record.priorTerminalTransitionLineageDigest as string,
    priorTerminalStoreDigest: record.priorTerminalStoreDigest as string,
    priorTerminalFinalityStateDigest:
      record.priorTerminalFinalityStateDigest as string,
    priorTerminalIncidentDigest: record.priorTerminalIncidentDigest as
      | string
      | null,
    recoveryStateDigest: record.recoveryStateDigest as string | null,
    recoveryLifecycleDigest: record.recoveryLifecycleDigest as string | null,
    checkpointStoreDigest: record.checkpointStoreDigest as string,
    checkpointFinalityStateDigest:
      record.checkpointFinalityStateDigest as string,
  };
  return sha256Canonical(canonical) === record.checkpointDigest
    ? Object.freeze({
        ...canonical,
        checkpointDigest: record.checkpointDigest as string,
      })
    : null;
};

export const decodeWatcherRollbackStateStructural = (
  value: unknown,
  parseStore = parseWatcherDurableStore,
): WatcherRollbackState | null => {
  try {
    const record = exactPlainRecord(value, [
      "schemaVersion",
      "policyDigest",
      "network",
      "blueprintHash",
      "deploymentMarker",
      "bootstrapStore",
      "bootstrapFinalityState",
      "epoch",
      "epochCheckpoint",
      "transitions",
      "storeDigest",
      "transitionCount",
      "currentFinalityStateDigest",
      "lastPreviousFinalityStateDigest",
      "lastConsistencyDigest",
      "lastFinalityResultDigest",
      "lastInstructionDigest",
      "transitionLineageDigest",
      "incident",
      "stateDigest",
    ]);
    if (
      record === null ||
      record.schemaVersion !== WATCHER_ROLLBACK_STATE_SCHEMA_VERSION ||
      typeof record.policyDigest !== "string" ||
      !HEX_32.test(record.policyDigest) ||
      !NETWORKS.includes(record.network as (typeof NETWORKS)[number]) ||
      typeof record.blueprintHash !== "string" ||
      !HEX_32.test(record.blueprintHash) ||
      typeof record.storeDigest !== "string" ||
      !HEX_32.test(record.storeDigest) ||
      typeof record.epoch !== "string" ||
      !CANONICAL_NATURAL.test(record.epoch) ||
      typeof record.transitionCount !== "string" ||
      !CANONICAL_NATURAL.test(record.transitionCount) ||
      ![
        record.currentFinalityStateDigest,
        record.lastPreviousFinalityStateDigest,
        record.lastConsistencyDigest,
        record.lastFinalityResultDigest,
        record.lastInstructionDigest,
      ].every(
        (member) =>
          member === null ||
          (typeof member === "string" && HEX_32.test(member)),
      ) ||
      typeof record.transitionLineageDigest !== "string" ||
      !HEX_32.test(record.transitionLineageDigest) ||
      typeof record.stateDigest !== "string" ||
      !HEX_32.test(record.stateDigest)
    ) {
      return null;
    }
    const deploymentMarker = marker(record.deploymentMarker);
    let bootstrapStore: WatcherDurableStore;
    try {
      bootstrapStore = parseStore(record.bootstrapStore);
    } catch {
      return null;
    }
    const bootstrapFinalityState = parseWatcherFinalityState(
      record.bootstrapFinalityState,
    );
    const epochCheckpoint =
      record.epochCheckpoint === null
        ? null
        : parseEpochCheckpoint(record.epochCheckpoint);
    const transitionInputs = exactArray(record.transitions);
    if (
      bootstrapFinalityState === null ||
      transitionInputs === null ||
      transitionInputs.length > WATCHER_ROLLBACK_BOUNDS.transitionHistory
    ) {
      return null;
    }
    const transitions = transitionInputs.map(parseTransitionRecord);
    const parsedIncident =
      record.incident === null ? null : parseRollbackIncident(record.incident);
    if (
      deploymentMarker === null ||
      (record.epochCheckpoint !== null && epochCheckpoint === null) ||
      transitions.some((transition) => transition === null) ||
      !sameMarker(bootstrapStore.deploymentMarker, deploymentMarker) ||
      (record.incident !== null && parsedIncident === null)
    ) {
      return null;
    }
    const canonical = {
      schemaVersion: WATCHER_ROLLBACK_STATE_SCHEMA_VERSION,
      policyDigest: record.policyDigest,
      network: record.network as (typeof NETWORKS)[number],
      blueprintHash: record.blueprintHash,
      deploymentMarker,
      bootstrapStore,
      bootstrapFinalityState,
      epoch: record.epoch,
      epochCheckpoint,
      transitions: Object.freeze(
        transitions as readonly WatcherRollbackTransition[],
      ),
      storeDigest: record.storeDigest,
      transitionCount: record.transitionCount,
      currentFinalityStateDigest: record.currentFinalityStateDigest as
        | string
        | null,
      lastPreviousFinalityStateDigest:
        record.lastPreviousFinalityStateDigest as string | null,
      lastConsistencyDigest: record.lastConsistencyDigest as string | null,
      lastFinalityResultDigest: record.lastFinalityResultDigest as
        | string
        | null,
      lastInstructionDigest: record.lastInstructionDigest as string | null,
      transitionLineageDigest: record.transitionLineageDigest,
      incident: parsedIncident,
    };
    const count = BigInt(canonical.transitionCount);
    const epochTransitionCount = BigInt(canonical.transitions.length);
    const priorTransitionCount = BigInt(
      canonical.epochCheckpoint?.priorTerminalTransitionCount ?? "0",
    );
    if (
      count !== priorTransitionCount + epochTransitionCount ||
      BigInt(canonical.epoch) !==
        BigInt(canonical.epochCheckpoint?.epoch ?? "0") ||
      (canonical.epochCheckpoint === null) !== (canonical.epoch === "0") ||
      (canonical.epochCheckpoint !== null &&
        (canonical.epochCheckpoint.checkpointStoreDigest !==
          storeDigest(canonical.bootstrapStore) ||
          canonical.epochCheckpoint.checkpointFinalityStateDigest !==
            canonical.bootstrapFinalityState.stateDigest))
    ) {
      return null;
    }
    const initialShape =
      count === 0n &&
      canonical.epoch === "0" &&
      canonical.currentFinalityStateDigest === null &&
      canonical.lastPreviousFinalityStateDigest === null &&
      canonical.lastConsistencyDigest === null &&
      canonical.lastFinalityResultDigest === null &&
      canonical.lastInstructionDigest === null &&
      canonical.incident === null &&
      canonical.transitionLineageDigest ===
        sha256Canonical({
          schemaVersion: WATCHER_ROLLBACK_STATE_SCHEMA_VERSION,
          kind: "genesis",
          policyDigest: canonical.policyDigest,
          network: canonical.network,
          blueprintHash: canonical.blueprintHash,
          deploymentMarker: canonical.deploymentMarker,
        });
    const checkpointShape =
      canonical.epochCheckpoint !== null &&
      epochTransitionCount === 0n &&
      canonical.currentFinalityStateDigest ===
        canonical.epochCheckpoint.checkpointFinalityStateDigest &&
      canonical.lastPreviousFinalityStateDigest === null &&
      canonical.lastConsistencyDigest === null &&
      canonical.lastFinalityResultDigest === null &&
      canonical.lastInstructionDigest === null &&
      canonical.incident === null &&
      canonical.transitionLineageDigest ===
        canonical.epochCheckpoint.priorTerminalTransitionLineageDigest;
    const transitionedShape =
      epochTransitionCount > 0n &&
      canonical.currentFinalityStateDigest !== null &&
      canonical.lastPreviousFinalityStateDigest !== null &&
      canonical.lastConsistencyDigest !== null &&
      canonical.lastFinalityResultDigest !== null &&
      ((canonical.incident === null &&
        canonical.lastInstructionDigest !== null) ||
        (canonical.incident !== null &&
          canonical.lastInstructionDigest === null));
    if (!initialShape && !checkpointShape && !transitionedShape) {
      return null;
    }
    if (
      parsedIncident !== null &&
      (parsedIncident.policyDigest !== canonical.policyDigest ||
        parsedIncident.blueprintHash !== canonical.blueprintHash ||
        !sameMarker(
          parsedIncident.deploymentMarker,
          canonical.deploymentMarker,
        ) ||
        parsedIncident.nextStoreDigest !== canonical.storeDigest ||
        parsedIncident.transitionCount !== canonical.transitionCount ||
        parsedIncident.previousFinalityStateDigest !==
          canonical.lastPreviousFinalityStateDigest ||
        parsedIncident.consistencyDigest !== canonical.lastConsistencyDigest ||
        parsedIncident.finalityResultDigest !==
          canonical.lastFinalityResultDigest ||
        parsedIncident.finalityStateDigest !==
          canonical.currentFinalityStateDigest)
    ) {
      return null;
    }
    if (sha256Canonical(canonical) !== record.stateDigest) {
      return null;
    }
    const state = Object.freeze({
      ...canonical,
      stateDigest: record.stateDigest,
    }) as WatcherRollbackState;
    return state;
  } catch {
    return null;
  }
};

export const parseRemovedRecords = (
  value: unknown,
): WatcherRollbackRemovedRecords | null => {
  const keys = [
    "l1ObservationIds",
    "chainPointIds",
    "protocolUtxoOutRefs",
    "daProofInputIds",
    "reconstructedBlockHashes",
    "decisionBlockHashes",
    "faultIds",
    "submissionIds",
    "confirmationIds",
    "retryIds",
    "deadlineIds",
    "correctionResultIds",
  ] as const;
  const record = exactPlainRecord(value, keys);
  if (record === null) {
    return null;
  }
  const parsed = {} as Record<(typeof keys)[number], readonly string[]>;
  for (const key of keys) {
    const array = exactUnrestrictedStringArray(record[key]);
    if (
      array === null ||
      array.some(
        (member) =>
          typeof member !== "string" ||
          (key === "protocolUtxoOutRefs"
            ? !/^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u.test(member)
            : !HEX_32.test(member)),
      )
    ) {
      return null;
    }
    const strings = array;
    if (
      new Set(strings).size !== strings.length ||
      !sameStrings(strings, [...strings].sort())
    ) {
      return null;
    }
    parsed[key] = Object.freeze([...strings]);
  }
  return Object.freeze(parsed) as WatcherRollbackRemovedRecords;
};

export const removedRecordCount = (
  removed: WatcherRollbackRemovedRecords,
): number =>
  Object.values(removed).reduce((total, values) => total + values.length, 0);

export const sorted = (values: Iterable<string>): readonly string[] =>
  Object.freeze([...values].sort());

export type PersistedConsistencyEvidence = Readonly<{
  observationIds: ReadonlySet<string>;
  chainPointIds: ReadonlySet<string>;
  observations: readonly WatcherNormalizedL1Block[];
  consistency: WatcherMultiProviderConsistency;
}>;

export type PersistedObservationIndexEntry = Readonly<{
  durable: WatcherDurableStore["l1Observations"][number];
  point: WatcherDurableStore["chainPoints"][number] | null;
  observation: WatcherNormalizedL1Block;
}>;

export type PersistedObservationIndex = ReadonlyMap<
  string,
  PersistedObservationIndexEntry | null
>;

const decodePersistedObservation = (
  cborHex: string,
): WatcherNormalizedL1Block | null => {
  try {
    const bytes = Buffer.from(cborHex, "hex");
    const text = new TextDecoder("utf-8", { fatal: true }).decode(bytes);
    const decoded = JSON.parse(text) as unknown;
    const root = exactPlainRecord(decoded, [
      "schemaVersion",
      "network",
      "provider",
      "chainPoint",
      "transactions",
      "blockContentDigest",
      "observationDigest",
    ]);
    if (
      root === null ||
      typeof root.observationDigest !== "string" ||
      !HEX_32.test(root.observationDigest)
    ) {
      return null;
    }
    const observation = decoded as WatcherNormalizedL1Block;
    if (!bytes.equals(encodeWatcherNormalizedL1Block(observation))) {
      return null;
    }
    return observation;
  } catch {
    return null;
  }
};

export const indexPersistedObservations = (
  store: WatcherDurableStore,
): PersistedObservationIndex => {
  const points = new Map(
    store.chainPoints.map((point) => [point.chainPointId, point] as const),
  );
  const index = new Map<string, PersistedObservationIndexEntry | null>();
  for (const durable of store.l1Observations) {
    const observation = decodePersistedObservation(durable.payload.cborHex);
    if (observation === null) {
      continue;
    }
    const digest = observation.observationDigest;
    if (index.has(digest)) {
      index.set(digest, null);
      continue;
    }
    index.set(
      digest,
      Object.freeze({
        durable,
        point: points.get(observation.chainPoint.chainPointId) ?? null,
        observation,
      }),
    );
  }
  return index;
};

export const AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE = Symbol(
  "authenticated-rollback-snapshot-evidence",
);

export const verifyPersistedConsistencyEvidence = (
  policy: WatcherFinalityPolicy,
  store: WatcherDurableStore,
  consistency: WatcherMultiProviderConsistency,
  transportAttestationsInput: unknown,
  persistedIndex: PersistedObservationIndex = indexPersistedObservations(store),
  authenticatedSnapshotEvidence:
    | typeof AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE
    | null = null,
): PersistedConsistencyEvidence | null => {
  const structuralVerification = evaluateWatcherFinality(
    policy,
    null,
    consistency,
  );
  if (
    structuralVerification.action !== "observe_pending" ||
    structuralVerification.state?.pending === null ||
    structuralVerification.state?.pending?.lastSeenConsistencyDigest !==
      consistency.consistencyDigest
  ) {
    return null;
  }
  const evidenceDigests = consistency.observationEvidenceDigests;
  if (
    consistency.sourceMode !== policy.sourceMode ||
    consistency.configuredNetwork !== policy.network ||
    consistency.authorityNodeId !== policy.authorityNodeId ||
    consistency.authorityGenesisIdentitySha256 !==
      policy.authorityGenesisIdentitySha256 ||
    consistency.configuredSourceDigest !==
      sha256Canonical(watcherFinalityConfiguredSource(policy)) ||
    consistency.rejectedObservationCount !== 0 ||
    evidenceDigests.length === 0 ||
    consistency.observationCount !== evidenceDigests.length
  ) {
    return null;
  }
  const decoded: WatcherNormalizedL1Block[] = [];
  const observationIds = new Set<string>();
  const chainPointIds = new Set<string>();
  for (const evidenceDigest of evidenceDigests) {
    const indexed = persistedIndex.get(evidenceDigest);
    if (indexed === undefined || indexed === null) {
      return null;
    }
    const { durable, observation, point } = indexed;
    if (
      durable.providerId !== observation.provider.providerId ||
      durable.chainPointId !== observation.chainPoint.chainPointId ||
      point === null ||
      point.providerId !== observation.provider.providerId ||
      point.blockHash !== observation.chainPoint.blockHash ||
      point.slot !== observation.chainPoint.slot ||
      point.blockNo !== observation.chainPoint.blockNo ||
      point.depth !== observation.chainPoint.depth
    ) {
      return null;
    }
    observationIds.add(durable.observationId);
    chainPointIds.add(durable.chainPointId);
    decoded.push(observation);
  }
  if (
    decoded.length !== evidenceDigests.length ||
    !sameStrings(
      sorted(decoded.map(({ observationDigest }) => observationDigest)),
      evidenceDigests,
    )
  ) {
    return null;
  }
  const recomputed =
    authenticatedSnapshotEvidence === AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE
      ? consistency
      : evaluateWatcherMultiProviderConsistency(
          watcherFinalityConfiguredSource(policy),
          decoded,
          transportAttestationsInput as readonly WatcherL1TransportAttestationContext[],
        );
  return watcherSameCanonicalJson(recomputed, consistency)
    ? Object.freeze({
        observationIds,
        chainPointIds,
        observations: Object.freeze(decoded),
        consistency: recomputed,
      })
    : null;
};

const verifyPersistedReplacementEvidence = (
  policy: WatcherFinalityPolicy,
  store: WatcherDurableStore,
  transition: Extract<ParsedFinalityTransition, { readonly kind: "rewind" }>,
  transportAttestationsInput: unknown,
  authenticatedSnapshotEvidence:
    | typeof AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE
    | null = null,
): PersistedConsistencyEvidence | null => {
  const evidence = verifyPersistedConsistencyEvidence(
    policy,
    store,
    transition.consistency,
    transportAttestationsInput,
    undefined,
    authenticatedSnapshotEvidence,
  );
  const agreement = transition.consistency.agreement;
  const replacement = transition.next
    .pending as WatcherFinalityBoundObservation;
  return evidence !== null &&
    transition.consistency.status === "agreed" &&
    transition.consistency.protocolDecision === "allowed" &&
    agreement !== null &&
    agreement.pointDigest === replacement.pointDigest &&
    agreement.blockHash === replacement.blockHash &&
    agreement.slot === replacement.slot &&
    agreement.blockNo === replacement.blockNo &&
    agreement.minimumDepth === replacement.currentDepth &&
    agreement.blockContentDigest === replacement.blockContentDigest
    ? evidence
    : null;
};

export const planRewind = (
  store: WatcherDurableStore,
  transition: Extract<ParsedFinalityTransition, { readonly kind: "rewind" }>,
  replacementEvidence: PersistedConsistencyEvidence,
): Readonly<{
  records: WatcherDurableRecords;
  removed: WatcherRollbackRemovedRecords;
}> => {
  const prior = transition.previous.pending as WatcherFinalityBoundObservation;
  const replacement = transition.next
    .pending as WatcherFinalityBoundObservation;
  const kind = transition.instruction.kind;
  const directInvalidPointIds = new Set<string>();
  const removedPointIds = new Set<string>();
  const pivot =
    BigInt(prior.blockNo) < BigInt(replacement.blockNo)
      ? BigInt(prior.blockNo)
      : BigInt(replacement.blockNo);
  const replacementTip =
    BigInt(replacement.blockNo) + BigInt(replacement.currentDepth);

  for (const point of store.chainPoints) {
    const isReplacement =
      point.blockHash === replacement.blockHash &&
      point.slot === replacement.slot &&
      point.blockNo === replacement.blockNo &&
      BigInt(point.depth) <= BigInt(replacement.currentDepth);
    let removePoint = false;
    let invalidateDependents = false;
    if (kind === "pending_point_changed") {
      removePoint =
        BigInt(point.blockNo) >= pivot &&
        !isReplacement &&
        !replacementEvidence.chainPointIds.has(point.chainPointId);
      invalidateDependents = removePoint;
    } else if (kind === "pending_content_changed") {
      invalidateDependents =
        point.blockHash === prior.blockHash &&
        point.slot === prior.slot &&
        point.blockNo === prior.blockNo;
    } else {
      const sameAnchor =
        point.blockHash === replacement.blockHash &&
        point.slot === replacement.slot &&
        point.blockNo === replacement.blockNo;
      removePoint =
        (sameAnchor &&
          BigInt(point.depth) > BigInt(replacement.currentDepth)) ||
        BigInt(point.blockNo) > replacementTip;
      invalidateDependents =
        removePoint && !sameAnchor && BigInt(point.blockNo) > replacementTip;
    }
    if (removePoint) {
      removedPointIds.add(point.chainPointId);
      directInvalidPointIds.add(point.chainPointId);
    }
    if (invalidateDependents) {
      directInvalidPointIds.add(point.chainPointId);
    }
  }

  const removedObservationIds = new Set(
    store.l1Observations
      .filter(
        (entry) =>
          directInvalidPointIds.has(entry.chainPointId) &&
          !replacementEvidence.observationIds.has(entry.observationId),
      )
      .map((entry) => entry.observationId),
  );
  const removedUtxoOutRefs = new Set([
    ...store.protocolUtxos
      .filter((entry) => directInvalidPointIds.has(entry.chainPointId))
      .map((entry) => entry.outRef),
    ...store.spentProtocolUtxos
      .filter((entry) => directInvalidPointIds.has(entry.chainPointId))
      .map((entry) => entry.outRef),
  ]);
  const restoredProtocolUtxos = store.spentProtocolUtxos
    .filter(
      (entry) =>
        !directInvalidPointIds.has(entry.chainPointId) &&
        directInvalidPointIds.has(entry.spentAtChainPointId),
    )
    .map(({ spentAtChainPointId: _spentAtChainPointId, ...entry }) => entry);
  const retainedSpentProtocolUtxos = store.spentProtocolUtxos.filter(
    (entry) =>
      !directInvalidPointIds.has(entry.chainPointId) &&
      !directInvalidPointIds.has(entry.spentAtChainPointId),
  );
  const removedStateBlockHashes = new Set(
    store.reconstructedStates
      .filter((entry) => directInvalidPointIds.has(entry.chainPointId))
      .map((entry) => entry.blockHash),
  );
  const removedDecisionBlockHashes = new Set(
    store.decisions
      .filter((entry) => removedStateBlockHashes.has(entry.blockHash))
      .map((entry) => entry.blockHash),
  );
  const removedFaultIds = new Set(
    store.faults
      .filter((entry) => removedDecisionBlockHashes.has(entry.blockHash))
      .map((entry) => entry.faultId),
  );
  const removedSubmissionIds = new Set(
    store.submissions
      .filter((entry) => removedFaultIds.has(entry.faultId))
      .map((entry) => entry.submissionId),
  );
  const removedConfirmationIds = new Set(
    store.confirmations
      .filter(
        (entry) =>
          removedSubmissionIds.has(entry.submissionId) ||
          directInvalidPointIds.has(entry.chainPointId),
      )
      .map((entry) => entry.confirmationId),
  );
  const removedRetryIds = new Set(
    store.retries
      .filter((entry) => removedSubmissionIds.has(entry.submissionId))
      .map((entry) => entry.retryId),
  );
  const removedDeadlineIds = new Set(
    store.deadlines
      .filter(
        (entry) =>
          (entry.subjectKind === "fault" &&
            removedFaultIds.has(entry.subjectId)) ||
          (entry.subjectKind === "submission" &&
            removedSubmissionIds.has(entry.subjectId)),
      )
      .map((entry) => entry.deadlineId),
  );
  const removedCorrectionResultIds = new Set(
    store.correctionResults
      .filter(
        (entry) =>
          removedFaultIds.has(entry.faultId) ||
          removedConfirmationIds.has(entry.confirmationId),
      )
      .map((entry) => entry.correctionId),
  );
  const retainedStates = store.reconstructedStates.filter(
    (entry) => !removedStateBlockHashes.has(entry.blockHash),
  );
  const retainedInputIds = new Set(
    retainedStates.flatMap((entry) => [...entry.inputIds]),
  );
  const removedStateInputIds = new Set(
    store.reconstructedStates
      .filter((entry) => removedStateBlockHashes.has(entry.blockHash))
      .flatMap((entry) => [...entry.inputIds])
      .filter((inputId) => !retainedInputIds.has(inputId)),
  );
  const removed = Object.freeze({
    l1ObservationIds: sorted(removedObservationIds),
    chainPointIds: sorted(removedPointIds),
    protocolUtxoOutRefs: sorted(removedUtxoOutRefs),
    daProofInputIds: sorted(removedStateInputIds),
    reconstructedBlockHashes: sorted(removedStateBlockHashes),
    decisionBlockHashes: sorted(removedDecisionBlockHashes),
    faultIds: sorted(removedFaultIds),
    submissionIds: sorted(removedSubmissionIds),
    confirmationIds: sorted(removedConfirmationIds),
    retryIds: sorted(removedRetryIds),
    deadlineIds: sorted(removedDeadlineIds),
    correctionResultIds: sorted(removedCorrectionResultIds),
  });
  return Object.freeze({
    removed,
    records: {
      l1Observations: store.l1Observations.filter(
        (entry) => !removedObservationIds.has(entry.observationId),
      ),
      chainPoints: store.chainPoints.filter(
        (entry) => !removedPointIds.has(entry.chainPointId),
      ),
      protocolUtxos: [
        ...store.protocolUtxos.filter(
          (entry) => !removedUtxoOutRefs.has(entry.outRef),
        ),
        ...restoredProtocolUtxos,
      ].sort((left, right) => left.outRef.localeCompare(right.outRef)),
      spentProtocolUtxos: retainedSpentProtocolUtxos,
      daProofInputs: store.daProofInputs.filter(
        (entry) => !removedStateInputIds.has(entry.inputId),
      ),
      reconstructedStates: retainedStates,
      decisions: store.decisions.filter(
        (entry) => !removedDecisionBlockHashes.has(entry.blockHash),
      ),
      faults: store.faults.filter(
        (entry) => !removedFaultIds.has(entry.faultId),
      ),
      submissions: store.submissions.filter(
        (entry) => !removedSubmissionIds.has(entry.submissionId),
      ),
      confirmations: store.confirmations.filter(
        (entry) => !removedConfirmationIds.has(entry.confirmationId),
      ),
      retries: store.retries.filter(
        (entry) => !removedRetryIds.has(entry.retryId),
      ),
      deadlines: store.deadlines.filter(
        (entry) => !removedDeadlineIds.has(entry.deadlineId),
      ),
      correctionResults: store.correctionResults.filter(
        (entry) => !removedCorrectionResultIds.has(entry.correctionId),
      ),
    },
  });
};

export const storeDigest = (store: WatcherDurableStore): string =>
  watcherDurableStoreBytesSha256(encodeWatcherDurableStore(store));

const rollbackStateBindingFailure = (
  policy: WatcherFinalityPolicy,
  state: WatcherRollbackState,
): WatcherRollbackReasonCode | null => {
  if (state.network !== policy.network) {
    return "network_mismatch";
  }
  if (state.blueprintHash !== policy.blueprintHash) {
    return "blueprint_mismatch";
  }
  if (!sameMarker(state.deploymentMarker, policy.deploymentMarker)) {
    return "deployment_mismatch";
  }
  return state.policyDigest === policy.policyDigest ? null : "policy_mismatch";
};

const trustedCheckpointStateDigest = (
  bootstrapState: WatcherRollbackState,
): string | null =>
  bootstrapState.epoch === "0" ? null : bootstrapState.stateDigest;

export const evaluateWatcherRollbackStep = (
  policy: WatcherFinalityPolicy,
  store: WatcherDurableStore,
  rollbackState: WatcherRollbackState,
  rollbackBootstrapState: WatcherRollbackState,
  previousFinalityStateInput: unknown,
  consistencyInput: unknown,
  finalityResultInput: unknown,
  transportAttestationsInput: unknown,
  authenticatedSnapshotEvidence:
    | typeof AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE
    | null = null,
): WatcherRollbackResult => {
  const sourceStoreDigest = storeDigest(store);
  if (rollbackState.incident !== null) {
    return makeResult({
      action: "reject",
      protocolDecision: "quarantined",
      reasonCodes: ["state_quarantined"],
      alertCodes: ["watcher_rollback_quarantined"],
      sourceRevision: store.revision,
      nextRevision: store.revision,
      instructionDigest: null,
      sourceStoreDigest,
      nextStoreDigest: sourceStoreDigest,
      removedRecords: emptyRemovedRecords(),
      nextStore: store,
      rollbackState,
      rollbackBootstrapState,
      trustedCheckpointStateDigest: trustedCheckpointStateDigest(
        rollbackBootstrapState,
      ),
    });
  }

  const transition = parseFinalityTransition(
    policy,
    previousFinalityStateInput,
    consistencyInput,
    finalityResultInput,
  );
  if (typeof transition === "string") {
    const bindingReasons: readonly WatcherRollbackReasonCode[] = [
      "deployment_mismatch",
      "blueprint_mismatch",
      "network_mismatch",
      "policy_mismatch",
    ];
    return reject(
      transition,
      bindingReasons.includes(transition)
        ? "watcher_rollback_configuration_mismatch"
        : "watcher_rollback_input_rejected",
    );
  }

  const adjacentDuplicate =
    BigInt(rollbackState.transitionCount) > 0n &&
    rollbackState.currentFinalityStateDigest === transition.next.stateDigest &&
    rollbackState.lastPreviousFinalityStateDigest ===
      transition.previous.stateDigest &&
    rollbackState.lastConsistencyDigest ===
      transition.consistency.consistencyDigest &&
    rollbackState.lastFinalityResultDigest ===
      transition.finalityResult.resultDigest &&
    rollbackState.lastInstructionDigest ===
      (transition.kind === "rewind"
        ? transition.instruction.instructionDigest
        : null);
  if (
    !adjacentDuplicate &&
    (BigInt(rollbackState.transitionCount) === 0n
      ? rollbackState.bootstrapFinalityState.stateDigest
      : rollbackState.currentFinalityStateDigest) !==
      transition.previous.stateDigest
  ) {
    return reject("stale_finality_state", "watcher_rollback_state_rejected");
  }
  if (transition.kind === "incident") {
    const consistencyEvidence = verifyPersistedConsistencyEvidence(
      policy,
      store,
      transition.consistency,
      transportAttestationsInput,
      undefined,
      authenticatedSnapshotEvidence,
    );
    if (consistencyEvidence === null) {
      return reject("consistency_evidence_missing");
    }
    const canonicalTransition = Object.freeze({
      ...transition,
      consistency: consistencyEvidence.consistency,
    });
    const transitionCount = (
      BigInt(rollbackState.transitionCount) + 1n
    ).toString();
    const nextStore = makeWatcherDurableStore({
      deploymentMarker: store.deploymentMarker,
      revision: (BigInt(store.revision) + 1n).toString(),
      records: store,
    });
    const nextStoreDigest = storeDigest(nextStore);
    const incident = makeIncident(
      policy,
      canonicalTransition,
      sourceStoreDigest,
      nextStoreDigest,
      transitionCount,
    );
    const nextRollback = advanceRollbackState(
      policy,
      rollbackState,
      rollbackBootstrapState,
      canonicalTransition,
      store,
      nextStoreDigest,
      incident,
    );
    return makeResult({
      action: "quarantine_incident",
      protocolDecision: "quarantined",
      reasonCodes: ["post_finality_incident"],
      alertCodes: ["watcher_rollback_post_finality_incident"],
      sourceRevision: store.revision,
      nextRevision: nextStore.revision,
      instructionDigest: null,
      sourceStoreDigest,
      nextStoreDigest,
      removedRecords: emptyRemovedRecords(),
      nextStore,
      rollbackState: nextRollback.state,
      rollbackBootstrapState: nextRollback.bootstrapState,
      trustedCheckpointStateDigest: trustedCheckpointStateDigest(
        nextRollback.bootstrapState,
      ),
    });
  }

  const instructionDigest = transition.instruction.instructionDigest;
  const replacementEvidence = verifyPersistedReplacementEvidence(
    policy,
    store,
    transition,
    transportAttestationsInput,
    authenticatedSnapshotEvidence,
  );
  if (replacementEvidence === null) {
    return reject("replacement_evidence_missing");
  }
  const canonicalTransition = Object.freeze({
    ...transition,
    consistency: replacementEvidence.consistency,
  });
  if (adjacentDuplicate) {
    return makeResult({
      action: "duplicate_rewind",
      protocolDecision: "hold",
      reasonCodes: ["duplicate_instruction"],
      alertCodes: [],
      sourceRevision: store.revision,
      nextRevision: store.revision,
      instructionDigest,
      sourceStoreDigest,
      nextStoreDigest: sourceStoreDigest,
      removedRecords: emptyRemovedRecords(),
      nextStore: store,
      rollbackState,
      rollbackBootstrapState,
      trustedCheckpointStateDigest: trustedCheckpointStateDigest(
        rollbackBootstrapState,
      ),
    });
  }

  const plan = planRewind(store, canonicalTransition, replacementEvidence);
  if (
    plan.removed.l1ObservationIds.some((id) =>
      replacementEvidence.observationIds.has(id),
    ) ||
    plan.removed.chainPointIds.some((id) =>
      replacementEvidence.chainPointIds.has(id),
    )
  ) {
    return reject("replacement_evidence_missing");
  }
  if (removedRecordCount(plan.removed) === 0) {
    return reject("unknown_rewind_target");
  }
  const nextStore = makeWatcherDurableStore({
    deploymentMarker: store.deploymentMarker,
    revision: (BigInt(store.revision) + 1n).toString(),
    records: plan.records,
  });
  const nextStoreDigest = storeDigest(nextStore);
  const nextRollback = advanceRollbackState(
    policy,
    rollbackState,
    rollbackBootstrapState,
    canonicalTransition,
    store,
    nextStoreDigest,
    null,
  );
  return makeResult({
    action: "apply_rewind",
    protocolDecision: "resume_pending",
    reasonCodes: ["rewind_applied"],
    alertCodes: ["watcher_rollback_rewind_applied"],
    sourceRevision: store.revision,
    nextRevision: nextStore.revision,
    instructionDigest,
    sourceStoreDigest,
    nextStoreDigest,
    removedRecords: plan.removed,
    nextStore,
    rollbackState: nextRollback.state,
    rollbackBootstrapState: nextRollback.bootstrapState,
    trustedCheckpointStateDigest: trustedCheckpointStateDigest(
      nextRollback.bootstrapState,
    ),
  });
};

export const replayWatcherRollbackState = (
  policy: WatcherFinalityPolicy,
  bootstrapState: WatcherRollbackState,
  candidate: WatcherRollbackState,
  currentStore: WatcherDurableStore,
  transportAttestationsInput: unknown,
  authenticatedSnapshotEvidence:
    | typeof AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE
    | null = null,
): WatcherRollbackState | null => {
  if (
    rollbackStateBindingFailure(policy, candidate) !== null ||
    !sameMarker(
      candidate.bootstrapStore.deploymentMarker,
      policy.deploymentMarker,
    )
  ) {
    return null;
  }
  if (
    candidate.epoch !== bootstrapState.epoch ||
    !watcherSameCanonicalJson(
      candidate.epochCheckpoint,
      bootstrapState.epochCheckpoint,
    ) ||
    !watcherSameCanonicalJson(
      candidate.bootstrapStore,
      bootstrapState.bootstrapStore,
    ) ||
    !watcherSameCanonicalJson(
      candidate.bootstrapFinalityState,
      bootstrapState.bootstrapFinalityState,
    )
  ) {
    return null;
  }
  let derivedState = bootstrapState;
  let derivedStore = bootstrapState.bootstrapStore;
  for (const transition of candidate.transitions) {
    const result = evaluateWatcherRollbackStep(
      policy,
      derivedStore,
      derivedState,
      bootstrapState,
      transition.previousFinalityState,
      transition.consistency,
      transition.finalityResult,
      transportAttestationsInput,
      authenticatedSnapshotEvidence,
    );
    if (
      !["apply_rewind", "quarantine_incident"].includes(result.action) ||
      result.nextStore === null ||
      result.rollbackState === null
    ) {
      return null;
    }
    derivedStore = result.nextStore;
    derivedState = result.rollbackState;
  }
  return watcherSameCanonicalJson(derivedStore, currentStore) &&
    watcherSameCanonicalJson(derivedState, candidate)
    ? derivedState
    : null;
};

/**
 * Creates the only valid zero-transition rollback state. Callers must persist
 * this explicit bootstrap and subsequently persist each returned state; the
 * evaluator never treats null or a compact self-hash as a fresh installation.
 */
export const makeWatcherRollbackBootstrapState = (
  policyInput: unknown,
  bootstrapStoreInput: unknown,
  bootstrapFinalityStateInput: unknown,
): WatcherRollbackState | null => {
  const policy = parseWatcherFinalityPolicy(policyInput);
  if (policy === null) {
    return null;
  }
  try {
    const bootstrapStore = parseWatcherDurableStore(bootstrapStoreInput);
    const bootstrapFinalityState = parseWatcherFinalityState(
      bootstrapFinalityStateInput,
      policy,
    );
    return sameMarker(
      bootstrapStore.deploymentMarker,
      policy.deploymentMarker,
    ) && bootstrapFinalityState !== null
      ? initialRollbackState(policy, bootstrapStore, bootstrapFinalityState)
      : null;
  } catch {
    return null;
  }
};

export const parseRollbackBootstrapStateWithTrustedDigest = (
  policy: WatcherFinalityPolicy,
  value: unknown,
  trustedCheckpointStateDigest: string | null,
  parseStore = parseWatcherDurableStore,
): WatcherRollbackState | null => {
  const candidate = decodeWatcherRollbackStateStructural(value, parseStore);
  if (
    candidate === null ||
    rollbackStateBindingFailure(policy, candidate) !== null ||
    parseWatcherFinalityState(candidate.bootstrapFinalityState, policy) === null
  ) {
    return null;
  }
  const expected = initialRollbackState(
    policy,
    candidate.bootstrapStore,
    candidate.bootstrapFinalityState,
  );
  return candidate.epoch === "0"
    ? watcherSameCanonicalJson(candidate, expected)
      ? candidate
      : null
    : candidate.epochCheckpoint !== null &&
        candidate.transitions.length === 0 &&
        candidate.incident === null &&
        trustedCheckpointStateDigest === candidate.stateDigest
      ? candidate
      : null;
};

const parseRollbackBootstrapState = (
  policy: WatcherFinalityPolicy,
  value: unknown,
  trustedCheckpointAuthorityInput: unknown = undefined,
): WatcherRollbackState | null => {
  const trustedAuthority =
    typeof trustedCheckpointAuthorityInput === "object" &&
    trustedCheckpointAuthorityInput !== null
      ? rollbackDurableAuthorityRuntime.get(
          trustedCheckpointAuthorityInput as WatcherRollbackDurableAuthority,
        )
      : undefined;
  return parseRollbackBootstrapStateWithTrustedDigest(
    policy,
    value,
    trustedAuthority !== undefined &&
      trustedAuthority.policy.policyDigest === policy.policyDigest
      ? trustedAuthority.snapshot.trustedCheckpointStateDigest
      : null,
  );
};

/**
 * Authoritative rollback-state restart parser. It deterministically replays
 * the bounded transition history from a separately persisted explicit
 * bootstrap state and requires every derived intermediate store to lead to
 * `currentStore`.
 */
export const parseWatcherRollbackState = (
  value: unknown,
  context: WatcherRollbackStateVerificationContext,
): WatcherRollbackState | null => {
  const candidate = decodeWatcherRollbackStateStructural(value);
  const policy = parseWatcherFinalityPolicy(context.policy);
  if (candidate === null || policy === null) {
    return null;
  }
  try {
    const bootstrapState = parseRollbackBootstrapState(
      policy,
      context.rollbackBootstrapState,
      context.trustedCheckpointAuthority,
    );
    const currentStore = parseWatcherDurableStore(context.currentStore);
    const authority = context.trustedCheckpointAuthority;
    const trustedRuntime =
      typeof authority === "object" && authority !== null
        ? rollbackDurableAuthorityRuntime.get(
            authority as WatcherRollbackDurableAuthority,
          )
        : undefined;
    // Only the complete already-authenticated snapshot may reuse its admitted
    // transport evidence after restart. Caller-supplied state keeps the live
    // transport validation path, even when it carries matching claimed digests.
    const authenticatedSnapshotEvidence =
      trustedRuntime !== undefined &&
      trustedRuntime.policy.policyDigest === policy.policyDigest &&
      watcherSameCanonicalJson(
        trustedRuntime.snapshot.currentStore,
        currentStore,
      ) &&
      watcherSameCanonicalJson(
        trustedRuntime.snapshot.rollbackState,
        candidate,
      ) &&
      watcherSameCanonicalJson(
        trustedRuntime.snapshot.rollbackBootstrapState,
        context.rollbackBootstrapState,
      )
        ? AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE
        : null;
    return bootstrapState === null
      ? null
      : replayWatcherRollbackState(
          policy,
          bootstrapState,
          candidate,
          currentStore,
          context.transportAttestations,
          authenticatedSnapshotEvidence,
        );
  } catch {
    return null;
  }
};

/**
 * Applies exactly one W11→W12 transition to W03's canonical durable store.
 * The explicit prior rollback state is fully replayed from bootstrap before
 * use; null cannot reset the journal after restart.
 */
export const evaluateWatcherRollback = (
  policyInput: unknown,
  storeInput: unknown,
  previousFinalityStateInput: unknown,
  consistencyInput: unknown,
  finalityResultInput: unknown,
  previousRollbackStateInput: unknown,
  rollbackBootstrapStateInput: unknown,
  trustedCheckpointAuthorityInput: unknown = undefined,
  transportAttestationsInput: unknown = [],
): WatcherRollbackResult => {
  const policy = parseWatcherFinalityPolicy(policyInput);
  if (policy === null) {
    return reject(
      "malformed_policy",
      "watcher_rollback_configuration_mismatch",
    );
  }
  let store: WatcherDurableStore;
  try {
    store = parseWatcherDurableStore(storeInput);
  } catch {
    return reject("malformed_store");
  }
  if (!sameMarker(store.deploymentMarker, policy.deploymentMarker)) {
    return reject(
      "deployment_mismatch",
      "watcher_rollback_configuration_mismatch",
    );
  }
  const rollbackStateCandidate = decodeWatcherRollbackStateStructural(
    previousRollbackStateInput,
  );
  if (rollbackStateCandidate === null) {
    return reject(
      "malformed_rollback_state",
      "watcher_rollback_state_rejected",
    );
  }
  const bindingFailure = rollbackStateBindingFailure(
    policy,
    rollbackStateCandidate,
  );
  if (bindingFailure !== null) {
    return reject(bindingFailure, "watcher_rollback_configuration_mismatch");
  }
  if (rollbackStateCandidate.storeDigest !== storeDigest(store)) {
    return reject(
      "rollback_state_store_mismatch",
      "watcher_rollback_state_rejected",
    );
  }
  const rollbackBootstrapState = parseRollbackBootstrapState(
    policy,
    rollbackBootstrapStateInput,
    trustedCheckpointAuthorityInput,
  );
  if (rollbackBootstrapState === null) {
    return reject(
      "malformed_rollback_state",
      "watcher_rollback_state_rejected",
    );
  }
  const rollbackState = replayWatcherRollbackState(
    policy,
    rollbackBootstrapState,
    rollbackStateCandidate,
    store,
    transportAttestationsInput,
  );
  if (rollbackState === null) {
    return reject(
      "malformed_rollback_state",
      "watcher_rollback_state_rejected",
    );
  }
  return evaluateWatcherRollbackStep(
    policy,
    store,
    rollbackState,
    rollbackBootstrapState,
    previousFinalityStateInput,
    consistencyInput,
    finalityResultInput,
    transportAttestationsInput,
  );
};
