import { type WatcherDurableStore } from "../../storage/durable-store.js";
import {
  type WatcherFinalityBoundObservation,
  type WatcherFinalityPolicy,
  type WatcherFinalityState,
} from ".././finality-engine.js";
import { exactPlainRecord } from "./records.js";
import {
  initialRollbackState,
  makeRollbackState,
  storeDigest,
} from "./state.parse-finality-transition.js";
import {
  CANONICAL_NATURAL,
  HEX_32,
  type ParsedFinalityTransition,
  sha256Canonical,
  WATCHER_ROLLBACK_BOUNDS,
  WATCHER_ROLLBACK_EPOCH_CHECKPOINT_SCHEMA_VERSION,
  WATCHER_ROLLBACK_TRANSITION_SCHEMA_VERSION,
  type WatcherRollbackEpochCheckpoint,
  type WatcherRollbackIncident,
  type WatcherRollbackState,
  type WatcherRollbackTransition,
} from "./types.js";

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

export const advanceRollbackState = (
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

export const parseBoundObservation = (
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
