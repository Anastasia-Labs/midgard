import {
  parseWatcherFinalityState,
  type WatcherFinalityIncident,
  type WatcherFinalityResult,
} from ".././finality-engine.js";
import { type WatcherMultiProviderConsistency } from ".././multi-provider-consistency.js";
import { exactPlainRecord, marker } from "./records.js";
import { parseBoundObservation } from "./state.advance-rollback-state.js";
import {
  CANONICAL_NATURAL,
  HEX_32,
  sha256Canonical,
  WATCHER_ROLLBACK_EPOCH_CHECKPOINT_SCHEMA_VERSION,
  WATCHER_ROLLBACK_INCIDENT_SCHEMA_VERSION,
  WATCHER_ROLLBACK_TRANSITION_SCHEMA_VERSION,
  type WatcherRollbackEpochCheckpoint,
  type WatcherRollbackIncident,
  type WatcherRollbackTransition,
} from "./types.js";

export const parseRollbackIncident = (
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

export const parseTransitionRecord = (
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

export const parseEpochCheckpoint = (
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
