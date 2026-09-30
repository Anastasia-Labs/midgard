import {
  parseWatcherFinalityState,
  type WatcherFinalityIncident,
} from ".././finality-engine.js";
import {
  exactPlainRecord,
  exactUnrestrictedStringArray,
  marker,
} from "./records.js";
import { parseRemovedRecords } from "./state.js";
import {
  CANONICAL_NATURAL,
  HEX_32,
  isNetwork,
  sha256Canonical,
  WATCHER_POST_FINALITY_RECOVERY_STATE_SCHEMA_VERSION,
  type WatcherPostFinalityRecoveryPath,
  type WatcherPostFinalityRecoveryState,
} from "./types.js";

const parseRecoveryPath = (
  value: unknown,
): WatcherPostFinalityRecoveryPath | null => {
  const path = exactPlainRecord(value, [
    "commonAncestorPointDigest",
    "commonAncestorBlockHash",
    "commonAncestorBlockNo",
    "orphanedFinalizedPointDigest",
    "orphanedFinalizedBlockHash",
    "replacementTipPointDigest",
    "replacementTipBlockHash",
    "replacementTipBlockNo",
    "rollbackDepth",
    "previousConsistencyDigests",
    "replacementConsistencyDigests",
    "pathDigest",
  ]);
  if (
    path === null ||
    ![
      path.commonAncestorPointDigest,
      path.commonAncestorBlockHash,
      path.orphanedFinalizedPointDigest,
      path.orphanedFinalizedBlockHash,
      path.replacementTipPointDigest,
      path.replacementTipBlockHash,
      path.pathDigest,
    ].every((digest) => typeof digest === "string" && HEX_32.test(digest)) ||
    ![
      path.commonAncestorBlockNo,
      path.replacementTipBlockNo,
      path.rollbackDepth,
    ].every(
      (natural) =>
        typeof natural === "string" && CANONICAL_NATURAL.test(natural),
    )
  ) {
    return null;
  }
  const previousConsistencyDigests = exactUnrestrictedStringArray(
    path.previousConsistencyDigests,
  );
  const replacementConsistencyDigests = exactUnrestrictedStringArray(
    path.replacementConsistencyDigests,
  );
  if (
    previousConsistencyDigests === null ||
    replacementConsistencyDigests === null ||
    previousConsistencyDigests.length < 2 ||
    replacementConsistencyDigests.length < 2 ||
    [...previousConsistencyDigests, ...replacementConsistencyDigests].some(
      (digest) => !HEX_32.test(digest),
    )
  ) {
    return null;
  }
  const canonical = {
    commonAncestorPointDigest: path.commonAncestorPointDigest as string,
    commonAncestorBlockHash: path.commonAncestorBlockHash as string,
    commonAncestorBlockNo: path.commonAncestorBlockNo as string,
    orphanedFinalizedPointDigest: path.orphanedFinalizedPointDigest as string,
    orphanedFinalizedBlockHash: path.orphanedFinalizedBlockHash as string,
    replacementTipPointDigest: path.replacementTipPointDigest as string,
    replacementTipBlockHash: path.replacementTipBlockHash as string,
    replacementTipBlockNo: path.replacementTipBlockNo as string,
    rollbackDepth: path.rollbackDepth as string,
    previousConsistencyDigests: Object.freeze([...previousConsistencyDigests]),
    replacementConsistencyDigests: Object.freeze([
      ...replacementConsistencyDigests,
    ]),
  };
  return sha256Canonical(canonical) === path.pathDigest
    ? Object.freeze({ ...canonical, pathDigest: path.pathDigest as string })
    : null;
};

export const decodePostFinalityRecoveryState = (
  value: unknown,
): WatcherPostFinalityRecoveryState | null => {
  try {
    const state = exactPlainRecord(value, [
      "schemaVersion",
      "policyDigest",
      "network",
      "blueprintHash",
      "deploymentMarker",
      "sourceRollbackStateDigest",
      "sourceStoreDigest",
      "nextStoreDigest",
      "path",
      "removedRecords",
      "resumableFinalityState",
      "incidentLifecycle",
      "stateDigest",
    ]);
    if (
      state === null ||
      state.schemaVersion !==
        WATCHER_POST_FINALITY_RECOVERY_STATE_SCHEMA_VERSION ||
      !isNetwork(state.network) ||
      ![
        state.policyDigest,
        state.blueprintHash,
        state.sourceRollbackStateDigest,
        state.sourceStoreDigest,
        state.nextStoreDigest,
        state.stateDigest,
      ].every((digest) => typeof digest === "string" && HEX_32.test(digest))
    ) {
      return null;
    }
    const deploymentMarker = marker(state.deploymentMarker);
    const path = parseRecoveryPath(state.path);
    const removedRecords = parseRemovedRecords(state.removedRecords);
    const resumableFinalityState = parseWatcherFinalityState(
      state.resumableFinalityState,
    );
    const lifecycle = exactPlainRecord(state.incidentLifecycle, [
      "detectedIncidentDigest",
      "detectedReasonCode",
      "detectedTriggerConsistencyDigest",
      "detectedFinalityStateDigest",
      "detectedStoreDigest",
      "status",
      "recoveryPathDigest",
      "recoveredStoreDigest",
      "resumableFinalityStateDigest",
      "lifecycleDigest",
    ]);
    if (
      deploymentMarker === null ||
      path === null ||
      removedRecords === null ||
      resumableFinalityState === null ||
      lifecycle === null ||
      lifecycle.status !== "recovered" ||
      lifecycle.detectedReasonCode !== "post_finality_point_changed" ||
      !(
        lifecycle.detectedTriggerConsistencyDigest === null ||
        (typeof lifecycle.detectedTriggerConsistencyDigest === "string" &&
          HEX_32.test(lifecycle.detectedTriggerConsistencyDigest))
      ) ||
      ![
        lifecycle.detectedIncidentDigest,
        lifecycle.detectedFinalityStateDigest,
        lifecycle.detectedStoreDigest,
        lifecycle.recoveryPathDigest,
        lifecycle.recoveredStoreDigest,
        lifecycle.resumableFinalityStateDigest,
        lifecycle.lifecycleDigest,
      ].every((digest) => typeof digest === "string" && HEX_32.test(digest))
    ) {
      return null;
    }
    const lifecycleCanonical = {
      detectedIncidentDigest: lifecycle.detectedIncidentDigest as string,
      detectedReasonCode:
        lifecycle.detectedReasonCode as WatcherFinalityIncident["reasonCode"],
      detectedTriggerConsistencyDigest:
        lifecycle.detectedTriggerConsistencyDigest as string | null,
      detectedFinalityStateDigest:
        lifecycle.detectedFinalityStateDigest as string,
      detectedStoreDigest: lifecycle.detectedStoreDigest as string,
      status: "recovered" as const,
      recoveryPathDigest: lifecycle.recoveryPathDigest as string,
      recoveredStoreDigest: lifecycle.recoveredStoreDigest as string,
      resumableFinalityStateDigest:
        lifecycle.resumableFinalityStateDigest as string,
    };
    if (
      sha256Canonical(lifecycleCanonical) !== lifecycle.lifecycleDigest ||
      lifecycleCanonical.detectedStoreDigest !== state.sourceStoreDigest ||
      lifecycleCanonical.recoveryPathDigest !== path.pathDigest ||
      lifecycleCanonical.recoveredStoreDigest !== state.nextStoreDigest ||
      lifecycleCanonical.resumableFinalityStateDigest !==
        resumableFinalityState.stateDigest
    ) {
      return null;
    }
    const canonical = {
      schemaVersion: WATCHER_POST_FINALITY_RECOVERY_STATE_SCHEMA_VERSION,
      policyDigest: state.policyDigest as string,
      network: state.network as WatcherPostFinalityRecoveryState["network"],
      blueprintHash: state.blueprintHash as string,
      deploymentMarker,
      sourceRollbackStateDigest: state.sourceRollbackStateDigest as string,
      sourceStoreDigest: state.sourceStoreDigest as string,
      nextStoreDigest: state.nextStoreDigest as string,
      path,
      removedRecords,
      resumableFinalityState,
      incidentLifecycle: Object.freeze({
        ...lifecycleCanonical,
        lifecycleDigest: lifecycle.lifecycleDigest as string,
      }),
    };
    return sha256Canonical(canonical) === state.stateDigest
      ? Object.freeze({
          ...canonical,
          stateDigest: state.stateDigest as string,
        })
      : null;
  } catch {
    return null;
  }
};
