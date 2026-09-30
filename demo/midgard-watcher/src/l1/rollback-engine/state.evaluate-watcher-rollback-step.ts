import {
  makeWatcherDurableStore,
  type WatcherDurableStore,
  watcherSameCanonicalJson,
} from "../../storage/durable-store.js";
import { type WatcherFinalityPolicy } from ".././finality-engine.js";
import {
  emptyRemovedRecords,
  makeResult,
  reject,
  sameMarker,
} from "./records.js";
import { advanceRollbackState } from "./state.advance-rollback-state.js";
import { removedRecordCount } from "./state.decode-watcher-rollback-state-structural.js";
import {
  makeIncident,
  parseFinalityTransition,
  storeDigest,
} from "./state.parse-finality-transition.js";
import {
  planRewind,
  rollbackStateBindingFailure,
  trustedCheckpointStateDigest,
} from "./state.plan-rewind.js";
import {
  AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE,
  verifyPersistedConsistencyEvidence,
  verifyPersistedReplacementEvidence,
} from "./state.verify-persisted-consistency-evidence.js";
import {
  type WatcherRollbackReasonCode,
  type WatcherRollbackResult,
  type WatcherRollbackState,
} from "./types.js";

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
