import {
  makeWatcherDurableStore,
  parseWatcherDurableStore,
  type WatcherDurableStore,
  watcherSameCanonicalJson,
} from "../../storage/durable-store.js";
import {
  parseWatcherFinalityPolicy,
  parseWatcherFinalityState,
} from ".././finality-engine.js";
import {
  emptyRemovedRecords,
  exactPlainRecord,
  sameMarker,
} from "./records.js";
import { decodePostFinalityRecoveryState } from "./recovery.decode-post-finality-recovery-state.js";
import {
  makePostFinalityRecoveryResult,
  makePostFinalityRecoveryState,
  makeRecoveryPath,
  makeResumableFinalityState,
  rejectPostFinalityRecovery,
  sameAgreementPoint,
  verifyPostFinalityPath,
} from "./recovery.verify-post-finality-path.js";
import {
  AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE,
  indexPersistedObservations,
  makeEpochBootstrapState,
  parseWatcherRollbackState,
  type PersistedConsistencyEvidence,
  planRewind,
  removedRecordCount,
  storeDigest,
} from "./state.js";
import {
  type ParsedFinalityTransition,
  rollbackDurableAuthorityRuntime,
  WATCHER_ROLLBACK_BOUNDS,
  type WatcherPostFinalityRecoveryInput,
  type WatcherPostFinalityRecoveryResult,
  type WatcherRollbackDurableAuthority,
} from "./types.js";

/**
 * Resolves a durable post-finality incident only after W10 bytes and W11
 * decisions prove both sides of the exact common ancestor. The recovery is
 * one W03 revision: every dependent orphan record is removed together while
 * canonical replacement evidence remains available for deterministic replay.
 */
export const evaluateWatcherPostFinalityRecoveryInternal = (
  input: WatcherPostFinalityRecoveryInput,
): WatcherPostFinalityRecoveryResult => {
  const recoveryInputKeys = [
    "policy",
    "sourceStore",
    "currentStore",
    "quarantinedRollbackState",
    "rollbackBootstrapState",
    "previousCanonicalPath",
    "replacementCanonicalPath",
    "previousRecoveryState",
  ];
  const hasTrustedCheckpointAuthority =
    typeof input === "object" &&
    input !== null &&
    !Array.isArray(input) &&
    Reflect.ownKeys(input).includes("trustedCheckpointAuthority");
  const hasTransportAttestations =
    typeof input === "object" &&
    input !== null &&
    !Array.isArray(input) &&
    Reflect.ownKeys(input).includes("transportAttestations");
  if (
    exactPlainRecord(input, [
      ...recoveryInputKeys,
      ...(hasTrustedCheckpointAuthority ? ["trustedCheckpointAuthority"] : []),
      ...(hasTransportAttestations ? ["transportAttestations"] : []),
    ]) === null
  ) {
    return rejectPostFinalityRecovery("recovery_path_malformed");
  }
  const policy = parseWatcherFinalityPolicy(input.policy);
  if (policy === null) {
    return rejectPostFinalityRecovery("malformed_policy");
  }
  if (
    BigInt(policy.maximumPostFinalityRecoveryDepth) >
    WATCHER_ROLLBACK_BOUNDS.postFinalityRecoveryDepth
  ) {
    return rejectPostFinalityRecovery("malformed_policy");
  }
  let sourceStore: WatcherDurableStore;
  let currentStore: WatcherDurableStore;
  try {
    sourceStore = parseWatcherDurableStore(input.sourceStore);
    currentStore = parseWatcherDurableStore(input.currentStore);
  } catch {
    return rejectPostFinalityRecovery("malformed_store");
  }
  if (
    !sameMarker(sourceStore.deploymentMarker, policy.deploymentMarker) ||
    !sameMarker(currentStore.deploymentMarker, policy.deploymentMarker)
  ) {
    return rejectPostFinalityRecovery("malformed_store");
  }
  const sourceStoreDigest = storeDigest(sourceStore);
  const rollbackState = parseWatcherRollbackState(
    input.quarantinedRollbackState,
    {
      policy,
      rollbackBootstrapState: input.rollbackBootstrapState,
      trustedCheckpointAuthority: input.trustedCheckpointAuthority,
      currentStore: sourceStore,
      transportAttestations: input.transportAttestations,
    },
  );
  if (rollbackState === null) {
    return rejectPostFinalityRecovery("malformed_rollback_state");
  }
  if (
    rollbackState.storeDigest !== sourceStoreDigest ||
    rollbackState.incident === null
  ) {
    return rejectPostFinalityRecovery(
      rollbackState.incident === null
        ? "incident_required"
        : "rollback_state_store_mismatch",
    );
  }
  const persistedIndex = indexPersistedObservations(sourceStore);
  const authenticatedSnapshotEvidence = (() => {
    if (
      typeof input.trustedCheckpointAuthority !== "object" ||
      input.trustedCheckpointAuthority === null
    ) {
      return null;
    }
    const runtime = rollbackDurableAuthorityRuntime.get(
      input.trustedCheckpointAuthority as WatcherRollbackDurableAuthority,
    );
    return runtime !== undefined &&
      watcherSameCanonicalJson(runtime.snapshot.currentStore, sourceStore)
      ? AUTHENTICATED_ROLLBACK_SNAPSHOT_EVIDENCE
      : null;
  })();
  const previousPath = verifyPostFinalityPath(
    policy,
    sourceStore,
    input.previousCanonicalPath,
    persistedIndex,
    input.transportAttestations,
    authenticatedSnapshotEvidence,
  );
  if (typeof previousPath === "string") {
    return rejectPostFinalityRecovery(previousPath);
  }
  const replacementPath = verifyPostFinalityPath(
    policy,
    sourceStore,
    input.replacementCanonicalPath,
    persistedIndex,
    input.transportAttestations,
    authenticatedSnapshotEvidence,
  );
  if (typeof replacementPath === "string") {
    return rejectPostFinalityRecovery(replacementPath);
  }
  const commonAncestor = previousPath.agreements[0]!;
  const replacementAncestor = replacementPath.agreements[0]!;
  const orphanedFinalized = previousPath.agreements.at(-1)!;
  const finalizedBinding = rollbackState.incident.finalizedBinding;
  if (
    !sameAgreementPoint(commonAncestor, replacementAncestor) ||
    previousPath.agreements
      .slice(1)
      .some((oldPoint) =>
        replacementPath.agreements
          .slice(1)
          .some((newPoint) => oldPoint.blockHash === newPoint.blockHash),
      )
  ) {
    return rejectPostFinalityRecovery("common_ancestor_mismatch");
  }
  if (
    orphanedFinalized.pointDigest !== finalizedBinding.pointDigest ||
    orphanedFinalized.blockHash !== finalizedBinding.blockHash ||
    orphanedFinalized.slot !== finalizedBinding.slot ||
    orphanedFinalized.blockNo !== finalizedBinding.blockNo ||
    orphanedFinalized.blockContentDigest !==
      finalizedBinding.blockContentDigest ||
    previousPath.consistencyDigests.at(-1) !==
      finalizedBinding.lastSeenConsistencyDigest
  ) {
    return rejectPostFinalityRecovery("finalized_binding_mismatch");
  }
  if (
    rollbackState.incident.triggerConsistencyDigest === null ||
    replacementPath.consistencyDigests.at(-1) !==
      rollbackState.incident.triggerConsistencyDigest
  ) {
    return rejectPostFinalityRecovery("incident_provenance_mismatch");
  }
  const rollbackDepth =
    BigInt(orphanedFinalized.blockNo) - BigInt(commonAncestor.blockNo);
  if (
    rollbackDepth <= 0n ||
    rollbackDepth > BigInt(policy.maximumPostFinalityRecoveryDepth) ||
    BigInt(replacementPath.agreements.at(-1)!.blockNo) -
      BigInt(commonAncestor.blockNo) >
      BigInt(policy.maximumPostFinalityRecoveryDepth)
  ) {
    return rejectPostFinalityRecovery("recovery_depth_exceeded");
  }
  const path = makeRecoveryPath(previousPath, replacementPath);
  const previousRecoveryState =
    input.previousRecoveryState === null
      ? null
      : decodePostFinalityRecoveryState(input.previousRecoveryState);
  if (input.previousRecoveryState !== null && previousRecoveryState === null) {
    return rejectPostFinalityRecovery("malformed_recovery_state");
  }
  if (
    previousRecoveryState === null &&
    !watcherSameCanonicalJson(currentStore, sourceStore)
  ) {
    return rejectPostFinalityRecovery("recovery_state_mismatch");
  }
  const replacementEvidence: PersistedConsistencyEvidence = Object.freeze({
    consistency: replacementPath.evidence.at(-1)!.consistency,
    observations: Object.freeze(
      replacementPath.evidence.flatMap(({ observations }) => observations),
    ),
    observationIds: replacementPath.observationIds,
    chainPointIds: replacementPath.chainPointIds,
  });
  const firstReplacement = replacementPath.agreements[1]!;
  const syntheticTransition = {
    previous: {
      pending: {
        ...finalizedBinding,
        blockNo: (BigInt(commonAncestor.blockNo) + 1n).toString(),
      },
    },
    next: {
      pending: {
        pointDigest: firstReplacement.pointDigest,
        blockHash: firstReplacement.blockHash,
        slot: firstReplacement.slot,
        blockNo: firstReplacement.blockNo,
        blockContentDigest: firstReplacement.blockContentDigest,
        firstSeenConsistencyDigest: replacementPath.consistencyDigests[1]!,
        lastSeenConsistencyDigest: replacementPath.consistencyDigests[1]!,
        firstSeenDepth: firstReplacement.minimumDepth,
        currentDepth: firstReplacement.minimumDepth,
        visibilityCount: "1",
      },
    },
    instruction: { kind: "pending_point_changed" },
  } as unknown as Extract<
    ParsedFinalityTransition,
    { readonly kind: "rewind" }
  >;
  const plan = planRewind(
    sourceStore,
    syntheticTransition,
    replacementEvidence,
  );
  if (
    plan.removed.l1ObservationIds.some((id) =>
      replacementPath.observationIds.has(id),
    ) ||
    plan.removed.chainPointIds.some((id) =>
      replacementPath.chainPointIds.has(id),
    )
  ) {
    return rejectPostFinalityRecovery("canonical_agreement_required");
  }
  if (removedRecordCount(plan.removed) === 0) {
    return rejectPostFinalityRecovery("unknown_recovery_target");
  }
  const nextStore = makeWatcherDurableStore({
    deploymentMarker: sourceStore.deploymentMarker,
    revision: (BigInt(sourceStore.revision) + 1n).toString(),
    records: plan.records,
  });
  const nextStoreDigest = storeDigest(nextStore);
  const resumableFinalityState = makeResumableFinalityState(policy);
  if (parseWatcherFinalityState(resumableFinalityState, policy) === null) {
    return rejectPostFinalityRecovery("malformed_policy");
  }
  const recoveryState = makePostFinalityRecoveryState(
    policy,
    rollbackState,
    sourceStoreDigest,
    nextStoreDigest,
    path,
    plan.removed,
    resumableFinalityState,
  );
  const resumableRollbackState = makeEpochBootstrapState(
    policy,
    rollbackState,
    nextStore,
    resumableFinalityState,
    {
      stateDigest: recoveryState.stateDigest,
      lifecycleDigest: recoveryState.incidentLifecycle.lifecycleDigest,
    },
  );
  if (previousRecoveryState !== null) {
    const currentStoreDigest = storeDigest(currentStore);
    if (
      currentStoreDigest !== nextStoreDigest ||
      !watcherSameCanonicalJson(currentStore, nextStore) ||
      !watcherSameCanonicalJson(previousRecoveryState, recoveryState)
    ) {
      return rejectPostFinalityRecovery("recovery_state_mismatch");
    }
    return makePostFinalityRecoveryResult({
      action: "duplicate_recovery",
      protocolDecision: "hold",
      reasonCodes: ["duplicate_recovery"],
      sourceRevision: currentStore.revision,
      nextRevision: currentStore.revision,
      sourceStoreDigest: currentStoreDigest,
      nextStoreDigest: currentStoreDigest,
      removedRecords: emptyRemovedRecords(),
      nextStore: currentStore,
      resumableFinalityState,
      resumableRollbackState,
      resumableRollbackBootstrapState: resumableRollbackState,
      resumableTrustedCheckpointStateDigest: resumableRollbackState.stateDigest,
      recoveryState,
    });
  }
  return makePostFinalityRecoveryResult({
    action: "rewind_and_replay",
    protocolDecision: "resume_replay",
    reasonCodes: ["recovery_applied"],
    sourceRevision: sourceStore.revision,
    nextRevision: nextStore.revision,
    sourceStoreDigest,
    nextStoreDigest,
    removedRecords: plan.removed,
    nextStore,
    resumableFinalityState,
    resumableRollbackState,
    resumableRollbackBootstrapState: resumableRollbackState,
    resumableTrustedCheckpointStateDigest: resumableRollbackState.stateDigest,
    recoveryState,
  });
};
