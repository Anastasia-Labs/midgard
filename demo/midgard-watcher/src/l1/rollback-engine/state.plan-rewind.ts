import {
  type WatcherDurableRecords,
  type WatcherDurableStore,
} from "../../storage/durable-store.js";
import {
  type WatcherFinalityBoundObservation,
  type WatcherFinalityPolicy,
} from ".././finality-engine.js";
import { sameMarker } from "./records.js";
import {
  type PersistedConsistencyEvidence,
  sorted,
} from "./state.decode-watcher-rollback-state-structural.js";
import {
  type ParsedFinalityTransition,
  type WatcherRollbackReasonCode,
  type WatcherRollbackRemovedRecords,
  type WatcherRollbackState,
} from "./types.js";

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

export const rollbackStateBindingFailure = (
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

export const trustedCheckpointStateDigest = (
  bootstrapState: WatcherRollbackState,
): string | null =>
  bootstrapState.epoch === "0" ? null : bootstrapState.stateDigest;
