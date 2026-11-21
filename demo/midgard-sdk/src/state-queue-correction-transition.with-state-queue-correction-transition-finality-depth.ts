import { parseStateQueueCorrectionTransition } from "./state-queue-correction-transition.derive-state-queue-authenticated-transition.js";
import { parseStateQueueAuthenticatedTransition } from "./state-queue-correction-transition.parse-state-queue-authenticated-transition.js";
import {
  digest,
  type Json,
  NATURAL,
  type StateQueueAuthenticatedTransition,
  type StateQueueCorrectionTransition,
  withoutDigest,
} from "./state-queue-correction-transition.state-queue-correction-lock-witness.js";

export const withStateQueueAuthenticatedTransitionFinalityDepth = (
  transitionInput: unknown,
  finalityDepth: string,
): StateQueueAuthenticatedTransition | null => {
  const transition = parseStateQueueAuthenticatedTransition(transitionInput);
  if (
    transition === null ||
    !NATURAL.test(finalityDepth) ||
    BigInt(finalityDepth) === 0n ||
    BigInt(finalityDepth) < BigInt(transition.finalityDepth)
  ) {
    return null;
  }
  const correctionTransition =
    transition.correctionTransition === null
      ? null
      : withStateQueueCorrectionTransitionFinalityDepth(
          transition.correctionTransition,
          finalityDepth,
        );
  if (
    transition.correctionTransition !== null &&
    correctionTransition === null
  ) {
    return null;
  }
  const { transitionDigest: _priorDigest, ...withoutPriorDigest } = transition;
  const canonical = {
    ...withoutPriorDigest,
    finalityDepth,
    correctionTransition,
  };
  const rebound = {
    ...canonical,
    transitionDigest: digest(canonical as unknown as Json),
  };
  return parseStateQueueAuthenticatedTransition(rebound);
};

/**
 * Advances only the finality attestation of an already canonical transition.
 * The L1 observer must independently prove that the same block remains on its
 * selected chain; this helper merely rebinds that newly observed depth into the
 * transition digest without replaying topology from an unauthenticated shape.
 */
export const withStateQueueCorrectionTransitionFinalityDepth = (
  transitionInput: unknown,
  finalityDepth: string,
): StateQueueCorrectionTransition | null => {
  const transition = parseStateQueueCorrectionTransition(transitionInput);
  if (
    transition === null ||
    !NATURAL.test(finalityDepth) ||
    BigInt(finalityDepth) === 0n ||
    BigInt(finalityDepth) < BigInt(transition.finalityDepth)
  ) {
    return null;
  }
  const canonical = {
    schemaVersion: transition.schemaVersion,
    deploymentIdentityDigest: transition.deploymentIdentityDigest,
    stateQueuePolicyId: transition.stateQueuePolicyId,
    transactionHash: transition.transactionHash,
    blockHash: transition.blockHash,
    slot: transition.slot,
    blockNo: transition.blockNo,
    chainPointId: transition.chainPointId,
    finalityDepth,
    timedOutHeaderHash: transition.timedOutHeaderHash,
    removalApproach: transition.removalApproach,
    consumedQueueOutRefs: transition.consumedQueueOutRefs,
    continuedQueueOutRefs: transition.continuedQueueOutRefs,
    removedHeaderHashes: transition.removedHeaderHashes,
  } satisfies Omit<StateQueueCorrectionTransition, "transitionDigest">;
  return Object.freeze({
    ...canonical,
    transitionDigest: digest(withoutDigest(canonical)),
  });
};
