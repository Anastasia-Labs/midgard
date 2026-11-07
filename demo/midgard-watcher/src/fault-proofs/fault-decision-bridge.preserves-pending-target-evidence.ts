import { type WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import { watcherSameCanonicalJson } from "../storage/durable-store.js";
import {
  observationPreservesTarget,
  preservesObservationAuthority,
  type WatcherFaultDecisionTarget,
} from "./fault-decision-bridge.selected-target.js";

/** Availability rechecks cannot erase an already admitted immutable fault.
 * This does not admit a new decision: the exact target, inclusion seal, queue
 * output, source authority and correction lock must still be unchanged. */
export const preservesPendingTargetEvidence = (
  previous: WatcherAuthenticatedStateQueueObservation,
  candidate: WatcherAuthenticatedStateQueueObservation,
  target: WatcherFaultDecisionTarget,
): boolean => {
  const priorHeader = previous.finalizedHeaders.find(
    (header) => header.headerHash === target.headerHash,
  );
  const currentHeader = candidate.finalizedHeaders.find(
    (header) => header.headerHash === target.headerHash,
  );
  if (priorHeader === undefined || currentHeader === undefined) return false;
  const { finalityDepth: priorHeaderDepth, ...priorHeaderSeal } = priorHeader;
  const { finalityDepth: currentHeaderDepth, ...currentHeaderSeal } =
    currentHeader;
  return (
    preservesObservationAuthority(previous, candidate) &&
    BigInt(currentHeaderDepth) >= BigInt(priorHeaderDepth) &&
    watcherSameCanonicalJson(priorHeaderSeal, currentHeaderSeal) &&
    watcherSameCanonicalJson(
      previous.finalizedCorrectionLock,
      candidate.finalizedCorrectionLock,
    ) &&
    observationPreservesTarget(candidate, target)
  );
};
