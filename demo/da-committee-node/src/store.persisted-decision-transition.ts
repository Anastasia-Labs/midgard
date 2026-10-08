import type { DaSignatureRecordV1 } from "./domain.js";
import type {
  DecisionOutboxRecord,
  L1ObservedDecision,
  L1SourceState,
} from "./store.committee-store.js";

/**
 * Merges a proposed L1 source state into the stored one. The observations
 * are informational: the decisions themselves are the class B record, and a
 * decision is never re-signed with different content whatever L1 shows. An
 * observation of a header with a persisted decision is kept when the
 * proposal omits it, and its `hasPersistedDecision` flag is sticky.
 */
export const mergeL1SourceState = (
  current: L1SourceState | undefined,
  proposed: L1SourceState,
): L1SourceState => {
  if (current === undefined) {
    return proposed;
  }
  if (
    current.sourceMode !== proposed.sourceMode ||
    current.network !== proposed.network
  ) {
    throw new Error(
      "committee node L1 source authority changed without a reset",
    );
  }
  return {
    ...proposed,
    observations: mergePersistedDecisionObservations(
      current.observations,
      proposed.observations,
    ),
  };
};

const mergePersistedDecisionObservations = (
  current: readonly L1ObservedDecision[],
  proposed: readonly L1ObservedDecision[],
): readonly L1ObservedDecision[] => {
  const observations = new Map(
    proposed.map((entry) => [entry.headerHash, entry] as const),
  );
  for (const prior of current) {
    if (!prior.hasPersistedDecision) {
      continue;
    }
    const next = observations.get(prior.headerHash);
    observations.set(
      prior.headerHash,
      next === undefined ? prior : { ...next, hasPersistedDecision: true },
    );
  }
  return [...observations.values()].sort((left, right) =>
    left.headerHash.localeCompare(right.headerHash),
  );
};

export const assertDecisionSourceState = (
  effect: DecisionOutboxRecord,
  sourceState: L1SourceState,
): void => {
  const observation = sourceState.observations.find(
    ({ headerHash }) => headerHash === effect.headerHash,
  );
  if (
    sourceState.status !== "healthy" ||
    sourceState.sourceMode !== effect.sourceMode ||
    sourceState.network !== effect.network ||
    observation?.stateQueueOutRef !== effect.stateQueueOutRef ||
    observation.hasPersistedDecision !== true
  ) {
    throw new Error("decision outbox lacks matching durable L1 observation");
  }
};

export const assertDecisionSignature = (
  effect: DecisionOutboxRecord,
  signature: DaSignatureRecordV1 | undefined,
): void => {
  if (
    (effect.effectKind === "signature_publish" &&
      (signature === undefined ||
        signature.deploymentFingerprint !== effect.deploymentFingerprint ||
        signature.headerHash !== effect.headerHash ||
        signature.signerIndex !== effect.signerIndex ||
        signature.validation.stateQueueOutRef !== effect.stateQueueOutRef ||
        signature.l1ChainPoint.slot !== effect.slot ||
        signature.l1ChainPoint.blockHash !== effect.blockHash)) ||
    (effect.effectKind === "l1_reconcile" && signature !== undefined)
  ) {
    throw new Error("decision outbox signature does not match effect identity");
  }
};
