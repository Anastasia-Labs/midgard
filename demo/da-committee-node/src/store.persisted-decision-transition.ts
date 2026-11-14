import type {
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
