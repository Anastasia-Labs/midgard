import type {
  WorkflowFundingAbandonmentHandoff,
  WorkflowFundingPreparedTransition,
} from "@al-ft/midgard-fault-proofs";

type Abandoned = Readonly<{
  transition: WorkflowFundingPreparedTransition;
  handoff: WorkflowFundingAbandonmentHandoff;
}>;

/**
 * Owner ruling (whichever lands wins): a superseded attempt holds nothing,
 * but every later attempt must spend one of its funding inputs so that at
 * most one of them lands. A superseded attempt is covered once a recorded,
 * not superseded, attempt shares such an input; retirement past k ends it.
 */
export const uncoveredSupersededAttempts = ({
  abandoned,
  submissions,
}: {
  readonly abandoned: readonly Abandoned[];
  readonly submissions: readonly WorkflowFundingPreparedTransition[];
}): readonly WorkflowFundingPreparedTransition[] => {
  const superseded = new Set(
    abandoned.map(({ transition }) => transition.transactionHash),
  );
  const live = submissions.filter(
    ({ transactionHash }) => !superseded.has(transactionHash),
  );
  return abandoned
    .filter(({ handoff }) => handoff.reconciliation.retirement === undefined)
    .map(({ transition }) => transition)
    .filter(
      (transition) =>
        !live.some((attempt) =>
          attempt.consumedOutRefs.some((outRef) =>
            transition.consumedOutRefs.includes(outRef),
          ),
        ),
    );
};

/** Inputs whose spend excludes every uncovered attempt at once; null when
 * there is none to exclude. Empty means no fresh attempt can exclude them. */
export const supersededExclusionOutRefs = (
  uncovered: readonly WorkflowFundingPreparedTransition[],
): readonly string[] | null =>
  uncovered.length === 0
    ? null
    : uncovered
        .slice(1)
        .reduce(
          (common, { consumedOutRefs }) =>
            common.filter((outRef) => consumedOutRefs.includes(outRef)),
          [...uncovered[0]!.consumedOutRefs],
        )
        .sort();

export const spendsEveryUncoveredAttempt = (
  uncovered: readonly WorkflowFundingPreparedTransition[],
  consumedOutRefs: readonly string[],
): boolean =>
  uncovered.every((transition) =>
    transition.consumedOutRefs.some((outRef) =>
      consumedOutRefs.includes(outRef),
    ),
  );
