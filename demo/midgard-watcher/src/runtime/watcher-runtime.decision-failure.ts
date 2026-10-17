import { classifyFailure } from "@al-ft/midgard-l1-follower";

import { isWatcherL1TransientFailure } from "../l1/transient-failure.js";

/**
 * Whether a failed decision pass or retirement reset says only that a
 * dependency did not answer: an L1 transient (`isWatcherL1TransientFailure`)
 * or a store failure the follower classifies as transient
 * (`classifyFailure`). An unrecognised failure is not transient.
 */
export const isDecisionFailureTransient = (error: unknown): boolean =>
  isWatcherL1TransientFailure(error) || classifyFailure(error) === "transient";

/** The detail suffix of a failure that is not retried on a timer. */
const NOT_RETRIED =
  "not transient: not retried until the follower moves or rewinds";

/**
 * A failed pass or reset, classified: whether it is retried on the timer,
 * and its detail (a failure that is not names that it waits for the
 * follower).
 */
export const decisionFailure = (
  error: unknown,
): { readonly transient: boolean; readonly detail: string } => {
  const transient = isDecisionFailureTransient(error);
  const detail = error instanceof Error ? error.message : String(error);
  return {
    transient,
    detail: transient ? detail : `${detail} (${NOT_RETRIED})`,
  };
};
