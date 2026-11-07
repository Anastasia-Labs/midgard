import { L1SubmitOutcomeUnknownError } from "@al-ft/midgard-l1-follower/provider";
import { Cause, Runtime } from "effect";

/**
 * The {@link L1SubmitOutcomeUnknownError} on `error`'s cause chain, or
 * `undefined`. The chain covers `cause` links and what an Effect
 * `FiberFailure` or `Cause` carries: Lucid's `submit()` rejects with a
 * FiberFailure around its `TxSubmitError`, whose cause is the provider's.
 * Matched by name too, so a second loaded copy of the follower package is
 * still recognised.
 */
export const findSubmitOutcomeUnknown = (
  error: unknown,
): L1SubmitOutcomeUnknownError | undefined => {
  const seen = new Set<unknown>();
  const pending: unknown[] = [error];
  while (pending.length > 0 && seen.size < 64) {
    const next = pending.shift();
    if (Cause.isCause(next)) {
      pending.push(...Cause.failures(next), ...Cause.defects(next));
      continue;
    }
    if (typeof next !== "object" || next === null || seen.has(next)) continue;
    seen.add(next);
    if (
      next instanceof L1SubmitOutcomeUnknownError ||
      ((next as { readonly name?: unknown }).name ===
        "L1SubmitOutcomeUnknownError" &&
        (next as { readonly outcomeUnknown?: unknown }).outcomeUnknown === true)
    )
      return next as L1SubmitOutcomeUnknownError;
    if (Runtime.isFiberFailure(next))
      pending.push(next[Runtime.FiberFailureCauseId]);
    pending.push((next as { readonly cause?: unknown }).cause);
  }
  return undefined;
};
