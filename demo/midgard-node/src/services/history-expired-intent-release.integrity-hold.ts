import { Cause, Effect, Ref } from "effect";

import { findSignedIntentReplacementIntegrityError } from "./canonical-journal-recovery.js";
import { Globals } from "./globals.js";
import {
  NativeRecoveryRootRefused,
  RetainedReplacementPlanChanged,
} from "./history-expired-intent-release.retained-journal-digest.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
  SIGNED_INTENT_REPLACEMENT_INTEGRITY,
} from "./liveness-halt.js";

/** Clears `reason` under `source` only if it is the reason raised there, so
 * one evaluation never clears another's reason under a shared source. */
export const clearLivenessReasonIf = (
  globals: Globals,
  source: string,
  reason: string,
) =>
  Effect.flatMap(Ref.get(globals.LIVENESS_REASONS), (reasons) =>
    reasons.get(source) === reason
      ? clearLivenessIncident(globals, source)
      : Effect.void,
  );

/** The held failure a cause carries: a replacement integrity failure however
 * deeply wrapped, a retained replacement plan that binds another journal, or
 * a native durable root the replacement plan refuses. */
const heldFailure = (cause: Cause.Cause<unknown>) =>
  findSignedIntentReplacementIntegrityError(cause) ??
  [...Cause.failures(cause)].find(
    (
      value,
    ): value is RetainedReplacementPlanChanged | NativeRecoveryRootRefused =>
      value instanceof RetainedReplacementPlanChanged ||
      value instanceof NativeRecoveryRootRefused,
  );

/**
 * A history recovery preparation (signed-intent release or replaced-block
 * revival) that meets a `SignedIntentReplacementIntegrityError` (or a
 * `RetainedReplacementPlanChanged`, or a `NativeRecoveryRootRefused`) holds instead of failing the history
 * owner, whose supervisor would only restart it into the same evidence: the
 * error is raised once as `signed_intent_replacement_integrity` under
 * `source`, which readiness reports, and the preparation completes. The error
 * is only ever raised before or inside an SQL transaction, which it rolls
 * back; a retained plan whose native CAS already ran stays retained and is
 * resumed by the next evaluation, as after any failed attempt. Its
 * disposition stays pending, so the history gate stays closed and block
 * production with it, and every later evaluation re-derives it from fresh
 * evidence; no winner is ever chosen here. A later completion that meets no
 * integrity failure clears it, as does its runtime once no release or
 * revival is in question. Any other failure propagates unchanged.
 */
export const heldOnIntegrityFailure =
  (source: string) =>
  <A, E, R>(work: Effect.Effect<A, E, R>) =>
    Effect.flatMap(Globals, (globals) =>
      work.pipe(
        Effect.tap(() =>
          clearLivenessReasonIf(
            globals,
            source,
            SIGNED_INTENT_REPLACEMENT_INTEGRITY,
          ),
        ),
        Effect.catchAllCause((cause) => {
          const integrity = heldFailure(cause);
          return integrity === undefined
            ? Effect.failCause(cause)
            : raiseLivenessIncident(
                globals,
                source,
                SIGNED_INTENT_REPLACEMENT_INTEGRITY,
                `${integrity.message} The history gate stays closed, which holds block production; the intent stays in place and every evaluation re-derives it from the evidence. Operator action is needed if the evidence does not change.`,
                { escalateAfterMs: 0 },
              );
        }),
      ),
    );
