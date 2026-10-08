import { Cause, Effect } from "effect";

import { findSignedIntentReplacementIntegrityError } from "./canonical-journal-recovery.js";
import { Globals } from "./globals.js";
import {
  clearNativeRestoreRefusal,
  holdNativeRestoreRefusal,
} from "./history-dependent-recovery.js";
import {
  NativeRecoveryRootRefused,
  RetainedReplacementPlanChanged,
} from "./history-expired-intent-release.retained-journal-digest.js";
import { SignedIntentJournalUnbound } from "./history-expired-intent-release.signed-commit-node.js";
import {
  clearLivenessReasonIf,
  raiseLivenessIncident,
  SIGNED_INTENT_JOURNAL_UNBOUND,
  SIGNED_INTENT_REPLACEMENT_INTEGRITY,
  SIGNED_INTENT_TARGET_ROOT_NOT_RETAINED,
} from "./liveness-halt.js";

/** The held failure a cause carries: a replacement integrity failure however
 * deeply wrapped, a retained replacement plan that binds another journal, a
 * native durable root the replacement plan refuses, or a journal whose
 * replay base nothing binds. */
const heldFailure = (cause: Cause.Cause<unknown>) =>
  findSignedIntentReplacementIntegrityError(cause) ??
  [...Cause.failures(cause)].find(
    (
      value,
    ): value is
      | RetainedReplacementPlanChanged
      | NativeRecoveryRootRefused
      | SignedIntentJournalUnbound =>
      value instanceof RetainedReplacementPlanChanged ||
      value instanceof NativeRecoveryRootRefused ||
      value instanceof SignedIntentJournalUnbound,
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
 * revival is in question. A `SignedIntentJournalUnbound` holds the same way
 * under `signed_intent_journal_unbound`, raised before anything is read from
 * L1 or written, and clears the same way. A native restore the owner
 * refuses (on the failure's cause chain) holds the same way, by cause (see
 * `holdNativeRestoreRefusal`): `NativeMpfRootNotRetained` under
 * `signed_intent_target_root_not_retained`, `NativeMpfFullIndexCapExceeded`
 * under `native_mpf_restore_index_cap_exceeded` and
 * `NativeMpfRestoreReadFailed` under `native_mpf_restore_read_transient`:
 * the native owner refuses before it changes its marker and before the SQL
 * transaction opens, so the plan stays retained and every evaluation
 * retries the restore; each clears the same way. Any other failure
 * propagates unchanged.
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
          ).pipe(
            Effect.zipRight(
              clearLivenessReasonIf(
                globals,
                source,
                SIGNED_INTENT_JOURNAL_UNBOUND,
              ),
            ),
            Effect.zipRight(
              clearNativeRestoreRefusal(
                globals,
                source,
                SIGNED_INTENT_TARGET_ROOT_NOT_RETAINED,
              ),
            ),
          ),
        ),
        Effect.catchAllCause((cause) => {
          const integrity = heldFailure(cause);
          if (integrity === undefined)
            return (
              holdNativeRestoreRefusal({
                globals,
                source,
                notRetainedReason: SIGNED_INTENT_TARGET_ROOT_NOT_RETAINED,
                subject: "recovery",
                cause,
              }) ?? Effect.failCause(cause)
            );
          return integrity instanceof SignedIntentJournalUnbound
            ? raiseLivenessIncident(
                globals,
                source,
                SIGNED_INTENT_JOURNAL_UNBOUND,
                `${integrity.message} The recovery holds with native MPF, the SQL root and the journal unchanged, and the history gate stays closed, which holds block production; every evaluation re-reads the journal. Operator action is needed if the journal does not change.`,
                { escalateAfterMs: 0 },
              )
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
