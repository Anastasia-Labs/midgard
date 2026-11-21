/**
 * Why a responder transaction was not built: the validity window
 * (`SDK.daAvailabilityValidityWindow`, on the committee's L1 clock) is not
 * admissible yet, or no longer.
 *
 * - `ledger_tip_behind`: the window is `not_due`. The ledger tip has not
 *   passed the interval's lower bound by the submit margin, so the ledger
 *   would refuse it; a later block makes it due.
 * - `no_validity_interval`: the window is `refused` with `no_interval`; the
 *   action's lower limit leaves no room before its upper bound yet.
 * - `deadline_passed`: a publication window is `refused` with
 *   `window_closed`: its upper bound (`response_deadline + 1`) does not reach
 *   past the L1 clock, so no answer can land any more.
 *
 * It is thrown before anything is signed or journaled. The responder reports
 * the first two as `pending` and the third as a missed deadline, never as a
 * failure.
 */
export type AvailabilityResponderValidityDeferral =
  | "ledger_tip_behind"
  | "no_validity_interval"
  | "deadline_passed";

export class AvailabilityResponderValidityDeferredError extends Error {
  constructor(
    readonly reason: AvailabilityResponderValidityDeferral,
    detail: string,
  ) {
    super(
      `Availability responder defers its transaction (${reason}): ${detail}`,
    );
    this.name = "AvailabilityResponderValidityDeferredError";
  }
}
