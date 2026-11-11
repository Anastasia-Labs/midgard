import {
  type JourneyTiming,
  MAX_TIMER_MS,
} from "./journey-timing.journey-timing-for-plan.js";
import { OPERATOR_RESUME_WAIT_MS } from "./operator-pause.js";

/**
 * What a family's staging shift cover is computed from on an attached
 * stack: its transaction allowance at the journey's action depth, the
 * deployment's event-history protection, and the deployment profile's event
 * wait and operator shift.
 */
export type StackShiftCoverInput = Readonly<{
  timing: Pick<JourneyTiming, "transactionAllowanceMs">;
  /** Each event history's protection duration, as its recipe carries it. */
  eventHistoryProtectionMs: Readonly<{ deposit: number; withdrawal: number }>;
  profile: Readonly<{ event_wait_ms: number; operator_shift_ms: number }>;
}>;

/**
 * A family's whole time limit on an attached stack (`journey.test.ts`): its
 * computed budget, plus the longest operator-2 waits for its shift and then
 * holds it. The drill stages only in operator-2's shift, which must hold the
 * family's staging shift cover C (`awaitOperatorShift`). At worst operator-2
 * still holds a shift with just under C left: it waits that out, then our
 * node's whole shift S, then holds C of its next shift. That is S + 2C. A
 * refusal-only family's age-out (two attestation timeouts) fits in the
 * budget it replaces.
 */
export const stackJourneyTimeoutMs = (
  timing: Pick<JourneyTiming, "journeyTimeoutMs">,
  profile: Readonly<{ operator_shift_ms: number }>,
  stagingCoverMs: number,
) => {
  if (
    !Number.isSafeInteger(stagingCoverMs) ||
    stagingCoverMs <= 0 ||
    stagingCoverMs > profile.operator_shift_ms
  )
    throw new Error(
      `A staging shift cover of ${stagingCoverMs.toString()} ms does not fit one ${profile.operator_shift_ms.toString()} ms shift`,
    );
  const limitMs =
    timing.journeyTimeoutMs + profile.operator_shift_ms + 2 * stagingCoverMs;
  if (!Number.isSafeInteger(limitMs) || limitMs > MAX_TIMER_MS)
    throw new Error(
      `A stack journey limit of ${limitMs.toString()} ms exceeds the supported timer range`,
    );
  return limitMs;
};

/**
 * Every family's time limit for one attached-stack run, in run order: each
 * family's own limit (`stackJourneyTimeoutMs`), and for the last, which alone
 * resumes operator-2's node (`journey.test.ts`), the longest that resume
 * takes (`OPERATOR_RESUME_WAIT_MS`).
 */
export const stackRunTimeoutsMs = (familyLimitsMs: readonly number[]) =>
  familyLimitsMs.map((limitMs, index) => {
    if (index < familyLimitsMs.length - 1) return limitMs;
    const withResumeMs = limitMs + OPERATOR_RESUME_WAIT_MS;
    if (!Number.isSafeInteger(withResumeMs) || withResumeMs > MAX_TIMER_MS)
      throw new Error(
        `A stack journey limit of ${withResumeMs.toString()} ms exceeds the supported timer range`,
      );
    return withResumeMs;
  });
