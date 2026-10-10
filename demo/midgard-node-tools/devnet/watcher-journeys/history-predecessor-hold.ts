import { Cause, Runtime } from "effect";

/** The SDK's user-event builders take `validFrom` this far behind the wall
 * clock (`userHistoryValidity`). */
const VALID_FROM_BACKOFF_MS = 60_000;
/** One ledger slot of slack for the builder's slot rounding. */
const SLOT_SLACK_MS = 1_000;
const PROTECTED_BUILD_ATTEMPTS = 3;

/** The protection end the SDK refused below, if `error` is that refusal. */
const protectedUntilOf = (error: unknown, depth = 0): bigint | undefined => {
  if (depth > 8 || typeof error !== "object" || error === null)
    return undefined;
  if (Runtime.isFiberFailure(error))
    return protectedUntilOf(
      Cause.squash(error[Runtime.FiberFailureCauseId]),
      depth + 1,
    );
  const { name, protectedUntil, cause } = error as {
    name?: unknown;
    protectedUntil?: unknown;
    cause?: unknown;
  };
  if (
    name === "EventHistoryPredecessorProtectedError" &&
    typeof protectedUntil === "bigint"
  )
    return protectedUntil;
  return protectedUntilOf(cause, depth + 1);
};

/**
 * Builds with `build`, holding while the event-history predecessor is still
 * protected. The SDK's user-event builders refuse a validity lower bound
 * below the predecessor's protection instead of waiting, as the SDK's own
 * submission loop does, so an event staged within a minute of the previous
 * one waits here until its lower bound clears the protection, then builds
 * again. The waits are on time; the attempts are bounded.
 */
export const buildPastProtectedPredecessor = async <T>(
  build: () => Promise<T>,
  clock: Readonly<{
    nowMs: () => number;
    sleep: (ms: number) => Promise<void>;
  }> = {
    nowMs: Date.now,
    sleep: (ms) => new Promise((resolve) => setTimeout(resolve, ms)),
  },
): Promise<T> => {
  for (let attempt = 1; ; attempt++) {
    try {
      return await build();
    } catch (error) {
      const until = protectedUntilOf(error);
      if (until === undefined || attempt === PROTECTED_BUILD_ATTEMPTS)
        throw error;
      await clock.sleep(
        Math.max(
          0,
          Number(until) + VALID_FROM_BACKOFF_MS + SLOT_SLACK_MS - clock.nowMs(),
        ),
      );
    }
  }
};
