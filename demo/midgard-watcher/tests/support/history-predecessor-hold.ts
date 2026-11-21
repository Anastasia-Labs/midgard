import { Cause, Runtime } from "effect";

import { FRESH_TIP_MS } from "./published-block-actor.is-unanswered-submission.js";

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
    /**
     * Waits until the ledger tip is recent enough for the builders'
     * backdated lower bound; the wall clock alone cannot tell.
     */
    awaitFreshTip?: () => Promise<void>;
  }> = {
    nowMs: Date.now,
    sleep: (ms) => new Promise((resolve) => setTimeout(resolve, ms)),
  },
): Promise<T> => {
  for (let attempt = 1; ; attempt++) {
    try {
      await clock.awaitFreshTip?.();
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

/**
 * The longest `buildPastProtectedPredecessor` holds an event's build after
 * its history predecessor's validity ends: the predecessor stays protected
 * for `protectionMs`, then the build waits out its lower bound's backoff and
 * one slot of slack.
 */
export const protectedPredecessorHoldMs = (protectionMs: number) => {
  if (!Number.isSafeInteger(protectionMs) || protectionMs < 0)
    throw new Error(
      `History protection is a whole number of milliseconds, not ${protectionMs.toString()}`,
    );
  return protectionMs + VALID_FROM_BACKOFF_MS + SLOT_SLACK_MS;
};

/**
 * The hold's clock on a chain's ledger time, for emulator and live chains.
 * Each build first waits for a tip no older than `FRESH_TIP_MS`: a block gap
 * longer than the builders' sixty-second backdating would otherwise leave
 * the lower bound ahead of the tip, and the node would refuse the
 * transaction.
 */
export const ledgerClock = (
  chain: Readonly<{
    now: () => number;
    awaitLedgerTime: (targetUnixTimeMs: number) => void | Promise<void>;
  }>,
) => ({
  nowMs: () => chain.now(),
  sleep: async (ms: number) => {
    await chain.awaitLedgerTime(chain.now() + ms);
  },
  awaitFreshTip: async () => {
    await chain.awaitLedgerTime(chain.now() - FRESH_TIP_MS);
  },
});
