import type * as SDK from "@al-ft/midgard-sdk";

import type { CommitteeL1Clock } from "../l1/follower/l1-clock.js";
import {
  type AvailabilityResponderBuildTime,
  availabilityResponderL1BuildTime,
} from "./factory.discover-availability-responder-challenges.js";
import type { committeePromiseNetworkClock } from "./promise-network-clock.js";

type PromiseNetworkClock = ReturnType<typeof committeePromiseNetworkClock>;
type BuiltResponse = Awaited<
  ReturnType<Parameters<typeof SDK.runDaAvailabilityOperation>[1]["build"]>
>;

/** The propagation allowance a promise response's TTL rule leaves, in ms. */
export const PROMISE_RESPONSE_PROPAGATION_MS = 3_000;
/** The future eligible slots a promise response's TTL must leave. */
export const PROMISE_RESPONSE_MIN_FUTURE_SLOTS = 50;
/**
 * The build/sign/persist stage allowance, in ms. The TTL rule is checked
 * again on the signed body while the stage runs, so it must still hold at
 * the stage's end.
 */
export const PROMISE_RESPONSE_BUILD_STAGE_MS = 2_000;

/**
 * Promise mode's response build time. The validity upper bound is picked from
 * the adopted promise network clock's upper bound on the current network time
 * (`upperTimeMs(0)`: the SDK window's `nowMs` is "now"; the TTL rule adds its
 * own propagation allowance), so the TTL clears the fifty-slot rule however
 * far the follower tip trails the wall clock. The lower bound stays anchored
 * to the observed tip, and every deadline decision stays on the committee's
 * L1 clock (plan §3.6: a wall-derived clock may pick valid_to, never decide
 * due, expired or deadline).
 *
 * An interval anchored on a lagging tip ends sooner than the lookahead, so
 * it is taken only while its TTL still clears the rule at the build stage's
 * end: `eligibleFutureSlots(ttl, propagation) >= min` holds once `ttl` is
 * past `upperReadySlot(propagation)` by `min` slots, and that slot is read
 * the stage allowance ahead, plus the clock's wall/monotonic drift bound: a
 * wall step inside that bound during the stage moves the upper bound with
 * it. Short of that room the window stays not due, and the step waits for
 * the next block as before. With one-second slots and the 3 s propagation,
 * 2 s stage and 2 s drift allowances, the room is 58 slots: the 240 s range
 * covers a tip up to 178 slots behind the clock with the full lookahead, and
 * up to 180 anchored on the tip.
 */
export const promiseResponseBuildTime =
  (
    l1Clock: CommitteeL1Clock,
    clock: Pick<
      PromiseNetworkClock,
      "upperTimeMs" | "upperReadySlot" | "maxWallMonotonicDriftMs"
    >,
  ) =>
  (): AvailabilityResponderBuildTime => {
    // Read before `validToNowMs`, so the room is never short of it.
    const nowSlot = clock.upperReadySlot(0);
    const validToNowMs = clock.upperTimeMs(0);
    const leastTtl =
      clock.upperReadySlot(
        PROMISE_RESPONSE_PROPAGATION_MS +
          PROMISE_RESPONSE_BUILD_STAGE_MS +
          clock.maxWallMonotonicDriftMs,
      ) +
      1 +
      PROMISE_RESPONSE_MIN_FUTURE_SLOTS;
    return {
      ...availabilityResponderL1BuildTime(l1Clock),
      validToNowMs,
      minimumRoomSlots: leastTtl - nowSlot,
    };
  };

/** A promise response's exclusive TTL, once it leaves at least fifty future
 * eligible slots after the three-second propagation allowance. */
export const promiseResponseTtl = (
  built: BuiltResponse,
  clock: Pick<PromiseNetworkClock, "eligibleFutureSlots">,
): number => {
  const tx = "toTransaction" in built ? built : built.tx;
  const ttl = tx.toTransaction().body().ttl();
  if (ttl === undefined) throw new Error("Built response has no exclusive TTL");
  const builtTtl = Number(ttl);
  if (
    !Number.isSafeInteger(builtTtl) ||
    clock.eligibleFutureSlots(builtTtl, PROMISE_RESPONSE_PROPAGATION_MS) <
      PROMISE_RESPONSE_MIN_FUTURE_SLOTS
  )
    throw new Error("Built response TTL has insufficient future slots");
  return builtTtl;
};
