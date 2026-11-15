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

/**
 * Promise mode's response build time. The validity upper bound is picked from
 * the adopted promise network clock's upper bound on the current network time
 * (`upperTimeMs(0)`: the SDK window's `nowMs` is "now"; the TTL rule adds its
 * own propagation allowance), so the TTL clears the fifty-slot rule however
 * far the follower tip trails the wall clock. The lower bound stays anchored
 * to the observed tip, and every deadline decision stays on the committee's
 * L1 clock (plan §3.6: a wall-derived clock may pick valid_to, never decide
 * due, expired or deadline).
 */
export const promiseResponseBuildTime =
  (
    l1Clock: CommitteeL1Clock,
    clock: Pick<PromiseNetworkClock, "upperTimeMs">,
  ) =>
  (): AvailabilityResponderBuildTime => ({
    ...availabilityResponderL1BuildTime(l1Clock),
    validToNowMs: clock.upperTimeMs(0),
  });

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
    clock.eligibleFutureSlots(builtTtl, 3000) < 50
  )
    throw new Error("Built response TTL has insufficient future slots");
  return builtTtl;
};
