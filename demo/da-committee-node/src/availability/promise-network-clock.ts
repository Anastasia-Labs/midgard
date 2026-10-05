import type { SlotConfig } from "@lucid-evolution/lucid";

/** Bound by the configured source's freshly authenticated fixed genesis.
 * Initial absolute skew is an adopted owner assumption, not measured by drift. */
export const committeePromiseNetworkClock = (
  input: Readonly<{
    slotConfig: SlotConfig;
    initialProducerMemberSkewMs: number;
    maxWallMonotonicDriftMs: number;
    nowMs: () => number;
    monotonicMs: () => number;
  }>,
) => {
  const natural = (value: number) => Number.isSafeInteger(value) && value >= 0;
  const mapping = Object.freeze({ ...input.slotConfig });
  const initialWall = input.nowMs(),
    initialMono = input.monotonicMs();
  if (
    !natural(mapping.zeroTime) ||
    !natural(mapping.zeroSlot) ||
    !natural(mapping.slotLength) ||
    mapping.slotLength === 0 ||
    !natural(initialWall) ||
    !Number.isFinite(initialMono) ||
    !natural(input.initialProducerMemberSkewMs) ||
    !natural(input.maxWallMonotonicDriftMs)
  )
    throw new Error("Promise network clock authority is incomplete");
  let fault: string | undefined;
  const upperTimeMs = (remainingPropagationMs: number): number => {
    const wall = input.nowMs(),
      mono = input.monotonicMs();
    if (
      !natural(wall) ||
      !Number.isFinite(mono) ||
      mono < initialMono ||
      Math.abs(wall - initialWall - (mono - initialMono)) >
        input.maxWallMonotonicDriftMs
    )
      fault = "Promise network clock drift invalidated the adopted capability";
    if (fault) throw new Error(fault);
    if (!natural(remainingPropagationMs))
      throw new Error("Promise propagation allowance is invalid");
    const upper =
      Math.ceil(Math.max(wall, initialWall + mono - initialMono)) +
      input.initialProducerMemberSkewMs +
      remainingPropagationMs;
    if (!natural(upper) || upper < mapping.zeroTime)
      throw new Error(
        "Promise current network time is outside the fixed genesis mapping",
      );
    return upper;
  };
  const upperReadySlot = (remainingPropagationMs: number): number => {
    const slot =
      mapping.zeroSlot +
      Math.floor(
        (upperTimeMs(remainingPropagationMs) - mapping.zeroTime) /
          mapping.slotLength,
      );
    if (!natural(slot)) throw new Error("Promise ready slot overflows");
    return slot;
  };
  return Object.freeze({
    upperTimeMs,
    upperReadySlot,
    eligibleFutureSlots: (
      exclusiveTtl: number,
      remainingPropagationMs: number,
    ): number => {
      if (!natural(exclusiveTtl))
        throw new Error("Promise exclusive TTL is invalid");
      return Math.max(
        0,
        exclusiveTtl - upperReadySlot(remainingPropagationMs) - 1,
      );
    },
    remainingHorizonSlots: (
      exclusiveDeadlineMs: number,
      remainingPropagationMs: number,
    ): number => {
      if (!natural(exclusiveDeadlineMs))
        throw new Error("Promise deadline is invalid");
      return Math.max(
        0,
        Math.floor(
          (exclusiveDeadlineMs - upperTimeMs(remainingPropagationMs)) /
            mapping.slotLength,
        ),
      );
    },
  });
};
