import type { LucidEvolution } from "@lucid-evolution/lucid";

/** The network slot clock Lucid uses to turn validity bounds into slots. */
export type SlotClock = Pick<
  LucidEvolution,
  "unixTimeToSlot" | "slotToUnixTime"
>;

/**
 * The earliest slot-boundary time at or after `notBeforeMs`.
 *
 * The ledger carries a validity lower bound as a slot, and Lucid's
 * `validFrom(ms)` converts with the enclosing (floored) slot, so the
 * inclusive lower bound a validator sees is the start of that slot. A
 * threshold that is not on a slot boundary, such as a deadline derived from a
 * block end time ending in 999 ms, is therefore presented up to one slot
 * early and a validator's `lower_bound >= threshold` check refuses it. Using
 * this value as `validFrom` makes the on-chain lower bound the smallest one
 * that still satisfies such a check.
 */
export const slotAlignedLowerBoundAtOrAfter = (
  slotClock: SlotClock,
  notBeforeMs: bigint,
): bigint => {
  const unixTime = Number(notBeforeMs);
  if (!Number.isSafeInteger(unixTime)) {
    throw new Error(
      `Cannot slot-align an unsafe validity lower bound ${notBeforeMs.toString()}`,
    );
  }
  const enclosingSlot = slotClock.unixTimeToSlot(unixTime);
  const enclosingSlotStart = slotClock.slotToUnixTime(enclosingSlot);
  const aligned =
    enclosingSlotStart >= unixTime
      ? enclosingSlotStart
      : slotClock.slotToUnixTime(enclosingSlot + 1);
  if (
    !Number.isSafeInteger(enclosingSlot) ||
    !Number.isSafeInteger(aligned) ||
    aligned < unixTime
  ) {
    throw new Error(
      `Slot clock returned an invalid boundary for validity lower bound ${notBeforeMs.toString()}`,
    );
  }
  return BigInt(aligned);
};

/**
 * The latest slot-boundary time at or before `notAfterMs`.
 *
 * Lucid's `validTo(ms)` also floors to the enclosing slot, and the ledger's
 * upper bound is exclusive, so the inclusive upper bound a validator sees is
 * `slotAlignedUpperBoundAtOrBefore(ms) - 1`, not `ms - 1`. A builder that
 * records the inclusive upper bound in a datum the validator compares for
 * equality must use this value as `validTo`.
 */
export const slotAlignedUpperBoundAtOrBefore = (
  slotClock: SlotClock,
  notAfterMs: bigint,
): bigint => {
  const unixTime = Number(notAfterMs);
  if (!Number.isSafeInteger(unixTime)) {
    throw new Error(
      `Cannot slot-align an unsafe validity upper bound ${notAfterMs.toString()}`,
    );
  }
  const enclosingSlot = slotClock.unixTimeToSlot(unixTime);
  const aligned = slotClock.slotToUnixTime(enclosingSlot);
  if (
    !Number.isSafeInteger(enclosingSlot) ||
    !Number.isSafeInteger(aligned) ||
    aligned > unixTime
  ) {
    throw new Error(
      `Slot clock returned an invalid boundary for validity upper bound ${notAfterMs.toString()}`,
    );
  }
  return BigInt(aligned);
};
