/**
 * Validity bounds measured from the observed ledger tip. The ledger judges a
 * transaction's lower bound against the slot after its tip, and the tip moves
 * only when a block lands, so between blocks the wall clock runs ahead of it.
 * A lower bound taken from the wall clock can then be past the tip, and the
 * ledger refuses the transaction as outside its validity interval. These
 * helpers take the lower bound from the tip instead, on a Lucid client's slot
 * mapping.
 */
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { SubmitSlotSnapshot } from "./ogmios-slot.parse-ogmios-evidence.js";

/**
 * The slot a validity lower bound is judged at: the ledger's latest block.
 * The mempool checks `invalidBefore` against the slot after that tip, so a
 * wall-clock slot (which runs ahead of the tip between blocks) would call a
 * transaction due while the ledger still refuses it. A snapshot without a
 * ledger tip (emulator, tests) has no block gaps: its current slot is the tip.
 */
export const submitLowerBoundSlot = (
  snapshot: Pick<SubmitSlotSnapshot, "currentSlot" | "ledgerTipSlot">,
): number => snapshot.ledgerTipSlot ?? snapshot.currentSlot;

/** The start of the slot `unixTime` falls in, on the client's slot mapping. */
export const alignUnixTimeToSlotBoundary = (
  lucid: LucidEvolution,
  unixTime: number,
): number => {
  if (!Number.isSafeInteger(unixTime)) {
    throw new Error(`Cannot align invalid unix time ${String(unixTime)}`);
  }
  const slot = Number(lucid.unixTimeToSlot(unixTime));
  const aligned = lucid.slotToUnixTime(slot);
  if (!Number.isSafeInteger(slot) || !Number.isSafeInteger(aligned)) {
    throw new Error(
      `Lucid returned an invalid slot boundary for unix time ${unixTime.toString()}`,
    );
  }
  return aligned;
};

/** The first slot start strictly after `unixTimeExclusive`. */
export const alignedUnixTimeStrictlyAfter = (
  lucid: LucidEvolution,
  unixTimeExclusive: number,
): number => {
  const aligned = alignUnixTimeToSlotBoundary(lucid, unixTimeExclusive);
  if (aligned > unixTimeExclusive) {
    return aligned;
  }
  const slot = Number(lucid.unixTimeToSlot(unixTimeExclusive));
  const strictlyAfter = lucid.slotToUnixTime(slot + 1);
  if (!Number.isSafeInteger(strictlyAfter)) {
    throw new Error(
      `Lucid returned an invalid slot boundary after unix time ${unixTimeExclusive.toString()}`,
    );
  }
  return strictlyAfter;
};

/**
 * The start of the observed ledger tip's slot, in ms: the time a lower bound
 * that must already be open is measured from. A snapshot without a ledger tip
 * (emulator, tests) has no block gaps: its current slot is the tip.
 */
export const ledgerTipStartMs = (
  lucid: LucidEvolution,
  submitSlotSnapshot: Pick<SubmitSlotSnapshot, "currentSlot" | "ledgerTipSlot">,
): number => {
  const startMs = lucid.slotToUnixTime(
    submitLowerBoundSlot(submitSlotSnapshot),
  );
  if (!Number.isSafeInteger(startMs)) {
    throw new Error(`Ledger-tip slot start is invalid: ${String(startMs)}`);
  }
  return startMs;
};

/**
 * A validity lower bound `backdateMs` before the observed ledger tip, on a
 * slot boundary and never before the first slot the client's slot config
 * expresses (its zeroSlot; an emulator client's is the slot it was created
 * at). The ledger tip, not the wall clock: between blocks the wall clock
 * runs ahead of the tip, and the mempool refuses a lower bound past the tip
 * as outside its validity interval. With `rangeFloorMs` (the upper bound less the longest range the
 * validator accepts) the bound is never before that floor either, so the
 * range stays within the limit even when the tip trails far behind.
 */
export const ledgerTipBackdatedValidFromMs = ({
  lucid,
  submitSlotSnapshot,
  backdateMs,
  rangeFloorMs,
}: {
  readonly lucid: LucidEvolution;
  readonly submitSlotSnapshot: Pick<
    SubmitSlotSnapshot,
    "currentSlot" | "ledgerTipSlot"
  >;
  readonly backdateMs: number;
  readonly rangeFloorMs?: number;
}): number => {
  // The first slot the client's slot config expresses (its zeroSlot): the
  // evaluator refuses a bound before it as too far in the past.
  const firstSlotStartMs =
    (typeof lucid.config === "function"
      ? lucid.config().slotConfig?.zeroTime
      : undefined) ?? 0;
  const backdatedMs = Math.max(
    firstSlotStartMs,
    ledgerTipStartMs(lucid, submitSlotSnapshot) - backdateMs,
  );
  if (rangeFloorMs === undefined) {
    return alignUnixTimeToSlotBoundary(lucid, backdatedMs);
  }
  const validFromMs = alignUnixTimeToSlotBoundary(
    lucid,
    Math.max(backdatedMs, rangeFloorMs),
  );
  return validFromMs < rangeFloorMs
    ? alignedUnixTimeStrictlyAfter(lucid, rangeFloorMs)
    : validFromMs;
};
