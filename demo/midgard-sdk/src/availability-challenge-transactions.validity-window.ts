import {
  alignedUnixTimeStrictlyAfter,
  alignUnixTimeToSlotBoundary,
  ledgerTipBackdatedValidFromMs,
  SUBMIT_SLOT_VALIDITY_BUFFER,
  submitLowerBoundSlot,
  type SubmitSlotSnapshot,
} from "@al-ft/midgard-core/ogmios-slot";
import { type LucidEvolution } from "@lucid-evolution/lucid";

import { DA_AVAILABILITY_MAX_VALIDITY_RANGE_MS } from "./availability-challenge-transactions.at.js";

/** How far before the observed ledger tip a lower bound starts. */
export const DA_AVAILABILITY_VALIDITY_BACKDATE_MS = 60_000;
/** How far past the L1 clock an upper bound runs, unless a window ends first. */
export const DA_AVAILABILITY_VALIDITY_LOOKAHEAD_MS = 60_000;

/**
 * The validity window an availability action's validator allows, in ms:
 *
 * - Open: `validTo <= node.block_time + da_challenge_window_ms` (its inclusive
 *   upper bound is before the window's end, availability-challenge-validation
 *   `validate_open_challenge`);
 * - Publish: `validTo <= response_deadline + 1` (inclusive upper bound at or
 *   before the deadline, `validate_advance_tranche`);
 * - Settle of an expired tranche, and Timeout with the correction lock it
 *   acquires: `validFrom >= response_deadline` (`validate_settlement_status`,
 *   `validate_timeout_challenge`, correction-lock acquire).
 */
export type DaAvailabilityValidityLimits = Readonly<{
  /** The least lower bound (`validFrom`) the action accepts. */
  lowerLimitMs?: bigint;
  /** The greatest exclusive upper bound (`validTo`) the action accepts. */
  upperLimitMs?: bigint;
}>;

/** Why no admissible interval exists, at any tip. */
export type DaAvailabilityValidityRefusal =
  /** The upper bound does not reach past the L1 clock: no block can include it. */
  | "window_closed"
  /** The lower limit is at or past the upper bound. */
  | "no_interval";

export type DaAvailabilityValidityWindow =
  /** The interval is admissible at the observed ledger tip. */
  | Readonly<{ status: "ready"; validFrom: bigint; validTo: bigint }>
  /**
   * The ledger tip has not reached the lower bound by the submit margin: the
   * transaction would be refused until a block at `dueSlot` or later lands.
   */
  | Readonly<{
      status: "not_due";
      validFrom: bigint;
      validTo: bigint;
      tipSlot: number;
      dueSlot: number;
    }>
  | Readonly<{
      status: "refused";
      reason: DaAvailabilityValidityRefusal;
      detail: string;
    }>;

/**
 * The validity interval of an availability transaction, measured from the
 * observed ledger tip. The ledger judges a lower bound against the slot after
 * its tip, which moves only when a block lands, so a lower bound taken from
 * the wall clock is refused whenever the last block trails it.
 *
 * The upper bound runs `DA_AVAILABILITY_VALIDITY_LOOKAHEAD_MS` past the L1
 * clock (`nowMs`), never past `upperLimitMs`, on a slot boundary. The lower
 * bound is backdated `DA_AVAILABILITY_VALIDITY_BACKDATE_MS` from the tip,
 * never so early that the interval outgrows
 * `DA_AVAILABILITY_MAX_VALIDITY_RANGE_MS`, and never before `lowerLimitMs`.
 * The interval is `ready` once the tip passes the lower bound by the submit
 * margin (`SUBMIT_SLOT_VALIDITY_BUFFER` slots), and `not_due` before: a
 * one-shot command waits for the tip and asks again, a long-running actor
 * defers the step. An interval whose upper bound does not reach past the L1
 * clock, or whose limits leave no room, is `refused`.
 */
export const daAvailabilityValidityWindow = (input: {
  readonly lucid: LucidEvolution;
  /** The observed ledger tip. */
  readonly tip: Pick<SubmitSlotSnapshot, "currentSlot" | "ledgerTipSlot">;
  /** The L1 clock, in ms. */
  readonly nowMs: number;
  readonly limits?: DaAvailabilityValidityLimits;
}): DaAvailabilityValidityWindow => {
  const { lucid, tip, nowMs, limits = {} } = input;
  const lookaheadMs = nowMs + DA_AVAILABILITY_VALIDITY_LOOKAHEAD_MS;
  const validTo = alignUnixTimeToSlotBoundary(
    lucid,
    limits.upperLimitMs === undefined
      ? lookaheadMs
      : Math.min(lookaheadMs, Number(limits.upperLimitMs)),
  );
  if (validTo <= nowMs)
    return {
      status: "refused",
      reason: "window_closed",
      detail: `the validity window closed before the L1 clock (valid_to_ms=${validTo.toString()},l1_now_ms=${nowMs.toString()})`,
    };
  const tipBackdated = ledgerTipBackdatedValidFromMs({
    lucid,
    submitSlotSnapshot: tip,
    backdateMs: DA_AVAILABILITY_VALIDITY_BACKDATE_MS,
    rangeFloorMs: validTo - Number(DA_AVAILABILITY_MAX_VALIDITY_RANGE_MS),
  });
  const validFrom =
    limits.lowerLimitMs === undefined
      ? tipBackdated
      : Math.max(
          tipBackdated,
          alignedUnixTimeStrictlyAfter(lucid, Number(limits.lowerLimitMs) - 1),
        );
  if (validFrom >= validTo)
    return {
      status: "refused",
      reason: "no_interval",
      detail: `the validity window leaves no interval (valid_from_ms=${validFrom.toString()},valid_to_ms=${validTo.toString()})`,
    };
  const tipSlot = submitLowerBoundSlot(tip);
  const dueSlot =
    Number(lucid.unixTimeToSlot(validFrom)) + SUBMIT_SLOT_VALIDITY_BUFFER;
  return tipSlot >= dueSlot
    ? {
        status: "ready",
        validFrom: BigInt(validFrom),
        validTo: BigInt(validTo),
      }
    : {
        status: "not_due",
        validFrom: BigInt(validFrom),
        validTo: BigInt(validTo),
        tipSlot,
        dueSlot,
      };
};
