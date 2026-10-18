import { MIDGARD_CONSENSUS_PROFILE } from "@al-ft/midgard-core/consensus-profile";

import type { ImportBlockChecks } from "../mpf/verified-block-import.js";

/**
 * The open deadline of an imported transaction's check: its block's end
 * plus half the block maturity M (`block_end + M/2`). A check that has no
 * local verdict by then leaves the set with `unverified_deadline_missed`.
 */
export const importCheckDeadlineMs = (blockEndTime: bigint): number =>
  Number(blockEndTime) +
  Math.floor(MIDGARD_CONSENSUS_PROFILE.limits.blockMaturityMs / 2);

/** A foreign block's transactions its import applied by their committed
 * verdicts, and the deadline of their checks. */
export type ImportChecks = Readonly<{
  deadlineMs: number;
  checks: ImportBlockChecks;
}>;

/** The checks a foreign block ending at `blockEndTime` leaves. */
export const importChecks = (
  blockEndTime: bigint,
  checks: ImportBlockChecks,
): ImportChecks => ({
  deadlineMs: importCheckDeadlineMs(blockEndTime),
  checks,
});
