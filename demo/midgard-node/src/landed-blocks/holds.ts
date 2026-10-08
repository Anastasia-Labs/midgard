/**
 * The `/readyz` reasons landed-block processing holds (plan §7.3, N3). Each
 * keeps the process up: the driver retries on its backoff, and the reason
 * clears once the condition does.
 */
import type { DriverHold } from "../l1-events/driver.js";

/** A landed block does not replay to its header, or does not link to its parent. Never adopted. */
export const LANDED_BLOCK_INVALID = "landed_block_invalid";
/** A landed block is this node's, but its journal was abandoned; it must be revived first. */
export const LANDED_BLOCK_OWN_JOURNAL_ABANDONED =
  "landed_block_own_journal_abandoned";
/** A foreign block's DA payload or one of its events is not available yet. */
export const LANDED_BLOCK_AWAITING_DA = "landed_block_awaiting_da";
/** Replaying or recording a landed block failed for a reason it could not classify. */
export const LANDED_BLOCK_REPLAY_FAILED = "landed_block_replay_failed";
/** The history owner is recovering; the write waits for it. */
export const LANDED_BLOCKS_WAITING = "landed_blocks_waiting";
/** `confirmed_ledger` cannot reach the merged queue root yet. */
export const CONFIRMED_LEDGER_BEHIND = "confirmed_ledger_behind";
/** The working ledger and native MPF wait for the rebase onto the processed blocks. */
export const LANDED_BLOCK_REBASE_PENDING = "landed_block_rebase_pending";

const PRIORITY = [
  LANDED_BLOCK_INVALID,
  LANDED_BLOCK_OWN_JOURNAL_ABANDONED,
  LANDED_BLOCK_AWAITING_DA,
  LANDED_BLOCK_REPLAY_FAILED,
  LANDED_BLOCKS_WAITING,
  CONFIRMED_LEDGER_BEHIND,
  LANDED_BLOCK_REBASE_PENDING,
] as const;

const DETAIL_LIMIT = 1_000;

/** The first hold by priority, with every other hold named in its detail. */
export const combineHolds = (
  holds: readonly DriverHold[],
): DriverHold | undefined => {
  const ordered = [...holds].sort(
    (left, right) =>
      PRIORITY.indexOf(left.reason as (typeof PRIORITY)[number]) -
      PRIORITY.indexOf(right.reason as (typeof PRIORITY)[number]),
  );
  const [first, ...rest] = ordered;
  if (first === undefined) return undefined;
  const detail = [
    first.detail,
    ...rest.map((hold) => `also ${hold.reason}: ${hold.detail}`),
  ].join("; ");
  return {
    reason: first.reason,
    detail:
      detail.length > DETAIL_LIMIT
        ? `${detail.slice(0, DETAIL_LIMIT)}…`
        : detail,
  };
};
