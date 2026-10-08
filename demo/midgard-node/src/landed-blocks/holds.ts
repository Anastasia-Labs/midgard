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
/** A foreign block's DA payload is not available yet, or it ends past what the view can know. */
export const LANDED_BLOCK_AWAITING_DA = "landed_block_awaiting_da";
/** A foreign block names an event the follower does not know at the view. */
export const LANDED_BLOCK_EVENT_UNKNOWN = "landed_block_event_unknown";
/** A forced order admitted in a foreign block's window cannot be read back from the facts yet. */
export const LANDED_BLOCK_FORCED_ORDER_PENDING =
  "landed_block_forced_order_pending";
/** Replaying or recording a landed block failed for a reason it could not classify. */
export const LANDED_BLOCK_REPLAY_FAILED = "landed_block_replay_failed";
/** The history owner is recovering, or the follower moved off the run's view ("view moved"); the next run retries. */
export const LANDED_BLOCKS_WAITING = "landed_blocks_waiting";
/** `confirmed_ledger` cannot reach the merged queue root yet. */
export const CONFIRMED_LEDGER_BEHIND = "confirmed_ledger_behind";
/** The working ledger and native MPF wait for the rebase onto the processed blocks. */
export const LANDED_BLOCK_REBASE_PENDING = "landed_block_rebase_pending";
/** The rebase failed; the history owner retries it on its backoff. */
export const LANDED_BLOCK_REBASE_FAILED = "landed_block_rebase_failed";
/**
 * The rebase's batch closure reached an acceptance receipt with a member
 * that is neither pending, settled, nor recorded rejected, so it cannot
 * reverse the batch; the history owner retries the rebase on its backoff,
 * and the reason clears once a rebase runs.
 */
export const LANDED_BLOCK_BATCH_UNDECIDED = "landed_block_batch_undecided";
/** The liveness source a failed rebase raises its reason under. */
export const LANDED_BLOCK_REBASE_SOURCE = "landed_block_rebase";

const PRIORITY = [
  LANDED_BLOCK_INVALID,
  LANDED_BLOCK_OWN_JOURNAL_ABANDONED,
  LANDED_BLOCK_EVENT_UNKNOWN,
  LANDED_BLOCK_FORCED_ORDER_PENDING,
  LANDED_BLOCK_AWAITING_DA,
  LANDED_BLOCK_BATCH_UNDECIDED,
  LANDED_BLOCK_REBASE_FAILED,
  LANDED_BLOCK_REPLAY_FAILED,
  LANDED_BLOCKS_WAITING,
  CONFIRMED_LEDGER_BEHIND,
  LANDED_BLOCK_REBASE_PENDING,
] as const;

/** Each reason's detail is cut to this many characters; reason names never are. */
const DETAIL_LIMIT = 500;

const cut = (detail: string) =>
  detail.length > DETAIL_LIMIT ? `${detail.slice(0, DETAIL_LIMIT)}…` : detail;

/**
 * The first hold by priority, with every other hold named in its detail.
 * Only each reason's own detail is truncated, so every reason stays named.
 */
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
  return {
    reason: first.reason,
    detail: [
      cut(first.detail),
      ...rest.map((hold) => `also ${hold.reason}: ${cut(hold.detail)}`),
    ].join("; "),
  };
};
