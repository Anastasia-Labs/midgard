/**
 * The `/readyz` reasons landed-block processing holds (plan §7.3, N3). Each
 * keeps the process up: the driver retries on its backoff, and the reason
 * clears once the condition does.
 */
import type { DriverHold } from "../l1-events/driver.js";
import {
  NATIVE_MPF_RESTORE_INDEX_CAP_EXCEEDED,
  NATIVE_MPF_RESTORE_READ_TRANSIENT,
  NATIVE_MPF_RESTORE_ROOT_NOT_RETAINED,
} from "../services/liveness-halt.js";

/** A landed block does not replay to its header, or does not link to its parent. Never adopted. */
export const LANDED_BLOCK_INVALID = "landed_block_invalid";
/**
 * This node's own landed block disagrees with its journal: the journal
 * names another base or root, or its delta on the parent's ledger does not
 * reach the header's root. A local fault (the block is this node's, and
 * its header binds the root), never attributed to the block; processing
 * stops at it until the journal is repaired.
 */
export const LANDED_BLOCK_OWN_JOURNAL_MISMATCH =
  "landed_block_own_journal_mismatch";
/**
 * This node's block landed after its journal was abandoned: blocks after it
 * wait until the rebase revived it and its local finalization ran (both
 * without operator action).
 */
export const LANDED_BLOCK_OWN_REVIVAL_PENDING =
  "landed_block_own_revival_pending";
/** A foreign block's DA payload is not available yet, or it ends past what the view can know. */
export const LANDED_BLOCK_AWAITING_DA = "landed_block_awaiting_da";
/**
 * A foreign block's retained DA payload no longer verified (its digest,
 * identity or decoding), so the row was deleted and the payload is being
 * fetched again; clears once a refetch is served.
 */
export const LANDED_BLOCK_DA_REFETCH_PENDING =
  "landed_block_da_refetch_pending";
/**
 * A foreign block names an event the follower does not know at all at the
 * view. It stays a wait, never `landed_block_invalid`, even at a view whose
 * horizon covers the block's window: the follower's facts can lack an
 * event for reasons local to this node (a retired event pruned without the
 * landed-frontier floor, or one admitted before the follower's origin), so
 * absence does not prove the block wrong, and a local gap is never blamed
 * on the block. The block is not adopted either way, and the verdict is
 * re-read on every run. An event the view knows but whose inclusion time
 * is outside the block's window is the block's fault: `invalid`.
 */
export const LANDED_BLOCK_EVENT_UNKNOWN = "landed_block_event_unknown";
/** A forced order admitted in a foreign block's window cannot be read back from the facts yet. */
export const LANDED_BLOCK_FORCED_ORDER_PENDING =
  "landed_block_forced_order_pending";
/**
 * Importing a foreign block failed for a reason it cannot pin on the block:
 * a local computation (an MPF root) or a replay failure it cannot classify.
 * Never attributed to the block; every run retries it.
 */
export const LANDED_BLOCK_REPLAY_INCOMPLETE = "landed_block_replay_incomplete";
/** Replaying or recording a landed block failed for a reason it could not classify. */
export const LANDED_BLOCK_REPLAY_FAILED = "landed_block_replay_failed";
/**
 * The follower write gate refused a write by its named reason (a driver
 * recompute pending, a view no longer on the chain), or the follower moved
 * off the run's view ("view moved"); the next run retries.
 */
export const LANDED_BLOCKS_WAITING = "landed_blocks_waiting";
/**
 * The merged queue root is on no lineage `confirmed_ledger` can reach: not
 * forward from the frontier through the queue history, nor back through the
 * retained folds (N5). On an honest chain it cannot occur once the frontier
 * was set at genesis or on a fold this node made: every rollback a merge can
 * suffer is above the prune boundary, where its fold is retained.
 */
export const CONFIRMED_LEDGER_BEHIND = "confirmed_ledger_behind";
/**
 * A fold's base is not what the block names: the frontier is not its parent
 * (header and root), or `confirmed_ledger` lacks what it spends or holds
 * what it produces. Refused, nothing written.
 */
export const CONFIRMED_LEDGER_BASE_MISMATCH = "confirmed_ledger_base_mismatch";
/** The merged root passed this node's own block, whose journal is not locally applied yet. */
export const CONFIRMED_LEDGER_OWN_BLOCK_PENDING =
  "confirmed_ledger_own_block_pending";
/** The working ledger and native MPF wait for the rebase onto the processed blocks. */
export const LANDED_BLOCK_REBASE_PENDING = "landed_block_rebase_pending";
/** The rebase failed; the follower-change driver retries it on its backoff. */
export const LANDED_BLOCK_REBASE_FAILED = "landed_block_rebase_failed";
/**
 * The follower's admission tables (`l1_event_keys`,
 * `node_l1_forced_order_fields`) are missing from the node database, so the
 * own-journal disposition cannot tell whether a journal's event left the
 * chain: the rebase is held until the schema is installed (`migrate`).
 */
export const LANDED_BLOCK_FOLLOWER_SCHEMA_MISSING =
  "landed_block_follower_schema_missing";
/** The liveness source a failed rebase raises its reason under. */
export const LANDED_BLOCK_REBASE_SOURCE = "landed_block_rebase";

const PRIORITY = [
  LANDED_BLOCK_INVALID,
  LANDED_BLOCK_OWN_JOURNAL_MISMATCH,
  LANDED_BLOCK_FOLLOWER_SCHEMA_MISSING,
  LANDED_BLOCK_EVENT_UNKNOWN,
  LANDED_BLOCK_FORCED_ORDER_PENDING,
  LANDED_BLOCK_DA_REFETCH_PENDING,
  LANDED_BLOCK_AWAITING_DA,
  NATIVE_MPF_RESTORE_ROOT_NOT_RETAINED,
  NATIVE_MPF_RESTORE_INDEX_CAP_EXCEEDED,
  NATIVE_MPF_RESTORE_READ_TRANSIENT,
  LANDED_BLOCK_REBASE_FAILED,
  LANDED_BLOCK_REPLAY_INCOMPLETE,
  LANDED_BLOCK_REPLAY_FAILED,
  CONFIRMED_LEDGER_BASE_MISMATCH,
  LANDED_BLOCKS_WAITING,
  CONFIRMED_LEDGER_BEHIND,
  LANDED_BLOCK_OWN_REVIVAL_PENDING,
  CONFIRMED_LEDGER_OWN_BLOCK_PENDING,
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
