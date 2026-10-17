import type { FollowStatus } from "@al-ft/midgard-l1-follower";

/** A pass failed; the detail names the failure. */
export const WATCHER_DECISION_PASS_FAILED = "watcher_decision_pass_failed";
/** The release-depth observation is not available yet (for example a short chain). */
export const WATCHER_RELEASE_OBSERVATION_PENDING =
  "release_observation_pending";
/**
 * The follower in this process has not finished its store start yet. The
 * start may reset the store (a tracked-set addition), so the facts before it
 * may be incomplete for this process's tracked set: no pass reads them.
 */
export const WATCHER_FOLLOWER_NOT_STARTED = "l1_follower_not_started";
/**
 * The replay-transcript retirement reset a rewind started failed; the
 * detail names the failure. The handled-generation marker waits for it, and
 * the next pass retries it.
 */
export const WATCHER_RETIREMENT_RESET_FAILED =
  "watcher_retirement_reset_failed";
/**
 * The replay-transcript retirement reset a rewind started has not settled
 * within the pass's wait. The handled-generation marker waits for it.
 */
export const WATCHER_RETIREMENT_RESET_PENDING =
  "watcher_retirement_reset_pending";

/** The driver's `started` gate: the follower published a status with a cursor. */
export const watcherFollowerStarted = (
  status: Pick<FollowStatus, "cursor"> | null | undefined,
): boolean => (status?.cursor ?? null) !== null;

export type WatcherDecisionReadiness = Readonly<{
  reason: string;
  detail: string;
}>;
