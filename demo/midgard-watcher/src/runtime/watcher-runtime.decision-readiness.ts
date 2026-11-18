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
/** The driver's `started` gate: the follower published a status with a cursor. */
export const watcherFollowerStarted = (
  status: Pick<FollowStatus, "cursor"> | null | undefined,
): boolean => (status?.cursor ?? null) !== null;

export type WatcherDecisionReadiness = Readonly<{
  reason: string;
  detail: string;
}>;
