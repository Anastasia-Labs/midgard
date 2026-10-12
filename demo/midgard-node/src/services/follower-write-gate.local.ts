/**
 * This process's side of the follower write gate (`follower-write-gate.ts`),
 * kept in `Globals`: the epoch its driver took, whether a recompute is
 * pending, and the producers running under permits.
 */
import type { Deferred } from "effect";

/** The view a write was computed at is no longer on the follower's chain. */
export const FOLLOWER_VIEW_STALE = "l1_follower_view_stale";
/** A driver recompute (rebase, orphan repair, first view) is pending. */
export const DRIVER_RECOMPUTE_PENDING = "l1_driver_recompute_pending";
/** No follower-change driver of this process has applied a view yet. */
export const FOLLOWER_VIEW_UNAPPLIED = "l1_follower_view_unapplied";

/** This process's side of the gate: the epoch its driver took, and its producers. */
export type FollowerWriteGateLocal = {
  /** The epoch this process's driver took; undefined before its first. */
  epoch: string | undefined;
  /** A recompute of this process's driver is under way or left pending. */
  recomputing: boolean;
  readonly producers: Set<Deferred.Deferred<void>>;
};

export const initialFollowerWriteGateLocal = (): FollowerWriteGateLocal => ({
  epoch: undefined,
  recomputing: false,
  producers: new Set(),
});

/**
 * The `/readyz` reasons of this process's side of the gate: no view applied
 * yet, or a recompute of its driver under way or left pending (the
 * driver's holds name why).
 */
export const followerWriteGateReasons = (
  local: FollowerWriteGateLocal,
): readonly string[] =>
  local.epoch === undefined
    ? [FOLLOWER_VIEW_UNAPPLIED]
    : local.recomputing
      ? [DRIVER_RECOMPUTE_PENDING]
      : [];
