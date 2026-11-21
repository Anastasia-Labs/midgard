import type { FollowStatus, PruneFloor } from "@al-ft/midgard-l1-follower";

/** The proof roster's prune floor, as `FollowStatus.prune.floorLags` names it. */
export const WATCHER_PROOF_ROSTER_FLOOR = "watcher_proof_roster";

export type WatcherProofRosterRetention = Readonly<{
  /** Holds pruning at the store's origin until the roster is complete. */
  floor: PruneFloor;
  /** Hears every follow status (`FollowStatus.atTip` and its cursor). */
  observe(status: FollowStatus): void;
  /**
   * Whether an admission that reached its final return over an observation
   * whose block stands at `observedHeight` on the follower's chain (null:
   * not on it) completed a pass at tip; the first such pass lifts the floor
   * for the rest of the process.
   */
  passAtTip(observedHeight: number | null): boolean;
  /** Whether a pass at tip has lifted the floor in this process. */
  lifted(): boolean;
}>;

/**
 * The proof roster: which headers' L1 history the store must hold for the
 * proof objectives it serves. Until the progress authority has rebuilt it in
 * this process (re-read its objective table, pinned each open objective and
 * admitted every fault the queue names) the store's pins are not known to
 * cover it: a lost follower store holds none, and an objective a lost
 * record no longer names may still be needed by a run of its header. So no
 * prune step deletes anything above the store's origin until then.
 *
 * A pass is at tip when an admission reaches its final return while the
 * follower reports `atTip`, over an observation whose block is at or above
 * the height where the follower first reported `atTip` in this process. The
 * floor is then lifted and stays lifted for the process, whatever authority
 * admits later: a journal-busy requeue re-creates the authority, never this.
 * Process memory only: a restart holds the floor again until its own first
 * pass at tip. Its lag is a follower degradation while it holds.
 */
export const createWatcherProofRosterRetention =
  (): WatcherProofRosterRetention => {
    let latest: FollowStatus | null = null;
    let firstTipHeight: number | null = null;
    let lifted = false;
    return Object.freeze({
      floor: Object.freeze<PruneFloor>({
        name: WATCHER_PROOF_ROSTER_FLOOR,
        floor: async ({ tx }) => {
          if (lifted) return null;
          const cursor = (
            await tx.query("SELECT origin_slot FROM l1_follower_cursor")
          )[0];
          return cursor === undefined ? 0 : Number(cursor.origin_slot);
        },
      }),
      observe: (status) => {
        latest = status;
        if (firstTipHeight === null && status.atTip && status.cursor !== null)
          firstTipHeight = status.cursor.height;
      },
      passAtTip: (observedHeight) => {
        if (
          firstTipHeight === null ||
          latest?.atTip !== true ||
          observedHeight === null ||
          observedHeight < firstTipHeight
        )
          return false;
        lifted = true;
        return true;
      },
      lifted: () => lifted,
    });
  };
