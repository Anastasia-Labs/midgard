import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherObservationRead } from "../l1-follower/observation.js";
import type { WatcherDecisionReadiness } from "./watcher-runtime.decision-readiness.js";

export type WatcherReplayCheckpointDriver = Readonly<{
  depth: number;
  advance(
    observation: WatcherAuthenticatedStateQueueObservation,
  ): Promise<readonly WatcherDecisionReadiness[]>;
  /**
   * Whether the checkpoint point `depth` blocks below the tip lies at or
   * below the follower's origin while the deployment's protocol init
   * followed that origin (`watcherReplayCheckpointPrecedesInit`).
   */
  checkpointPrecedesInit(depth: number): Promise<boolean>;
}>;

/** Advance even an empty queue, using the separately stable L1 observation. */
export const advanceWatcherReplayCheckpoint = async (
  input: Readonly<{
    releaseDepth: number;
    replayedLedger?: WatcherReplayCheckpointDriver;
  }>,
  release: WatcherObservationRead,
  observe: (depth: number) => Promise<WatcherObservationRead>,
  retry: () => void,
): Promise<readonly WatcherDecisionReadiness[]> => {
  if (input.replayedLedger === undefined) return [];
  const stable =
    input.replayedLedger.depth === input.releaseDepth
      ? release
      : await observe(input.replayedLedger.depth);
  // A checkpoint point at or below the origin has no observation, but
  // before the init it needs none: the queue is empty, and every ledger
  // replays from genesis. Nothing is advanced, persisted or released.
  if (
    stable.kind === "unready" &&
    stable.reason === "l1_follower_short_chain" &&
    (await input.replayedLedger.checkpointPrecedesInit(
      input.replayedLedger.depth,
    ))
  )
    return [];
  if (stable.kind !== "ok")
    return [
      {
        reason: "replayed_ledger_stable_observation_pending",
        detail: `${stable.reason}: ${stable.detail}`,
      },
    ];
  const reasons = await input.replayedLedger.advance(stable.observation);
  if (reasons.length > 0) retry();
  return reasons;
};
