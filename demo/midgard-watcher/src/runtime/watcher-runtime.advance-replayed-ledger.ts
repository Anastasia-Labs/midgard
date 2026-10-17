import type { WatcherAuthenticatedStateQueueObservation } from "../indexers/authenticated-state-queue-observation.js";
import type { WatcherObservationRead } from "../l1-follower/observation.js";
import type { WatcherDecisionReadiness } from "./watcher-runtime.decision-readiness.js";

export type WatcherReplayCheckpointDriver = Readonly<{
  depth: number;
  advance(
    observation: WatcherAuthenticatedStateQueueObservation,
  ): Promise<readonly WatcherDecisionReadiness[]>;
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
