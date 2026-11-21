import type { WatcherFollowerRuntime } from "../l1-follower/follower-runtime.js";
import type { WatcherL1Degradation } from "../l1-follower/tx-inputs.js";
import type {
  WatcherDecisionDriver,
  WatcherDecisionReadiness,
} from "./watcher-runtime.decision-driver.js";

/**
 * The watcher's L1 readiness, as /readyz reads it: the follower's reasons
 * (an intervention, catching up, an unhealthy view) and then the decision
 * driver's. /readyz reads synchronously, so the follower's reasons are a
 * cache that `refresh` renews (on every follower change, after every
 * decision pass, and on a timer); the driver's are read live. The
 * follower's degradations (status and metrics only, never readiness) are
 * cached alongside.
 */
export const createWatcherL1Readiness = (
  input: Readonly<{
    follower: Pick<WatcherFollowerRuntime, "readiness" | "degradations">;
    driver: () => Pick<WatcherDecisionDriver, "readiness"> | undefined;
  }>,
) => {
  let followerReadiness: readonly WatcherDecisionReadiness[] = [
    {
      reason: "l1_follower_catching_up",
      detail: "the follower has not reported a status yet",
    },
  ];
  let degradations: readonly WatcherL1Degradation[] = [];
  let refreshing: Promise<void> | null = null;
  const refresh = (): Promise<void> => {
    refreshing ??= Promise.all([
      input.follower.readiness(),
      input.follower.degradations().then(
        (next) => {
          degradations = next;
        },
        () => {
          degradations = [];
        },
      ),
    ])
      .then(([reasons]) => reasons)
      .then(
        (reasons) => {
          followerReadiness = reasons;
        },
        (error: unknown) => {
          followerReadiness = [
            {
              reason: "l1_follower_store_unreadable",
              detail: error instanceof Error ? error.message : String(error),
            },
          ];
        },
      )
      .finally(() => {
        refreshing = null;
      });
    return refreshing;
  };
  const read = (): readonly WatcherDecisionReadiness[] => [
    ...followerReadiness,
    ...(input.driver()?.readiness() ?? [
      {
        reason: "watcher_decision_pending",
        detail: "the decision driver has not started",
      },
    ]),
  ];
  return Object.freeze({ refresh, read, degradations: () => degradations });
};
