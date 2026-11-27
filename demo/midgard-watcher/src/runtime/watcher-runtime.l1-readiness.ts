import type { WatcherProofMaterialsViews } from "../fault-proofs/proof-materials-views.js";
import type { WatcherFollowerRuntime } from "../l1-follower/follower-runtime.js";
import type { WatcherL1Degradation } from "../l1-follower/tx-inputs.js";
import type {
  WatcherDecisionDriver,
  WatcherDecisionReadiness,
} from "./watcher-runtime.decision-driver.js";

/** The proof-materials sweep is closed: informational, never readiness. */
export const PROOF_MATERIALS_SWEEP_CLOSED = "proof_materials_sweep_closed";

/**
 * The watcher's L1 readiness, as /readyz reads it: the follower's reasons
 * (an intervention, catching up, an unhealthy view) and then the decision
 * driver's. /readyz reads synchronously, so the follower's reasons are a
 * cache that `refresh` renews (on every follower change, after every
 * decision pass, and on a timer); the driver's are read live. The
 * follower's degradations (status and metrics only, never readiness) are
 * cached alongside, followed by why the proof-materials sweep is closed,
 * read live.
 */
export const createWatcherL1Readiness = (
  input: Readonly<{
    follower: Pick<WatcherFollowerRuntime, "readiness" | "degradations">;
    driver: () => Pick<WatcherDecisionDriver, "readiness"> | undefined;
    proofMaterials?: Pick<WatcherProofMaterialsViews, "sweepClosed">;
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
  const sweepClosed = (): readonly WatcherL1Degradation[] =>
    (input.proofMaterials?.sweepClosed() ?? []).map((detail) => ({
      reason: PROOF_MATERIALS_SWEEP_CLOSED,
      count: 1,
      detail,
    }));
  return Object.freeze({
    refresh,
    read,
    degradations: () => [...degradations, ...sweepClosed()],
  });
};
