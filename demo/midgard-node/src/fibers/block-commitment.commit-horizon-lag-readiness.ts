import { Effect } from "effect";

import { followerBlockBelowCoveredTip } from "../database/follower-events.block-below-covered-tip.js";
import { Globals, NodeConfig } from "../services/index.js";
import {
  clearLivenessIncident,
  raiseLivenessIncident,
} from "../services/liveness-halt.js";

/** The source the commit horizon lag readiness raises under. It holds no
 * fiber: while it is raised every commit already holds (the lagged cap is
 * `null`, see `laggedEligibilityCap`), so the reason only makes that hold
 * visible on /readyz. */
export const COMMIT_HORIZON_LAG_SOURCE = "commit_horizon_lag";

/** The horizon lag d is above 0 and the L1 follower has no block d below its
 * covered tip: it has no cursor yet, or the chain above its origin is not yet
 * d blocks long, or that block is pruned. No end time is safe, so no block is
 * committed. It clears on the first commitment tick that finds the block. */
export const COMMIT_HORIZON_LAG_UNAVAILABLE = "commit_horizon_lag_unavailable";

/**
 * Raises or clears `commit_horizon_lag_unavailable` from the same follower
 * read the commit worker caps its end time with
 * (`followerBlockBelowCoveredTip`). At d = 0 it reads nothing and clears. A
 * failed read leaves the reason as it is and is logged; the next tick reads
 * again. Never fails.
 */
export const publishCommitHorizonLagReadiness = Effect.gen(function* () {
  const globals = yield* Globals;
  const lagBlocks = (yield* NodeConfig).HISTORY_COMMIT_HORIZON_LAG_BLOCKS;
  if (lagBlocks === 0)
    return yield* clearLivenessIncident(globals, COMMIT_HORIZON_LAG_SOURCE);
  const below = yield* Effect.either(followerBlockBelowCoveredTip(lagBlocks));
  if (below._tag === "Left")
    return yield* Effect.logWarning(
      `Could not read the follower block the commit horizon lag ${lagBlocks.toString()} needs; readiness is unchanged.`,
      below.left,
    );
  if (below.right.kind === "block")
    return yield* clearLivenessIncident(globals, COMMIT_HORIZON_LAG_SOURCE);
  yield* raiseLivenessIncident(
    globals,
    COMMIT_HORIZON_LAG_SOURCE,
    COMMIT_HORIZON_LAG_UNAVAILABLE,
    `every commit holds until the L1 follower has the block ${lagBlocks.toString()} below its covered tip (${below.right.reason}: ${below.right.detail})`,
  );
});
