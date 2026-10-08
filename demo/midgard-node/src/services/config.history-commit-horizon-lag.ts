import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { isFinal } from "@al-ft/midgard-l1-follower/heads";
import { Config } from "effect";

/**
 * The horizon lag d (U3): how many L1 blocks the commit end time lags behind
 * the L1 follower's covered tip. Default 0 (no lag). A negative or
 * fractional d, or one deeper than the follower keeps, refuses startup.
 *
 * The lagged block is at heads depth d + 1, and the follower prunes below
 * the shallowest final block (depth k + 1, k the deployment profile's
 * `automaticRecoveryMaxDepth`, which every verified manifest carries): the
 * block just above the lagged one must not be final, or every commit would
 * hold for good.
 */
export const historyCommitHorizonLagConfig = Config.all({
  HISTORY_COMMIT_HORIZON_LAG_BLOCKS: Config.number(
    "HISTORY_COMMIT_HORIZON_LAG_BLOCKS",
  ).pipe(
    Config.withDefault(0),
    Config.mapAttempt((value) => {
      if (!Number.isSafeInteger(value) || value < 0)
        throw new Error(
          "HISTORY_COMMIT_HORIZON_LAG_BLOCKS must be a non-negative safe integer",
        );
      const k = DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth;
      if (isFinal(value, { securityParameter: k }))
        throw new Error(
          `HISTORY_COMMIT_HORIZON_LAG_BLOCKS must be at most the deployment profile's rollback depth k = ${k.toString()}`,
        );
      return value;
    }),
  ),
});
