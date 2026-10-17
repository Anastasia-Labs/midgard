import {
  evaluateMidgardCekVerdict,
  type PhaseBConfig,
} from "@al-ft/midgard-validation";
import { Effect } from "effect";

import { type ValidationPoolService } from "../services/index.js";

/**
 * Admission Phase B runs its verdict-mode scripts on the validation pool, so
 * script execution never holds the node's main thread. A pool failure
 * (worker crash, job timeout, shutdown) computes the same verdict in process,
 * so admission never stalls on the pool and the verdict never depends on it.
 * Without a pool, Phase B evaluates in process as before.
 */
export const pooledCekVerdictEvaluator = (
  pool: ValidationPoolService,
): PhaseBConfig["evaluateCekVerdict"] =>
  pool.poolSize === 0
    ? undefined
    : (request) =>
        pool
          .evaluateCekVerdict(request)
          .pipe(
            Effect.catchAll((error) =>
              Effect.logWarning(
                `CEK verdict worker failed (${error.message}); evaluating in process`,
              ).pipe(
                Effect.zipRight(
                  Effect.sync(() => evaluateMidgardCekVerdict(request)),
                ),
              ),
            ),
          );
