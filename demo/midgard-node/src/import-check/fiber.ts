import { Effect, Schedule } from "effect";

import { Database, NodeConfig, ValidationPool } from "../services/index.js";
import { logImportCheckAlarm } from "./alarms.js";
import { runImportCheckPass } from "./checker.js";
import { poolImportCheckEvaluator } from "./evaluator.js";

/** How often the checker looks for new entries and expired ones. */
export const IMPORT_CHECK_INTERVAL_MS = 1_000;

/**
 * The import checker's fiber: a pass (`checker.ts`) every
 * `IMPORT_CHECK_INTERVAL_MS`, on the validation pool below admission. A
 * failed pass is logged and the next one retries; the import path never
 * waits on it.
 */
export const importCheckFiber: Effect.Effect<
  number,
  never,
  Database | NodeConfig | ValidationPool
> = Effect.gen(function* () {
  const config = yield* NodeConfig;
  const evaluator = poolImportCheckEvaluator(yield* ValidationPool);
  if (evaluator === undefined)
    yield* Effect.logWarning(
      "The validation pool is disabled: imported transactions are not checked and leave the unverified set at their deadline",
    );
  const pass = runImportCheckPass({
    evaluator,
    config: config.IMPORT_CHECK,
    alarm: logImportCheckAlarm,
  }).pipe(
    Effect.catchAllCause((cause) =>
      Effect.logError(`Import check pass failed: ${String(cause)}`),
    ),
  );
  return yield* pass.pipe(
    Effect.repeat(Schedule.spaced(`${IMPORT_CHECK_INTERVAL_MS} millis`)),
  );
});
