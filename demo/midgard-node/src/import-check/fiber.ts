import { Effect, Schedule } from "effect";

import { Database, NodeConfig, ValidationPool } from "../services/index.js";
import { logImportCheckAlarm } from "./alarms.js";
import {
  makeImportCheckRuns,
  runBacklogPass,
  runImportCheckPass,
  runUnverifiedSweep,
} from "./checker.js";
import { poolImportCheckEvaluator } from "./evaluator.js";
import { makeNegativeCache } from "./negative-cache.js";

/** How often each checker loop looks for new and expired entries. */
export const IMPORT_CHECK_INTERVAL_MS = 1_000;

/**
 * The import checker's fiber: the import pass, the backlog pass and the
 * sweep (`checker.ts`), each in its own loop every
 * `IMPORT_CHECK_INTERVAL_MS`, so the sweep never waits on a run. The import
 * loop runs again at once after a block, and the backlog loop after any
 * batch. A failed pass is logged and the next one retries; the import path
 * never waits on them.
 */
export const importCheckFiber: Effect.Effect<
  void,
  never,
  Database | NodeConfig | ValidationPool
> = Effect.gen(function* () {
  const config = yield* NodeConfig;
  const evaluator = poolImportCheckEvaluator(yield* ValidationPool);
  if (evaluator === undefined)
    yield* Effect.logWarning(
      "The validation pool is disabled: imported transactions are not checked and leave the unverified set at their deadline",
    );
  else if (evaluator.backlogShare === 0)
    yield* Effect.logWarning(
      "The validation pool has one worker: import checks have no reserved worker and run only while admission is idle, and every unrun entry is projected past its deadline",
    );
  const input = {
    evaluator,
    config: config.IMPORT_CHECK,
    alarm: logImportCheckAlarm,
    cache: makeNegativeCache(config.IMPORT_CHECK.negativeCacheTtlMs),
    runs: makeImportCheckRuns(),
  };
  const alarmed = new Set<number>();
  const loop = <A, E, R>(name: string, pass: Effect.Effect<A, E, R>) =>
    pass.pipe(
      Effect.catchAllCause((cause) =>
        Effect.logError(`Import check ${name} failed: ${String(cause)}`).pipe(
          Effect.as(undefined),
        ),
      ),
      Effect.repeat({
        schedule: Schedule.spaced(`${IMPORT_CHECK_INTERVAL_MS} millis`),
      }),
    );
  const imports = runImportCheckPass(input).pipe(
    Effect.repeat({ while: (worked) => worked }),
  );
  const backlog = runBacklogPass(input).pipe(
    Effect.repeat({ while: (read) => read > 0 }),
  );
  yield* Effect.all(
    [
      loop("import pass", imports),
      loop("backlog pass", backlog),
      loop("sweep", runUnverifiedSweep(input, alarmed)),
    ],
    { concurrency: "unbounded", discard: true },
  );
});
