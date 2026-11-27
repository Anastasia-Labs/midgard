import { Effect, Runtime, type Scope } from "effect";

import { submissionCheckConfig } from "../commands/http/submission-check.js";
import { submittedTxRules } from "../commands/http/transaction-admission.js";
import { NodeConfig } from "./config.js";
import { Lucid } from "./lucid.js";
import {
  createSubmitCheckPool,
  DEFAULT_START_TIMEOUT_MS,
  type SubmitCheckPool,
} from "./submit-check-pool.js";

/**
 * The node's submit check pool for the enclosing scope, sized by
 * `SUBMIT_INGRESS_WORKER_POOL_SIZE`, `SUBMIT_INGRESS_WORKER_QUEUE` and
 * `SUBMIT_INGRESS_MAX_IN_FLIGHT_BYTES`, checking with the node's rules
 * (`submittedTxRules`, read once here). Worker stops are logged.
 */
export const acquireSubmitCheckPool: Effect.Effect<
  SubmitCheckPool,
  never,
  NodeConfig | Lucid | Scope.Scope
> = Effect.gen(function* () {
  const config = yield* NodeConfig;
  const rules = yield* submittedTxRules;
  const runFork = Runtime.runFork(yield* Effect.runtime<never>());
  const pool = yield* Effect.acquireRelease(
    Effect.sync(() =>
      createSubmitCheckPool({
        size: config.SUBMIT_INGRESS_WORKER_POOL_SIZE,
        maxQueued: config.SUBMIT_INGRESS_WORKER_QUEUE,
        maxQueuedBytes: config.SUBMIT_INGRESS_MAX_IN_FLIGHT_BYTES,
        config: submissionCheckConfig(config, rules),
        onWorkerStopped: (_, detail) => {
          runFork(Effect.logError(`${detail}; replacing it`));
        },
      }),
    ),
    (pool) => Effect.promise(() => pool.close()),
  );
  // /submit is served only after this resolves, so its first requests find a
  // loaded worker instead of holding ingress slots while workers load. A pool
  // that loads no worker in time is served anyway: its requests then wait for
  // a replacement worker or answer 503, as after any worker crash.
  if (!(yield* Effect.promise(() => pool.whenReady(DEFAULT_START_TIMEOUT_MS))))
    yield* Effect.logWarning(
      `no submit check worker loaded within ${DEFAULT_START_TIMEOUT_MS.toString()} ms; serving /submit while workers restart`,
    );
  return pool;
});
