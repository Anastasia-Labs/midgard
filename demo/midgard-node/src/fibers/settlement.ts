import { Worker } from "node:worker_threads";

import { Effect, Ref, Schedule } from "effect";

import { Globals } from "../services/globals.js";
import type { SettlementHealth } from "../services/settlement.js";
import { resolveWorkerEntry } from "./resolve-worker-entry.js";

/** Separate JS heap/event loop, serial builds, and a two-connection SQL pool.
 * Node shutdown awaits termination; a replacement reconciles the same journal. */
export const settlementFiber = Effect.gen(function* () {
  const globals = yield* Globals;
  const attempt = Effect.scoped(
    Effect.gen(function* () {
      const worker = yield* Effect.acquireRelease(
        Effect.try(
          () =>
            new Worker(resolveWorkerEntry(import.meta.url, "settlement.js"), {
              env: { ...process.env, POSTGRES_WORKER_POOL_SIZE: "2" },
              resourceLimits: { maxOldGenerationSizeMb: 256 },
            }),
        ),
        (worker) => Effect.promise(() => worker.terminate()),
      );
      yield* Effect.async<void, Error>((resume) => {
        let lastProgress = Date.now();
        const watchdog = setInterval(() => {
          if (Date.now() - lastProgress > 180_000)
            resume(
              Effect.fail(
                new Error("Settlement worker stopped reporting progress"),
              ),
            );
        }, 30_000);
        const onMessage = (health: SettlementHealth) => {
          lastProgress = Date.now();
          Effect.runSync(Ref.set(globals.SETTLEMENT_HEALTH, health));
        };
        const onError = (error: Error) => resume(Effect.fail(error));
        const onExit = (code: number) =>
          resume(Effect.fail(new Error(`Settlement worker exited (${code})`)));
        worker.on("message", onMessage);
        worker.once("error", onError);
        worker.once("exit", onExit);
        return Effect.sync(() => {
          clearInterval(watchdog);
          worker.off("message", onMessage);
          worker.off("error", onError);
          worker.off("exit", onExit);
        });
      });
    }),
  ).pipe(
    Effect.catchAll((error) =>
      Ref.set(globals.SETTLEMENT_HEALTH, {
        observedAt: Date.now(),
        state: "error" as const,
        detail: error.message,
      }),
    ),
  );
  yield* Effect.repeat(attempt, Schedule.spaced("10 seconds"));
});
