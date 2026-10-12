import { randomUUID } from "node:crypto";
import { Worker } from "node:worker_threads";

import { Duration, Effect, Ref, Runtime, Schedule } from "effect";

import { Globals } from "../services/globals.js";
import type { UnwrittenHold } from "../services/intent-journal.holds.js";
import { IntentJournal } from "../services/intent-journal.js";
import type { SettlementHealth } from "../services/settlement.js";
import { SETTLEMENT_WORKER_RECOVERY_TICKS } from "../services/settlement-readiness.js";
import { resolveWorkerEntry } from "./resolve-worker-entry.js";

/** Old-generation cap of the settlement worker. A worker-thread probe against
 * the lc1 devnet's Kupo (945 published reference scripts) peaked at 176 MB
 * through the module graph and the absorb and payout reference-script
 * lookups; reading the whole reference-script wallet instead of each role
 * token needed ~410 MB and killed every run. Not yet measured: a transaction
 * build with local UPLC evaluation inside the worker. */
export const SETTLEMENT_WORKER_HEAP_MB = 256;

export type SettlementWorkerData = { readonly ownerToken: string };

export type SettlementSupervision = {
  readonly spacing: Duration.DurationInput;
  readonly watchdogMs: number;
  /**
   * Takes over the refusal holds a worker run reports it could not write
   * (the node journal's `adopt`, I1-H1).
   */
  readonly adoptRefusalHolds?: (holds: readonly UnwrittenHold[]) => void;
};

const DEFAULT_SETTLEMENT_SUPERVISION: SettlementSupervision = {
  spacing: "10 seconds",
  watchdogMs: 180_000,
};

const spawnSettlementWorker = (workerData: SettlementWorkerData) =>
  new Worker(resolveWorkerEntry(import.meta.url, "settlement.js"), {
    env: { ...process.env, POSTGRES_WORKER_POOL_SIZE: "2" },
    resourceLimits: { maxOldGenerationSizeMb: SETTLEMENT_WORKER_HEAP_MB },
    workerData,
  });

/** One worker run after another, each awaited to termination before the next
 * starts. That is what lets every run carry the same ownership token: a dead
 * thread cannot renew or write, so the replacement resumes the lease at once
 * instead of waiting a minute for it to expire, while any other process holds
 * a different token and stays fenced by renew and assertOwner. */
export const superviseSettlementWorker = (
  health: Ref.Ref<SettlementHealth>,
  spawn: (workerData: SettlementWorkerData) => Worker,
  {
    spacing,
    watchdogMs,
    adoptRefusalHolds,
  }: SettlementSupervision = DEFAULT_SETTLEMENT_SUPERVISION,
) =>
  Effect.gen(function* () {
    const runSync = Runtime.runSync(yield* Effect.runtime<never>());
    const workerData: SettlementWorkerData = { ownerToken: randomUUID() };
    // Consecutive failed runs; cleared once a replacement run completes
    // SETTLEMENT_WORKER_RECOVERY_TICKS ticks in a row.
    let streak: SettlementHealth["workerFailures"];
    const attempt = Effect.scoped(
      Effect.gen(function* () {
        const worker = yield* Effect.acquireRelease(
          Effect.try(() => spawn(workerData)),
          (worker) => Effect.promise(() => worker.terminate()),
        );
        // Runs on every way out of a run, not only on interruption: a failed
        // run must not leave its watchdog timer and listeners behind.
        let detach = () => {};
        yield* Effect.async<void, Error>((resume) => {
          let lastProgress = Date.now();
          // This run's completed ticks since its last report of any other kind.
          let completedTicks = 0;
          // The worker's own account of why it is failing, kept so its death
          // is reported with the cause rather than just the exit code.
          let lastError: string | undefined;
          const withLastError = (message: string) =>
            lastError === undefined
              ? message
              : `${message} (last reported error: ${lastError})`;
          const watchdog = setInterval(
            () => {
              if (Date.now() - lastProgress > watchdogMs)
                resume(
                  Effect.fail(
                    new Error(
                      withLastError(
                        "Settlement worker stopped reporting progress",
                      ),
                    ),
                  ),
                );
            },
            Math.min(30_000, watchdogMs),
          );
          const onMessage = ({
            tickCompleted,
            intentRefusalHolds,
            ...report
          }: SettlementHealth) => {
            lastProgress = Date.now();
            if (intentRefusalHolds !== undefined)
              adoptRefusalHolds?.(intentRefusalHolds);
            if (report.state === "error" && report.detail !== lastError)
              runSync(
                Effect.logWarning(
                  `Settlement worker reported an error: ${report.detail}`,
                ),
              );
            lastError = report.state === "error" ? report.detail : undefined;
            // Only ticks that ran to their end prove the replacement works,
            // and several in a row: one cheap tick before the run dies
            // building a job proves nothing. A run still waiting for the
            // ownership lease, or one whose ticks keep failing, keeps its
            // predecessors' failures; if it dies it adds its own, however long
            // it stayed up reporting errors.
            completedTicks = tickCompleted === true ? completedTicks + 1 : 0;
            if (
              streak !== undefined &&
              completedTicks >= SETTLEMENT_WORKER_RECOVERY_TICKS
            ) {
              runSync(
                Effect.logInfo(
                  `Settlement worker recovered after ${streak.count} failed runs`,
                ),
              );
              streak = undefined;
            }
            runSync(
              Ref.set(
                health,
                streak === undefined
                  ? report
                  : { ...report, workerFailures: streak },
              ),
            );
          };
          const onError = (error: Error) =>
            resume(Effect.fail(new Error(withLastError(error.message))));
          const onExit = (code: number) =>
            resume(
              Effect.fail(
                new Error(
                  `${lastError ?? "no error reported"} (worker exited ${code})`,
                ),
              ),
            );
          worker.on("message", onMessage);
          worker.once("error", onError);
          worker.once("exit", onExit);
          // The 'error' listener stays: an error emitted with none attached
          // would throw in the node. A resume after the first is ignored.
          detach = () => {
            clearInterval(watchdog);
            worker.off("message", onMessage);
            worker.off("exit", onExit);
          };
        }).pipe(Effect.ensuring(Effect.sync(() => detach())));
      }),
    ).pipe(
      Effect.catchAll((error) =>
        Effect.gen(function* () {
          const observedAt = Date.now();
          streak = {
            count: (streak?.count ?? 0) + 1,
            since: streak?.since ?? observedAt,
            last: error.message,
          };
          yield* Effect.logWarning(
            `Settlement worker failed (${streak.count} in a row): ${error.message}`,
          );
          yield* Ref.set(health, {
            observedAt,
            state: "error" as const,
            detail: error.message,
            workerFailures: streak,
          });
        }),
      ),
    );
    yield* Effect.repeat(attempt, Schedule.spaced(spacing));
  });

/** Separate JS heap/event loop, serial builds, and a two-connection SQL pool.
 * Node shutdown awaits termination; a replacement reconciles the same journal. */
export const settlementFiber = Effect.gen(function* () {
  const globals = yield* Globals;
  const journal = yield* IntentJournal;
  yield* superviseSettlementWorker(
    globals.SETTLEMENT_HEALTH,
    spawnSettlementWorker,
    { ...DEFAULT_SETTLEMENT_SUPERVISION, adoptRefusalHolds: journal.adopt },
  );
});
