import { SqlClient } from "@effect/sql/SqlClient";
import { Effect, Metric, Ref, Schedule } from "effect";

import { TxAdmissionsDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import {
  BatchSql,
  Globals,
  Lucid,
  MempoolLedgerCache,
  NodeConfig,
  ValidationPool,
  WriteBehind,
} from "../services/index.js";
import {
  validationCoalescedWakeupCounter,
  validationDrainLoopsActiveGauge,
  validationEventWakeupCounter,
} from "./tx-queue-processor.classify-plutus-evaluation-failure.js";
import { repeatScheduledWithCauseLogging } from "./tx-queue-processor.run-phase-afor-batch.js";
import { txQueueProcessorAction } from "./tx-queue-processor.tx-queue-processor-action.js";

const txQueueProcessorDrainLoop = (): Effect.Effect<
  bigint,
  DatabaseError | Error,
  | SqlClient
  | NodeConfig
  | Globals
  | Lucid
  | WriteBehind
  | ValidationPool
  | MempoolLedgerCache
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    let handledGeneration = yield* Ref.get(globals.TX_QUEUE_WAKE_GENERATION);
    let recoverySweep = true;
    while (true) {
      const result = yield* txQueueProcessorAction(recoverySweep);
      recoverySweep = false;
      if (result.processed && result.claimedCount >= result.batchSize) {
        continue;
      }
      if (result.processed && (yield* TxAdmissionsDB.countBacklog) > 0n) {
        continue;
      }
      const currentGeneration = yield* Ref.get(
        globals.TX_QUEUE_WAKE_GENERATION,
      );
      if (currentGeneration === handledGeneration) return handledGeneration;
      handledGeneration = currentGeneration;
      yield* Metric.increment(validationCoalescedWakeupCounter);
    }
  });

export const hasUnseenTxQueueWake = (
  handledGeneration: bigint,
  currentGeneration: bigint,
): boolean => currentGeneration !== handledGeneration;

/**
 * Runs one drain loop if a drain slot is free. The slot is claimed and
 * released as one bracket: the claim cannot be interrupted, and its release
 * runs however the loop ends, so neither a defect nor a sibling's
 * interruption between the claim and the loop can leak a slot.
 */
export const txQueueProcessorDrainOnce = (
  hooks: {
    /** Runs right after a slot is claimed; tests inject failures here. */
    readonly afterSlotClaimed?: () => Effect.Effect<void>;
  } = {},
): Effect.Effect<
  void,
  DatabaseError | Error,
  | SqlClient
  | NodeConfig
  | Globals
  | Lucid
  | WriteBehind
  | ValidationPool
  | MempoolLedgerCache
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const config = yield* NodeConfig;
    let handledGeneration = 0n;
    yield* Effect.acquireUseRelease(
      Effect.gen(function* () {
        const claimed = yield* Ref.modify(
          globals.TX_QUEUE_PROCESSOR_ACTIVE,
          (active) =>
            active < config.VALIDATION_DRAIN_LOOPS
              ? [true, active + 1]
              : [false, active],
        );
        handledGeneration = yield* Ref.get(globals.TX_QUEUE_WAKE_GENERATION);
        return claimed;
      }),
      (claimed) =>
        Effect.gen(function* () {
          if (!claimed) {
            yield* Metric.increment(validationCoalescedWakeupCounter);
            return;
          }
          yield* hooks.afterSlotClaimed?.() ?? Effect.void;
          const active = yield* Ref.get(globals.TX_QUEUE_PROCESSOR_ACTIVE);
          yield* validationDrainLoopsActiveGauge(
            Effect.succeed(BigInt(active)),
          );
          handledGeneration = yield* txQueueProcessorDrainLoop();
        }),
      (claimed) =>
        claimed
          ? Effect.gen(function* () {
              const count = yield* Ref.updateAndGet(
                globals.TX_QUEUE_PROCESSOR_ACTIVE,
                (activeCount) => Math.max(0, activeCount - 1),
              );
              yield* validationDrainLoopsActiveGauge(
                Effect.succeed(BigInt(count)),
              );
              const currentGeneration = yield* Ref.get(
                globals.TX_QUEUE_WAKE_GENERATION,
              );
              if (hasUnseenTxQueueWake(handledGeneration, currentGeneration)) {
                yield* Effect.forkDaemon(
                  txQueueProcessorDrainOnce().pipe(
                    Effect.catchAllCause(Effect.logWarning),
                  ),
                );
              }
            })
          : Effect.void,
    );
  });

export const requestTxQueueProcessorWakeup: Effect.Effect<
  void,
  never,
  | BatchSql
  | NodeConfig
  | Globals
  | Lucid
  | WriteBehind
  | ValidationPool
  | MempoolLedgerCache
> = Effect.gen(function* () {
  const globals = yield* Globals;
  const batchSql = yield* BatchSql;
  yield* Ref.update(
    globals.TX_QUEUE_WAKE_GENERATION,
    (generation) => generation + 1n,
  );
  yield* Metric.increment(validationEventWakeupCounter);
  yield* Effect.forkDaemon(
    txQueueProcessorDrainOnce().pipe(
      Effect.provideService(SqlClient, batchSql),
      Effect.catchAllCause(Effect.logWarning),
    ),
  );
});

/**
 * Fiber wrapper that repeats queue-drain and validation work on the provided
 * schedule.
 */
export const txQueueProcessorFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<
  void,
  never,
  | SqlClient
  | NodeConfig
  | Globals
  | Lucid
  | WriteBehind
  | ValidationPool
  | MempoolLedgerCache
> =>
  Effect.gen(function* () {
    const config = yield* NodeConfig;
    yield* Effect.logInfo("🔶 Tx queue processor fiber started.");
    yield* repeatScheduledWithCauseLogging(
      Effect.forEach(
        Array.from({ length: config.VALIDATION_DRAIN_LOOPS }),
        () => txQueueProcessorDrainOnce(),
        { concurrency: "unbounded", discard: true },
      ),
      schedule,
    );
  });
