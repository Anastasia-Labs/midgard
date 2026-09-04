import {
  applyUTxOStatePatch,
  type UTxOStatePatch,
} from "@al-ft/midgard-validation";
import { SqlClient } from "@effect/sql";
import {
  Context,
  Data,
  Deferred,
  Duration,
  Effect,
  Layer,
  Metric,
  Ref,
} from "effect";

import { MempoolLedgerDB } from "../database/index.js";
import type { DatabaseError } from "../database/utils/common.js";
import {
  Globals,
  type MempoolLedgerDelta,
  type MempoolLedgerDeltaLog,
} from "./globals.js";

export type MempoolLedgerState = Map<string, Buffer>;

export type PhaseBSequence = {
  readonly epoch: number;
  readonly sequence: bigint;
  readonly runDecision: <A, E, R>(
    effect: Effect.Effect<A, E, R>,
  ) => Effect.Effect<A, E | ValidationPipelineEpochError, R>;
  readonly runPersistence: <A, E, R>(
    effect: Effect.Effect<A, E, R>,
  ) => Effect.Effect<A, E | ValidationPipelineEpochError, R>;
  readonly cancel: Effect.Effect<void>;
};

export class ValidationPipelineEpochError extends Data.TaggedError(
  "ValidationPipelineEpochError",
)<{
  readonly epoch: number;
  readonly sequence: bigint;
  readonly failedSequence: bigint;
  readonly message: string;
}> {}

export type MempoolLedgerCacheService = {
  readonly withClaimLock: <A, E, R>(
    effect: Effect.Effect<A, E, R>,
  ) => Effect.Effect<A, E, R>;
  /** Register while holding withClaimLock, immediately after a non-empty claim. */
  readonly registerPhaseBSequence: Effect.Effect<PhaseBSequence>;
  readonly withPhaseBLock: <A, E, R>(
    effect: Effect.Effect<A, E, R>,
  ) => Effect.Effect<A, E, R>;
  /** Must be called while holding withPhaseBLock. */
  readonly currentState: Effect.Effect<MempoolLedgerState, DatabaseError>;
  /** Must be called while holding withPhaseBLock, before ordered persistence. */
  readonly applyPatchAndSync: (
    patch: UTxOStatePatch,
  ) => Effect.Effect<void, DatabaseError>;
  /** Must be called by its matching sequence while holding withPhaseBLock. */
  readonly applySpeculativePatch: (
    sequence: bigint,
    patch: UTxOStatePatch,
  ) => Effect.Effect<void, DatabaseError>;
  /** Reloads durable state and advances beyond a poisoned validation epoch. */
  readonly recoverPoisonedEpoch: Effect.Effect<void, DatabaseError>;
};

export class MempoolLedgerCache extends Context.Tag("MempoolLedgerCache")<
  MempoolLedgerCache,
  MempoolLedgerCacheService
>() {}

export const validationLedgerCacheDeltaApplyCounter = Metric.counter(
  "validation_ledger_cache_delta_apply_count",
  {
    description: "Incremental mempool-ledger cache deltas applied",
    bigint: true,
    incremental: true,
  },
);
export const validationLedgerCacheFullReloadCounter = Metric.counter(
  "validation_ledger_cache_full_reload_count",
  {
    description:
      "Full mempool-ledger cache reloads after startup or a delta gap",
    bigint: true,
    incremental: true,
  },
);
export const validationPhaseBLockWaitTimer = Metric.timer(
  "validation_phase_b_lock_wait_duration",
  "Time validation drain loops wait for the serialized Phase B section",
);

export const makeMempoolLedgerCacheService = (
  globals: Globals,
  loadSpendable: Effect.Effect<
    readonly MempoolLedgerDB.EntryWithTimeStamp[],
    DatabaseError
  >,
): Effect.Effect<MempoolLedgerCacheService> =>
  Effect.gen(function* () {
    const lock = yield* Effect.makeSemaphore(1);
    const claimLock = yield* Effect.makeSemaphore(1);
    const persistenceLock = yield* Effect.makeSemaphore(1);
    const initiallyReady = yield* Deferred.make<void>();
    yield* Deferred.succeed(initiallyReady, undefined);
    const phaseBTail = yield* Ref.make(initiallyReady);
    const persistenceTail = yield* Ref.make(initiallyReady);
    const nextSequence = yield* Ref.make(0n);
    type EpochPoison = {
      readonly epoch: number;
      readonly sequence: bigint;
      readonly priorPersistence: Deferred.Deferred<void>;
    };
    const pipeline = yield* Ref.make<{
      readonly epoch: number;
      readonly poison: EpochPoison | undefined;
    }>({ epoch: 0, poison: undefined });
    const durableState: MempoolLedgerState = new Map();
    const state: MempoolLedgerState = new Map();
    const speculativePatches = new Map<bigint, UTxOStatePatch>();
    let cachedVersion = -1;

    const rebuildCombinedState = (): void => {
      state.clear();
      for (const [outRefHex, output] of durableState) {
        state.set(outRefHex, Buffer.from(output));
      }
      for (const [, patch] of [...speculativePatches].sort(([left], [right]) =>
        left < right ? -1 : left > right ? 1 : 0,
      )) {
        applyUTxOStatePatch(state, patch);
      }
    };

    const contiguousIncrementalEntries = (
      journal: MempoolLedgerDeltaLog,
    ): readonly MempoolLedgerDelta[] | undefined => {
      const needed = journal.entries.filter(
        (entry) => entry.version > cachedVersion,
      );
      return needed.length === journal.version - cachedVersion &&
        needed.every(
          (entry, index) => entry.version === cachedVersion + index + 1,
        ) &&
        needed.every((entry) => !entry.full)
        ? needed
        : undefined;
    };

    const applyIncrementalEntries = (
      entries: readonly MempoolLedgerDelta[],
    ): void => {
      for (const entry of entries) {
        for (const outRefHex of entry.deletes) durableState.delete(outRefHex);
        for (const [outRefHex, output] of entry.upserts) {
          durableState.set(outRefHex, Buffer.from(output));
        }
        cachedVersion = entry.version;
      }
    };

    const fullReload: Effect.Effect<void, DatabaseError> = Effect.gen(
      function* () {
        while (true) {
          // Bracket the DB snapshot with journal reads. A mutator commits its
          // DB change before publishing its delta, so replaying entries newer
          // than `before` closes the load/publish race without ever skipping a
          // committed change. A full marker or ring gap forces another load.
          const before = yield* Ref.get(globals.MEMPOOL_LEDGER_DELTA_LOG);
          const entries = yield* loadSpendable;
          durableState.clear();
          for (const entry of entries) {
            durableState.set(
              entry[MempoolLedgerDB.Columns.OUTREF].toString("hex"),
              Buffer.from(entry[MempoolLedgerDB.Columns.OUTPUT]),
            );
          }
          cachedVersion = before.version;
          yield* Metric.increment(validationLedgerCacheFullReloadCounter);

          const after = yield* Ref.get(globals.MEMPOOL_LEDGER_DELTA_LOG);
          if (after.version === cachedVersion) {
            rebuildCombinedState();
            return;
          }
          const replay = contiguousIncrementalEntries(after);
          if (replay !== undefined) {
            applyIncrementalEntries(replay);
            yield* Metric.incrementBy(
              validationLedgerCacheDeltaApplyCounter,
              BigInt(replay.length),
            );
            rebuildCombinedState();
            return;
          }
        }
      },
    );

    const synchronize: Effect.Effect<void, DatabaseError> = Effect.gen(
      function* () {
        const journal = yield* Ref.get(globals.MEMPOOL_LEDGER_DELTA_LOG);
        if (cachedVersion === journal.version) return;

        if (cachedVersion < 0) {
          yield* fullReload;
          return;
        }
        const needed = contiguousIncrementalEntries(journal);
        if (needed === undefined) {
          yield* fullReload;
          return;
        }
        applyIncrementalEntries(needed);
        rebuildCombinedState();
        yield* Metric.incrementBy(
          validationLedgerCacheDeltaApplyCounter,
          BigInt(needed.length),
        );
      },
    );

    const withPhaseBLock: MempoolLedgerCacheService["withPhaseBLock"] = (
      effect,
    ) =>
      Effect.suspend(() => {
        const startedAt = Date.now();
        return lock.withPermits(1)(
          Effect.sync(() => Date.now() - startedAt).pipe(
            Effect.flatMap((waitMs) =>
              Metric.update(
                validationPhaseBLockWaitTimer,
                Duration.millis(waitMs),
              ),
            ),
            Effect.zipRight(effect),
          ),
        );
      });

    const epochError = (
      epoch: number,
      sequence: bigint,
      failedSequence: bigint,
    ): ValidationPipelineEpochError =>
      new ValidationPipelineEpochError({
        epoch,
        sequence,
        failedSequence,
        message: `Validation epoch ${epoch.toString()} was poisoned at sequence ${failedSequence.toString()}; sequence ${sequence.toString()} cannot advance`,
      });

    const assertSequenceMayAdvance = (
      epoch: number,
      sequence: bigint,
    ): Effect.Effect<void, ValidationPipelineEpochError> =>
      Ref.get(pipeline).pipe(
        Effect.flatMap((current) => {
          if (current.epoch !== epoch) {
            return Effect.fail(epochError(epoch, sequence, sequence));
          }
          if (
            current.poison !== undefined &&
            sequence >= current.poison.sequence
          ) {
            return Effect.fail(
              epochError(epoch, sequence, current.poison.sequence),
            );
          }
          return Effect.void;
        }),
      );

    const poisonEpoch = (
      epoch: number,
      sequence: bigint,
      priorPersistence: Deferred.Deferred<void>,
    ): Effect.Effect<void> =>
      Ref.update(pipeline, (current) => {
        if (current.epoch !== epoch) return current;
        if (
          current.poison !== undefined &&
          current.poison.sequence <= sequence
        ) {
          return current;
        }
        return {
          ...current,
          poison: { epoch, sequence, priorPersistence },
        };
      });

    const recoverPoisonedEpoch: Effect.Effect<void, DatabaseError> = Effect.gen(
      function* () {
        const observed = (yield* Ref.get(pipeline)).poison;
        if (observed === undefined) return;

        // Sequence N may fail while an earlier ordered persistence is still
        // active. Never reload or advance the epoch until every <N write has
        // reached its terminal result.
        yield* Deferred.await(observed.priorPersistence);
        yield* claimLock.withPermits(1)(
          withPhaseBLock(
            persistenceLock.withPermits(1)(
              Effect.gen(function* () {
                const current = yield* Ref.get(pipeline);
                if (current.poison === undefined) return;
                speculativePatches.clear();
                yield* fullReload;
                yield* Ref.set(pipeline, {
                  epoch: current.epoch + 1,
                  poison: undefined,
                });
              }),
            ),
          ),
        );
      },
    );

    const service: MempoolLedgerCacheService = {
      withClaimLock: (effect) => claimLock.withPermits(1)(effect),
      registerPhaseBSequence: Effect.gen(function* () {
        const currentEpoch = (yield* Ref.get(pipeline)).epoch;
        const sequence = yield* Ref.getAndUpdate(
          nextSequence,
          (current) => current + 1n,
        );
        const nextDecision = yield* Deferred.make<void>();
        const previousDecision = yield* Ref.getAndSet(phaseBTail, nextDecision);
        const nextPersistence = yield* Deferred.make<void>();
        const previousPersistence = yield* Ref.getAndSet(
          persistenceTail,
          nextPersistence,
        );
        const decisionCompleted = yield* Ref.make(false);
        const persistenceCompleted = yield* Ref.make(false);
        const persisted = yield* Ref.make(false);
        const finish = (
          completed: Ref.Ref<boolean>,
          deferred: Deferred.Deferred<void>,
        ) =>
          Ref.modify(completed, (alreadyCompleted) =>
            alreadyCompleted
              ? ([false, true] as const)
              : ([true, true] as const),
          ).pipe(
            Effect.flatMap((shouldComplete) =>
              shouldComplete
                ? Deferred.succeed(deferred, undefined).pipe(Effect.asVoid)
                : Effect.void,
            ),
          );
        const finishDecision = finish(decisionCompleted, nextDecision);
        const finishPersistence = finish(persistenceCompleted, nextPersistence);
        const poison = poisonEpoch(currentEpoch, sequence, previousPersistence);
        const protect = <A, E, R>(
          effect: Effect.Effect<A, E, R>,
        ): Effect.Effect<A, E, R> =>
          effect.pipe(
            Effect.catchAllCause((cause) =>
              poison.pipe(Effect.zipRight(Effect.failCause(cause))),
            ),
          );
        return {
          epoch: currentEpoch,
          sequence,
          runDecision: (effect) =>
            Effect.uninterruptibleMask((restore) =>
              restore(
                protect(
                  Deferred.await(previousDecision).pipe(
                    Effect.zipRight(
                      withPhaseBLock(
                        assertSequenceMayAdvance(currentEpoch, sequence).pipe(
                          Effect.zipRight(effect),
                          Effect.tap(() =>
                            assertSequenceMayAdvance(currentEpoch, sequence),
                          ),
                        ),
                      ),
                    ),
                  ),
                ),
              ).pipe(Effect.ensuring(finishDecision)),
            ),
          runPersistence: (effect) =>
            Effect.uninterruptibleMask((restore) =>
              restore(
                protect(
                  Deferred.await(previousPersistence).pipe(
                    Effect.zipRight(
                      persistenceLock.withPermits(1)(
                        assertSequenceMayAdvance(currentEpoch, sequence).pipe(
                          Effect.zipRight(effect),
                        ),
                      ),
                    ),
                    // Persistence and poisoned-epoch recovery both need the
                    // persistence and Phase-B locks. Release the persistence
                    // lock before taking the Phase-B lock so recovery's
                    // claim -> Phase-B -> persistence order cannot deadlock.
                    // Promotion is serialized with full reloads; interruption
                    // after the durable write instead poisons the epoch and
                    // recovers from the committed database state.
                    Effect.tap(() =>
                      withPhaseBLock(
                        assertSequenceMayAdvance(currentEpoch, sequence).pipe(
                          Effect.zipRight(
                            Effect.sync(() => {
                              const patch = speculativePatches.get(sequence);
                              if (patch !== undefined) {
                                applyUTxOStatePatch(durableState, patch);
                              }
                              speculativePatches.delete(sequence);
                            }),
                          ),
                        ),
                      ),
                    ),
                    Effect.tap(() => Ref.set(persisted, true)),
                  ),
                ),
              ).pipe(Effect.ensuring(finishPersistence)),
            ),
          cancel: Ref.get(persisted).pipe(
            Effect.flatMap((wasPersisted) =>
              wasPersisted ? Effect.void : poison,
            ),
            Effect.ensuring(finishDecision),
            Effect.ensuring(finishPersistence),
          ),
        } satisfies PhaseBSequence;
      }),
      withPhaseBLock,
      currentState: synchronize.pipe(Effect.as(state)),
      applyPatchAndSync: (patch) =>
        synchronize.pipe(
          Effect.tap(() =>
            Effect.sync(() => applyUTxOStatePatch(state, patch)),
          ),
        ),
      applySpeculativePatch: (sequence, patch) =>
        synchronize.pipe(
          Effect.tap(() =>
            Effect.sync(() => {
              speculativePatches.set(sequence, patch);
              applyUTxOStatePatch(state, patch);
            }),
          ),
        ),
      recoverPoisonedEpoch,
    };
    return service;
  });

const makeMempoolLedgerCache = Effect.gen(function* () {
  const globals = yield* Globals;
  const sql = yield* SqlClient.SqlClient;
  return yield* makeMempoolLedgerCacheService(
    globals,
    MempoolLedgerDB.retrieveSpendable.pipe(
      Effect.provideService(SqlClient.SqlClient, sql),
    ),
  );
});

export const mempoolLedgerCacheLayer = Layer.effect(
  MempoolLedgerCache,
  makeMempoolLedgerCache,
);
