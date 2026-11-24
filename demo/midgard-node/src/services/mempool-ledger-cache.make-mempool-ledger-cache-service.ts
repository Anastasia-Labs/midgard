import { Deferred, Duration, Effect, Metric, Ref } from "effect";

import { MempoolLedgerDB } from "../database/index.js";
import type { DatabaseError } from "../database/utils/common.js";
import { Globals } from "./globals.globals.js";
import type {
  MempoolLedgerDelta,
  MempoolLedgerDeltaLog,
} from "./globals.next-l1-provider-health-evidence.js";
import {
  type CanonicalCacheRecovery,
  type MempoolLedgerCacheService,
  type PhaseBSequence,
  validationLedgerCacheDeltaApplyCounter,
  validationLedgerCacheEntryWriteCounter,
  validationLedgerCacheFullReloadCounter,
  validationPhaseBLockWaitTimer,
  ValidationPipelineEpochError,
} from "./mempool-ledger-cache.mempool-ledger-cache-service.js";
import {
  contiguousEntriesAfter,
  makeSpendableView,
} from "./mempool-ledger-cache.spendable-view.js";
import { makeWorkingLedger } from "./mempool-ledger-cache.working-ledger.js";

export const makeMempoolLedgerCacheService = (
  globals: Globals,
  loadSpendable: Effect.Effect<
    readonly MempoolLedgerDB.EntryWithTimeStamp[],
    DatabaseError
  >,
  /** The spendable outputs at hex outrefs; a key with none is absent. */
  spendableAt: (
    keys: ReadonlySet<string>,
  ) => Effect.Effect<ReadonlyMap<string, Buffer>, DatabaseError>,
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
      readonly canonicalSuspended: boolean;
    }>({ epoch: 0, poison: undefined, canonicalSuspended: false });
    const ledger = makeWorkingLedger();
    // The delta-log version the durable entries reflect; -1 until first loaded.
    let cachedVersion = -1;

    const countWrites = (written: number) =>
      Metric.incrementBy(
        validationLedgerCacheEntryWriteCounter,
        BigInt(written),
      );

    const contiguousIncrementalEntries = (journal: MempoolLedgerDeltaLog) =>
      contiguousEntriesAfter(journal, cachedVersion);

    /** Applies journal entries to the durable ledger; the entries written. */
    const applyIncrementalEntries = (
      entries: readonly MempoolLedgerDelta[],
    ): number => {
      let written = 0;
      for (const entry of entries) {
        written += ledger.applyDelta(entry);
        cachedVersion = entry.version;
      }
      return written;
    };

    const fullReload: Effect.Effect<void, DatabaseError> = Effect.gen(
      function* () {
        while (true) {
          // Bracket the DB snapshot with journal reads. A mutator commits its
          // DB change before publishing its delta, so replaying entries newer
          // than `before` closes the load/publish race without ever skipping a
          // committed change. A full marker or ring gap forces another load.
          const before = yield* Ref.get(globals.MEMPOOL_LEDGER_DELTA_LOG);
          const written = ledger.loadAll(yield* loadSpendable);
          cachedVersion = before.version;
          yield* Metric.increment(validationLedgerCacheFullReloadCounter);
          yield* countWrites(written);

          const after = yield* Ref.get(globals.MEMPOOL_LEDGER_DELTA_LOG);
          if (after.version === cachedVersion) return;
          const replay = contiguousIncrementalEntries(after);
          if (replay !== undefined) {
            yield* countWrites(applyIncrementalEntries(replay));
            yield* Metric.incrementBy(
              validationLedgerCacheDeltaApplyCounter,
              BigInt(replay.length),
            );
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
        yield* countWrites(applyIncrementalEntries(needed));
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
          if (current.epoch !== epoch || current.canonicalSuspended) {
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
        if (current.epoch !== epoch || current.canonicalSuspended)
          return current;
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
                if (
                  current.epoch !== observed.epoch ||
                  current.poison !== observed ||
                  current.canonicalSuspended
                )
                  return;
                // Every earlier persistence has finished and every later one
                // is fenced, and each commits its ledger effects in one
                // transaction, at keys its patch names. So the durable
                // entries can differ from the table only at the keys of the
                // patches never promoted.
                const unknown = ledger.dropPatches();
                yield* synchronize;
                yield* countWrites(
                  ledger.repair(unknown, yield* spendableAt(unknown)),
                );
                // Canonical retirement can arrive while the reads await SQL.
                // An older poisoned recovery must never clear that suspension.
                yield* Ref.update(pipeline, (latest) =>
                  latest.epoch === current.epoch &&
                  latest.poison === observed &&
                  !latest.canonicalSuspended
                    ? {
                        epoch: current.epoch + 1,
                        poison: undefined,
                        canonicalSuspended: false,
                      }
                    : latest,
                );
              }),
            ),
          ),
        );
      },
    );

    const requireCanonicalReady = Ref.get(pipeline).pipe(
      Effect.flatMap((current) =>
        current.canonicalSuspended
          ? Effect.fail(
              new ValidationPipelineEpochError({
                epoch: current.epoch,
                sequence: 0n,
                failedSequence: 0n,
                message:
                  "Validation cache is suspended for canonical history recovery",
              }),
            )
          : Effect.void,
      ),
    );

    const retireCanonicalEpoch = Effect.gen(function* () {
      const retired = yield* Ref.updateAndGet(pipeline, (current) => ({
        epoch: current.epoch + 1,
        poison: undefined,
        canonicalSuspended: true,
      }));
      const requireCurrent = Ref.get(pipeline).pipe(
        Effect.flatMap((current) =>
          current.epoch === retired.epoch && current.canonicalSuspended
            ? Effect.void
            : Effect.fail(epochError(retired.epoch, 0n, 0n)),
        ),
      );
      return {
        epoch: retired.epoch,
        runRecovery: (repairDurable, publishReady, durableChanges) =>
          claimLock.withPermits(1)(
            withPhaseBLock(
              persistenceLock.withPermits(1)(
                Effect.gen(function* () {
                  yield* requireCurrent;
                  const result = yield* repairDurable;
                  yield* requireCurrent;
                  const dropped = ledger.dropPatches();
                  const changes = durableChanges?.(result);
                  if (changes === undefined) yield* fullReload;
                  else {
                    // The repair publishes no delta: the journal brings the
                    // state to the repair's start, then its changed keys
                    // are read again.
                    yield* synchronize;
                    yield* countWrites(
                      ledger.repair(changes.keys, changes.spendable),
                    );
                    // A dropped patch's persistence may have committed
                    // while this retirement stopped its promotion, so its
                    // keys are read again as well.
                    yield* countWrites(
                      ledger.repair(dropped, yield* spendableAt(dropped)),
                    );
                  }
                  yield* requireCurrent;
                  // Parked retired Phase-A jobs retain old ordering tails.
                  // Their epoch checks fence writes; fresh work must not wait
                  // for their eventual worker response or cancellation.
                  const ready = yield* Deferred.make<void>();
                  yield* Deferred.succeed(ready, undefined);
                  yield* Ref.set(phaseBTail, ready);
                  yield* Ref.set(persistenceTail, ready);
                  yield* publishReady;
                  const resumed = yield* Ref.modify(pipeline, (current) =>
                    current.epoch === retired.epoch &&
                    current.canonicalSuspended
                      ? ([
                          true,
                          { ...current, canonicalSuspended: false },
                        ] as const)
                      : ([false, current] as const),
                  );
                  if (!resumed)
                    return yield* Effect.fail(
                      epochError(retired.epoch, 0n, 0n),
                    );
                  return result;
                }),
              ),
            ),
          ),
      } satisfies CanonicalCacheRecovery;
    });

    const spendableView = makeSpendableView({
      pipeline: Ref.get(pipeline),
      journal: Ref.get(globals.MEMPOOL_LEDGER_DELTA_LOG),
      synced: () => ({ state: ledger.lookup, version: cachedVersion }),
      entriesAfter: contiguousEntriesAfter,
    });

    const service: MempoolLedgerCacheService = {
      withClaimLock: (effect) =>
        claimLock.withPermits(1)(
          requireCanonicalReady.pipe(Effect.zipRight(effect)),
        ),
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
                            Effect.sync(() => ledger.promote(sequence)),
                          ),
                          Effect.flatMap(countWrites),
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
      currentState: requireCanonicalReady.pipe(
        Effect.zipRight(synchronize),
        Effect.zipRight(requireCanonicalReady),
        Effect.as(ledger.lookup),
      ),
      applySpeculativePatch: (sequence, patch) =>
        requireCanonicalReady.pipe(
          Effect.zipRight(synchronize),
          Effect.zipRight(requireCanonicalReady),
          Effect.tap(() => Effect.sync(() => ledger.setPatch(sequence, patch))),
        ),
      recoverPoisonedEpoch,
      retireCanonicalEpoch,
      spendableView,
    };
    return service;
  });
