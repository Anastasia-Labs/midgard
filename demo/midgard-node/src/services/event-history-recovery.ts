import { randomUUID } from "node:crypto";

import { Context, Data, Deferred, Effect, Option } from "effect";

import * as Authority from "../database/eventHistoryAuthority.js";
import { reclaimLapsedLease } from "../database/eventHistoryAuthority.reclaim-lapsed-lease.js";
import type { DatabaseError } from "../database/utils/common.js";
import type {
  CanonicalCacheRecovery,
  MempoolLedgerCacheService,
} from "./mempool-ledger-cache.js";

export class HistoryRecoverySuperseded extends Data.TaggedError(
  "HistoryRecoverySuperseded",
)<{ readonly message: string }> {}

type Capture = Parameters<typeof Authority.publishReady>[1];

/** Coordinates recovery once the node's source owner has established canonical
 * branch and deployment provenance. It does not authenticate a capture itself.
 * Every producer must register its whole lifetime, including postcommit deltas;
 * every SQL mutation must separately use Authority.withReady as its OUTERMOST
 * transaction. Never wrap a producer's network/worker lifetime in that SQL gate.
 */
export type HistoryRecoveryPreparation = Readonly<{
  token: Authority.Token;
  assertCurrent: Effect.Effect<void, HistoryRecoverySuperseded>;
}>;

/** Available only during the owner's source-checked recovery preparation. */
export const HistoryPreparation =
  Context.GenericTag<HistoryRecoveryPreparation>(
    "midgard/HistoryRecoveryPreparation",
  );

export const makeEventHistoryRecovery = (input: {
  readonly deploymentIdentity: string;
  readonly ownerToken?: string;
  readonly leaseDurationMs: number;
  readonly cache: MempoolLedgerCacheService;
  /** Drain deferred persistence after producers stop, before any inverse SQL.
   * This runs outside SQL and must succeed before recovery can become Ready. */
  readonly drainBeforeRepair?: Effect.Effect<void, DatabaseError>;
}) =>
  Effect.gen(function* () {
    const control = yield* Effect.makeSemaphore(1);
    const repairs = yield* Effect.makeSemaphore(1);
    let revision = 0;
    let closed = false;
    let ready: Authority.Token | undefined;
    const producers = new Set<Deferred.Deferred<void>>();
    const recoveries = new Set<Deferred.Deferred<void>>();
    const closeDone = yield* Deferred.make<void, DatabaseError>();
    // Startup never exposes persisted Ready or an unretired local cache.
    const initialCache = yield* input.cache.retireCanonicalEpoch;
    let token = yield* Authority.acquire({
      deploymentIdentity: input.deploymentIdentity,
      ownerToken: input.ownerToken ?? randomUUID(),
      leaseDurationMs: input.leaseDurationMs,
    });

    const unavailable = () =>
      new HistoryRecoverySuperseded({
        message: "Canonical history recovery or producer was superseded",
      });
    const requireRevision = (expected: number) =>
      Effect.suspend(() =>
        !closed && revision === expected
          ? Effect.void
          : Effect.fail(unavailable()),
      );
    // A lease that lapsed while this process held it, and that nobody else
    // claimed, is re-taken as a new Recovering generation, as a restart would
    // re-take it, instead of stopping the owner. One attempt per failure,
    // under the control lock, and only while `held` is still the current
    // generation: a live, foreign or suspended lease is never taken, and then
    // the original failure stands. A reclaim supersedes every handle and the
    // readiness of the lapsed generation, so the owner reconnects into one
    // fresh recovery.
    const reclaimLapsed = (expected: number, held: Authority.Token) =>
      Effect.uninterruptible(
        control.withPermits(1)(
          Effect.suspend(() =>
            closed || revision !== expected || token !== held
              ? Effect.succeed(false)
              : reclaimLapsedLease(
                  held,
                  input.leaseDurationMs,
                  "history lease lapsed and was reclaimed",
                ).pipe(
                  Effect.map(
                    Option.match({
                      onNone: () => false,
                      onSome: (next) => {
                        token = next;
                        ready = undefined;
                        revision += 1;
                        return true;
                      },
                    }),
                  ),
                ),
          ),
        ),
      ).pipe(
        Effect.orElseSucceed(() => false),
        Effect.tap((reclaimed) =>
          reclaimed ? input.cache.retireCanonicalEpoch : Effect.void,
        ),
      );
    const supersedeIfLapsed = <A, E, R>(
      expected: number,
      held: Authority.Token,
      work: Effect.Effect<A, E, R>,
    ) =>
      work.pipe(
        Effect.catchAll((error) =>
          reclaimLapsed(expected, held).pipe(
            Effect.flatMap(
              (
                reclaimed,
              ): Effect.Effect<never, E | HistoryRecoverySuperseded> =>
                reclaimed
                  ? Effect.fail(
                      new HistoryRecoverySuperseded({
                        message:
                          "History lease lapsed while held and was reclaimed as a new recovery generation",
                      }),
                    )
                  : Effect.fail(error),
            ),
          ),
        ),
      );

    const handle = (
      expected: number,
      recoveryToken: Authority.Token,
      cache: CanonicalCacheRecovery,
    ) => {
      const drainThen = <A, E, R>(work: Effect.Effect<A, E, R>) =>
        Effect.acquireUseRelease(
          Effect.gen(function* () {
            const done = yield* Deferred.make<void>();
            yield* requireRevision(expected);
            recoveries.add(done);
            return done;
          }),
          () =>
            Effect.gen(function* () {
              yield* requireRevision(expected);
              // Drain whole producer lifetimes, including postcommit publication.
              // Neither the authority lock nor a cache lock is held while waiting.
              yield* Effect.all([...producers].map(Deferred.await), {
                concurrency: "unbounded",
                discard: true,
              });
              yield* requireRevision(expected);
              // A receipt batch cannot interleave between the final repair,
              // cache reload and Ready publication of complete().
              return yield* repairs.withPermits(1)(
                requireRevision(expected).pipe(
                  Effect.zipRight(input.drainBeforeRepair ?? Effect.void),
                  Effect.zipRight(requireRevision(expected)),
                  Effect.zipRight(work),
                ),
              );
            }),
          (done) =>
            Effect.gen(function* () {
              recoveries.delete(done);
              yield* Deferred.succeed(done, undefined);
            }),
        );
      const afterProducerDrain = <A, E, R>(work: Effect.Effect<A, E, R>) =>
        supersedeIfLapsed(expected, recoveryToken, drainThen(work));
      const ownedRepair = <A, E, R>(repair: Effect.Effect<A, E, R>) =>
        Authority.withRecovery(
          recoveryToken,
          requireRevision(expected).pipe(
            Effect.zipRight(repair),
            Effect.tap(() => requireRevision(expected)),
          ),
        );

      return {
        /** Prepare native/worker recovery while producers remain drained. This
         * deliberately holds no SQL transaction or cache lock. Any bounded SQL
         * work must separately use withRecovery(token, assertCurrent + work).
         * Preparation cannot publish Ready or bypass the subsequent SQL CAS.
         */
        prepare: <A, E, R>(
          work: (
            preparation: HistoryRecoveryPreparation,
          ) => Effect.Effect<A, E, R>,
        ) =>
          afterProducerDrain(
            requireRevision(expected).pipe(
              Effect.zipRight(
                work({
                  token: recoveryToken,
                  assertCurrent: requireRevision(expected),
                }),
              ),
              Effect.tap(() => requireRevision(expected)),
            ),
          ),
        /** Commit a bounded replay/undo batch while remaining Recovering. The
         * owner serializes batches and fetches evidence before entering this
         * fence. No network, workers, cache locks or delta publication inside.
         * A later complete reloads caches and publishes Ready only at convergence.
         */
        persist: <A, E, R>(repair: Effect.Effect<A, E, R>) =>
          afterProducerDrain(
            Effect.uninterruptible(
              ownedRepair(repair).pipe(
                Effect.tap(() => requireRevision(expected)),
              ),
            ),
          ),
        /** Fetch and verify evidence before completion. Repair may perform only
         * bounded SQL work; no cache locks, network, workers or delta publication.
         * Failed repair remains fenced and can be retried on this same handle.
         */
        complete: <A, E, R>(capture: Capture, repair: Effect.Effect<A, E, R>) =>
          afterProducerDrain(
            // Finish bounded SQL/cache repair and readiness publication even if
            // cancelled at COMMIT. Evidence fetch and producer draining above
            // remain interruptible.
            Effect.uninterruptible(
              Effect.gen(function* () {
                const result = yield* cache.runRecovery(
                  ownedRepair(repair),
                  requireRevision(expected).pipe(
                    Effect.zipRight(
                      Authority.publishReady(recoveryToken, capture),
                    ),
                  ),
                );
                // No asynchronous boundary between the final generation check
                // and local readiness. A later signal clears Ready before SQL.
                yield* Effect.suspend(() => {
                  if (closed || revision !== expected)
                    return Effect.fail(unavailable());
                  ready = recoveryToken;
                  return Effect.void;
                });
                return result;
              }),
            ),
          ),
      };
    };

    const close = Effect.uninterruptible(
      Effect.suspend(() => {
        if (closed) return Deferred.await(closeDone);
        closed = true;
        ready = undefined;
        revision += 1;
        return Effect.gen(function* () {
          yield* input.cache.retireCanonicalEpoch;
          // Source/worker shutdown must cancel outstanding work before joining
          // close. No SQL/cache/control lock is held while those lifetimes drain.
          const drain = Effect.all(
            [...producers, ...recoveries].map(Deferred.await),
            {
              concurrency: "unbounded",
              discard: true,
            },
          );
          yield* control
            .withPermits(1)(Effect.suspend(() => Authority.release(token)))
            .pipe(Effect.ensuring(drain));
        }).pipe(
          Effect.exit,
          Effect.flatMap((exit) => Deferred.done(closeDone, exit)),
          Effect.zipRight(Deferred.await(closeDone)),
        );
      }),
    );
    yield* Effect.addFinalizer(() => close.pipe(Effect.orDie));

    return {
      startup: handle(revision, token, initialCache),
      close,
      /** Called immediately on rollback, lost source authority, or an append
       * that cannot complete without dependent repair; never for a plain
       * forward block or tip advance (see append). SQL
       * revocation commits before the returned handle can drain producers. */
      beginRecovery: (reason: string) =>
        Effect.uninterruptible(
          Effect.gen(function* () {
            if (closed) return yield* Effect.fail(unavailable());
            ready = undefined;
            const expected = ++revision;
            const cache = yield* input.cache.retireCanonicalEpoch;
            const next = yield* control.withPermits(1)(
              Effect.gen(function* () {
                yield* requireRevision(expected);
                const lapsed = token;
                token = yield* Authority.beginRecovery(lapsed, reason).pipe(
                  Effect.catchAll((error) =>
                    reclaimLapsedLease(
                      lapsed,
                      input.leaseDurationMs,
                      reason,
                    ).pipe(
                      Effect.orElseFail(() => error),
                      Effect.flatMap(
                        Option.match({
                          onNone: () => Effect.fail(error),
                          onSome: Effect.succeed,
                        }),
                      ),
                    ),
                  ),
                );
                return token;
              }),
            );
            return handle(expected, next, cache);
          }),
        ),
      /** Journal one forward block at the head of the current Ready
       * generation. Producers are not drained and keep their registration:
       * they hold a journaled prefix this only extends, and the authority row
       * lock serializes the append with each of their SQL writes. No cache lock
       * and no reload: the work must leave spendable cache state untouched, or
       * fail (rolling back) so the owner can begin a recovery instead.
       */
      append: <A, E, R>(work: Effect.Effect<A, E, R>) =>
        Effect.suspend(() => {
          const token = ready;
          const expected = revision;
          if (closed || token === undefined) return Effect.fail(unavailable());
          return supersedeIfLapsed(
            expected,
            token,
            Effect.uninterruptible(
              Authority.withReadyAppend(
                token,
                requireRevision(expected).pipe(
                  Effect.zipRight(work),
                  Effect.tap(() => requireRevision(expected)),
                ),
              ),
            ),
          );
        }),
      /** The source owner must renew only while its monitor is healthy. A
       * failed renewal immediately retires local readiness; expiry still
       * fences SQL independently if this process stops executing altogether. */
      renew: Effect.suspend(() =>
        supersedeIfLapsed(
          revision,
          token,
          control.withPermits(1)(
            Effect.gen(function* () {
              if (closed) return yield* Effect.fail(unavailable());
              yield* Authority.renew(token, input.leaseDurationMs);
            }),
          ),
        ),
      ).pipe(
        Effect.onError(() =>
          Effect.gen(function* () {
            ready = undefined;
            revision += 1;
            yield* input.cache.retireCanonicalEpoch;
          }),
        ),
      ),
      /** Register before claims/fetch/build/SQL, release after final publication.
       * The callback token is immutable; never refresh it midway through work.
       * Use assertCurrent at asynchronous boundaries, and withReady at each
       * actual SQL commit. Registration alone is not a transaction fence. */
      runProducer: <A, E, R>(
        work: (
          token: Authority.Token,
          assertCurrent: Effect.Effect<void, HistoryRecoverySuperseded>,
        ) => Effect.Effect<A, E, R>,
      ) =>
        Effect.acquireUseRelease(
          Effect.gen(function* () {
            const done = yield* Deferred.make<void>();
            if (closed || ready === undefined)
              return yield* Effect.fail(unavailable());
            const registered = { done, token: ready, revision };
            producers.add(done);
            return registered;
          }),
          (registered) => {
            const assertCurrent = requireRevision(registered.revision);
            return Authority.withReady(registered.token, Effect.void).pipe(
              Effect.zipRight(assertCurrent),
              Effect.zipRight(work(registered.token, assertCurrent)),
              Effect.tap(() => assertCurrent),
            );
          },
          ({ done }) =>
            Effect.gen(function* () {
              producers.delete(done);
              yield* Deferred.succeed(done, undefined);
            }),
        ),
    };
  });
