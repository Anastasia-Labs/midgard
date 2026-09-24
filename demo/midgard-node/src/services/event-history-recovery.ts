import { randomUUID } from "node:crypto";

import { Context, Data, Deferred, Effect } from "effect";

import * as Authority from "../database/eventHistoryAuthority.js";
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

    const handle = (
      expected: number,
      recoveryToken: Authority.Token,
      cache: CanonicalCacheRecovery,
    ) => {
      const afterProducerDrain = <A, E, R>(work: Effect.Effect<A, E, R>) =>
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
      /** Called immediately on rollback or lost source authority. SQL
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
                token = yield* Authority.beginRecovery(token, reason);
                return token;
              }),
            );
            return handle(expected, next, cache);
          }),
        ),
      /** The source owner must renew only while its monitor is healthy. A
       * failed renewal immediately retires local readiness; expiry still
       * fences SQL independently if this process stops executing altogether. */
      renew: control
        .withPermits(1)(
          Effect.gen(function* () {
            if (closed) return yield* Effect.fail(unavailable());
            yield* Authority.renew(token, input.leaseDurationMs);
          }),
        )
        .pipe(
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
