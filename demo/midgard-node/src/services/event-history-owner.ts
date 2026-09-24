import { setTimeout as delay } from "node:timers/promises";

import type { OutRefLike } from "@al-ft/midgard-core/out-ref";
import type * as SDK from "@al-ft/midgard-sdk";
import { Data, Effect, Runtime } from "effect";

import type * as Authority from "../database/eventHistoryAuthority.js";
import * as Journal from "../database/eventHistoryJournal.js";
import * as ReplayReceipts from "../database/eventHistoryReplayReceipts.js";
import type { DatabaseError } from "../database/utils/common.js";
import type { HistoryChainTip } from "../l1-event-history-chain.js";
import {
  advanceEventHistoryListReplay,
  beginEventHistoryListReplay,
  type EventHistoryListReplay,
  joinEventHistoryListReplay,
} from "../l1-event-history-list-replay.js";
import { projectEventHistoryBlock } from "../l1-event-history-projection.js";
import { verifyEventHistoryReferenceBody } from "../l1-event-history-reference.js";
import {
  type BoundHistoryChainBlock,
  type EventHistorySourceBinding,
  followBoundEventHistoryChain,
  readBoundEventHistoryLedgerSnapshot,
} from "../l1-event-history-source.js";
import {
  type HistoryTransportOptions,
  locateEventHistoryActivation,
  readEventHistoryCreatingBody,
} from "../l1-event-history-transport.js";
import type { LedgerSnapshotPoint } from "../l1-ledger-snapshot.js";
import type { Database } from "./database.js";
import {
  type HistoryRecoveryPreparation,
  HistoryRecoverySuperseded,
  makeEventHistoryRecovery,
} from "./event-history-recovery.js";
import type { MempoolLedgerCacheService } from "./mempool-ledger-cache.js";

export class HistoryOwnerUnavailable extends Data.TaggedError(
  "HistoryOwnerUnavailable",
)<{ readonly cause: unknown }> {}

export type HistoryOwnerChange = Readonly<{
  kind: "seed" | "forward" | "rollback" | "resume";
  before: Journal.Checkpoint | null;
  after: Journal.Checkpoint;
}>;

/** The source can continue collecting canonical evidence while producers stay
 * fenced. Pending reconciliation must perform no dependent ledger mutations.
 */
export type HistoryReconciliationPending = Readonly<{
  status: "pending";
  reason: string;
}>;

export type HistoryOwnerCoverage = Readonly<{
  bindingDigest: string;
  checkpointRevision: string;
  point: LedgerSnapshotPoint;
  snapshotDigest: string;
  includedThroughMs: number;
}>;

const samePoint = (a: LedgerSnapshotPoint, b: LedgerSnapshotPoint) =>
  a.id === b.id && a.slot === b.slot;
class MissingBody extends Error {
  constructor(readonly txHash: string) {
    super(`Missing history creating body ${txHash}`);
  }
}

/** One scoped source owner. Only low-level transports are replaceable: capture,
 * source authentication, whole-block replay, projection and SQL fencing always
 * run through the production implementations. Persisted state is local recovery
 * material; every start freshly authenticates and intersects the source.
 *
 * reconcile is mandatory bounded SQL work inside the same recovery transaction
 * as the fresh journal image. It must repair dependent state or fail. In
 * particular, a rollback cannot succeed with orphan-funded L2 descendants.
 * Network/worker repair preparation belongs outside that transaction. This
 * private owner is not a substitute for wiring every producer through its gate.
 */
export const makeEventHistoryOwner = <E, R>(input: {
  readonly binding: EventHistorySourceBinding;
  readonly histories: SDK.EventHistoryContractPair;
  readonly slotToUnixTime: (slot: number) => number;
  readonly transport: Omit<HistoryTransportOptions, "signal">;
  readonly heartbeatIntervalMs: number;
  readonly retainedPointLimit: number;
  readonly maximumReceiptBytes: number;
  readonly leaseDurationMs: number;
  readonly ownerToken?: string;
  readonly expectedInitializationTransactionHash?: string;
  readonly cache: MempoolLedgerCacheService;
  readonly reconcile: (
    change: HistoryOwnerChange,
  ) => Effect.Effect<void | HistoryReconciliationPending, E, R>;
  readonly drainBeforeRepair?: Effect.Effect<void, DatabaseError>;
  /** Outside SQL under the same drained recovery generation, before cache reload
   * and Ready. Final SQL reconciliation still checks the exact checkpoint.
   */
  /** Pending dependent recovery may collect evidence and perform native repair
   * outside SQL. It cannot grant readiness; reconcile must subsequently clear
   * the durable pending condition under the same current checkpoint. */
  readonly preparePendingReconciliation?: (
    checkpoint: Journal.Checkpoint,
    preparation: HistoryRecoveryPreparation,
  ) => Effect.Effect<void, E, R>;
  readonly prepareCompletion?: (
    checkpoint: Journal.Checkpoint,
    preparation: HistoryRecoveryPreparation,
  ) => Effect.Effect<void, E, R>;
}) =>
  Effect.gen(function* () {
    if (
      !Number.isSafeInteger(input.heartbeatIntervalMs) ||
      input.heartbeatIntervalMs <= 0 ||
      !Number.isSafeInteger(input.transport.timeoutMs) ||
      input.transport.timeoutMs <= 0 ||
      !Number.isSafeInteger(input.leaseDurationMs) ||
      input.leaseDurationMs <=
        input.heartbeatIntervalMs + input.transport.timeoutMs
    )
      return yield* Effect.fail(
        new HistoryOwnerUnavailable({
          cause:
            "History lease must exceed its heartbeat interval plus source deadline",
        }),
      );
    const runtime = yield* Effect.runtime<Database | R>();
    const run = Runtime.runPromise(runtime);
    const recovery = yield* makeEventHistoryRecovery({
      deploymentIdentity: input.binding.manifestId,
      ownerToken: input.ownerToken,
      leaseDurationMs: input.leaseDurationMs,
      cache: input.cache,
      drainBeforeRepair: input.drainBeforeRepair,
    });
    const controller = new AbortController();
    const signal = controller.signal;
    const startupMonitor = new AbortController();
    const startupSignal = AbortSignal.any([signal, startupMonitor.signal]);
    const transport = { ...input.transport, signal };
    let checkpoint: Journal.Checkpoint | null = null;
    let replay: EventHistoryListReplay | undefined;
    let tip: HistoryChainTip | "origin" | undefined;
    let epoch = 0;
    let ready = false;
    let pendingReconciliation: HistoryReconciliationPending | undefined;
    let published = false;
    let closing = false;
    let failed = false;
    let failure: unknown;
    let handle = Promise.resolve(recovery.startup);
    let queue: Promise<void> = Promise.resolve();
    let convergenceQueued = false;
    const readinessWaiters = new Set<() => void>();
    const notifyReadiness = () => {
      for (const notify of [...readinessWaiters]) notify();
    };
    const coverage = (): HistoryOwnerCoverage => {
      if (checkpoint === null) throw new Error("History checkpoint is missing");
      const includedThroughMs = input.slotToUnixTime(checkpoint.head.slot);
      if (!Number.isSafeInteger(includedThroughMs))
        throw new Error("History checkpoint has an invalid time mapping");
      return Object.freeze({
        bindingDigest: checkpoint.bindingDigest,
        checkpointRevision: checkpoint.revision,
        point: Object.freeze({ ...checkpoint.head }),
        snapshotDigest: checkpoint.capture.snapshotDigest,
        includedThroughMs,
      });
    };
    let activation:
      | Awaited<ReturnType<typeof locateEventHistoryActivation>>
      | undefined;
    let renewal: Promise<void> | undefined;
    let resolveFirstReady!: () => void;
    let rejectFirstReady!: (cause: unknown) => void;
    const firstReady = new Promise<void>((resolve, reject) => {
      resolveFirstReady = resolve;
      rejectFirstReady = reject;
    });
    // A caller may await readiness only after startup has already failed.
    void firstReady.catch(() => undefined);

    const invalidate = (reason: string) => {
      ready = false;
      epoch += 1;
      if (published) {
        published = false;
        handle = run(recovery.beginRecovery(reason));
        void handle.catch(fail);
      }
    };
    const fail = (cause: unknown) => {
      if (closing || failed) return;
      failed = true;
      failure = cause;
      invalidate("history source unavailable");
      controller.abort(cause);
      rejectFirstReady(cause);
      notifyReadiness();
    };
    const enqueue = (work: () => Promise<void>) => {
      const next = queue.then(async () => {
        signal.throwIfAborted();
        await work();
      });
      queue = next.catch(fail);
      return next;
    };
    const capture = (parentSignal = signal) =>
      readBoundEventHistoryLedgerSnapshot({
        binding: input.binding,
        ogmiosUrl: input.transport.ogmiosUrl,
        timeoutMs: input.transport.timeoutMs,
        webSocketFactory: input.transport.webSocketFactory,
        signal: AbortSignal.any([
          parentSignal,
          AbortSignal.timeout(input.transport.timeoutMs),
        ]),
      });
    // Locating an old activation can outlive one lease. Renew only after a
    // fresh, source-authenticated complete capture, never from Kupo navigation
    // or a cached receipt. The follower's heartbeats replace this startup loop.
    const startupHealth = (async () => {
      try {
        while (true) {
          await delay(input.heartbeatIntervalMs, undefined, {
            signal: startupSignal,
          });
          await capture(startupSignal);
          startupSignal.throwIfAborted();
          await run(recovery.renew);
        }
      } catch (cause) {
        if (!startupSignal.aborted) fail(cause);
      }
    })();
    const requireCheckpoint = Effect.gen(function* () {
      const value = yield* Journal.load(input.binding);
      if (value === null)
        return yield* Effect.fail(
          new HistoryOwnerUnavailable({
            cause: "History checkpoint is missing",
          }),
        );
      return value;
    });
    const reconciled = (
      kind: HistoryOwnerChange["kind"],
      before: Journal.Checkpoint | null,
    ) =>
      requireCheckpoint.pipe(
        Effect.tap((after) =>
          input.reconcile({ kind, before, after }).pipe(
            Effect.tap((result) =>
              Effect.sync(() => {
                pendingReconciliation = result ?? undefined;
              }),
            ),
          ),
        ),
      );

    // A per-block, lazy body cache: tracked outputs resolve first, and only
    // references actually needed by transition decoding trigger archive reads.
    const withBodies = async <A>(
      block: BoundHistoryChainBlock,
      work: (body: (txHash: string) => string) => A | Promise<A>,
    ): Promise<A> => {
      const bodies = new Map<string, string>();
      const getBody = (txHash: string) => {
        const value = bodies.get(txHash);
        if (value === undefined) throw new MissingBody(txHash);
        return value;
      };
      while (true) {
        signal.throwIfAborted();
        try {
          return await work(getBody);
        } catch (cause) {
          if (!(cause instanceof MissingBody)) throw cause;
          const refs = new Map<string, OutRefLike>();
          for (const transaction of block.transactions)
            for (const ref of transaction.references)
              if (ref.txHash === cause.txHash)
                refs.set(`${ref.txHash}#${ref.outputIndex}`, ref);
          if (refs.size === 0) throw cause;
          let lastFailure: unknown = cause;
          for (const ref of refs.values()) {
            try {
              bodies.set(
                cause.txHash,
                await readEventHistoryCreatingBody(transport, ref),
              );
              break;
            } catch (error) {
              signal.throwIfAborted();
              lastFailure = error;
            }
          }
          if (!bodies.has(cause.txHash)) throw lastFailure;
        }
      }
    };

    const converge = async () => {
      if (ready || tip === undefined || tip === "origin") return;
      const head = checkpoint?.head ?? replay?.point;
      if (head === undefined || !samePoint(head, tip)) return;
      const expected = epoch;
      const fresh = await capture();
      signal.throwIfAborted();
      if (expected !== epoch || !samePoint(fresh.history.ledger.point, head))
        return;
      const active = await handle;
      if (checkpoint === null) {
        if (replay === undefined) throw new Error("History replay is missing");
        const origin = joinEventHistoryListReplay({
          state: replay,
          capture: fresh,
          binding: input.binding,
        });
        checkpoint = await run(
          active.persist(
            Journal.seed({ binding: input.binding, ...origin }).pipe(
              Effect.zipRight(reconciled("seed", null)),
            ),
          ),
        );
        replay = undefined;
      } else if (checkpoint.capture.snapshotDigest !== fresh.snapshotDigest) {
        throw new Error(
          "Fresh history capture disagrees with replayed journal",
        );
      }
      if (expected !== epoch) return;
      // On restart the journal may already be at the source intersection. Recheck
      // dependent state under recovery authority before any native preparation;
      // an in-memory pending flag from a previous process is not authority.
      checkpoint = await run(active.persist(reconciled("resume", checkpoint)));
      if (expected !== epoch) return;
      const prepare = async (
        work: NonNullable<typeof input.prepareCompletion>,
      ) => {
        const preparing = checkpoint!;
        const sourceCurrent = Effect.suspend(() =>
          expected === epoch && !signal.aborted
            ? Effect.void
            : Effect.fail(
                new HistoryRecoverySuperseded({
                  message: "History source changed during recovery preparation",
                }),
              ),
        );
        await run(
          active.prepare((preparation) =>
            sourceCurrent.pipe(
              Effect.zipRight(
                work(preparing, {
                  token: preparation.token,
                  assertCurrent: sourceCurrent.pipe(
                    Effect.zipRight(preparation.assertCurrent),
                  ),
                }),
              ),
              Effect.tap(() => sourceCurrent),
            ),
          ),
          { signal },
        );
      };
      try {
        if (pendingReconciliation !== undefined) {
          if (input.preparePendingReconciliation === undefined) return;
          await prepare(input.preparePendingReconciliation);
          if (expected !== epoch) return;
          checkpoint = await run(
            active.persist(reconciled("resume", checkpoint)),
          );
          if (expected !== epoch || pendingReconciliation !== undefined) return;
        }
        if (input.prepareCompletion !== undefined)
          await prepare(input.prepareCompletion);
      } catch (cause) {
        if (expected !== epoch && !signal.aborted) return;
        throw cause;
      }
      if (expected !== epoch) return;
      // Mark before completion so a source signal can supersede publication
      // during cache reload. The local gate opens only after all checks below.
      published = true;
      try {
        await run(
          active.complete(
            { point: head, snapshotDigest: fresh.snapshotDigest },
            reconciled("resume", checkpoint).pipe(
              Effect.tap(() =>
                pendingReconciliation === undefined
                  ? Effect.void
                  : Effect.fail(
                      new HistoryOwnerUnavailable({
                        cause: pendingReconciliation.reason,
                      }),
                    ),
              ),
            ),
          ),
        );
      } catch (cause) {
        if (expected !== epoch && !signal.aborted) return;
        throw cause;
      }
      signal.throwIfAborted();
      if (expected !== epoch) return;
      ready = true;
      resolveFirstReady();
      notifyReadiness();
    };

    const forward = async (block: BoundHistoryChainBlock) => {
      const active = await handle;
      if (checkpoint === null) {
        const step = await withBodies(block, (getCreatingBody) => {
          const options = {
            block,
            binding: input.binding,
            histories: input.histories,
            slotToUnixTime: input.slotToUnixTime,
            maximumBodyBytes: input.transport.maximumTransactionBytes,
            getCreatingBody,
          };
          return replay === undefined
            ? beginEventHistoryListReplay(options)
            : advanceEventHistoryListReplay({ ...options, previous: replay });
        });
        if (
          replay === undefined &&
          (activation === undefined ||
            !samePoint(step.state.activation.point, activation.point) ||
            step.state.activation.transactionHash !==
              activation.transactionHash)
        )
          throw new Error(
            "History activation changed since locating its predecessor",
          );
        await run(
          active.persist(
            ReplayReceipts.put({
              binding: input.binding,
              block,
              receipt: step.receipt,
              replay: step.state,
              maximumReceiptBytes: input.maximumReceiptBytes,
            }),
          ),
        );
        replay = step.state;
      } else {
        const before = checkpoint;
        const transactions = new Map(
          block.transactions.map((tx) => [tx.txHash, tx]),
        );
        const projection = await withBodies(block, (body) =>
          projectEventHistoryBlock({
            previous: before.capture,
            block,
            binding: input.binding,
            histories: input.histories,
            slotToUnixTime: input.slotToUnixTime,
            resolveReference: (txHash, ref) => {
              const transaction = transactions.get(txHash);
              if (transaction === undefined)
                throw new Error("Missing observing transaction");
              return verifyEventHistoryReferenceBody({
                transaction,
                ref,
                creatingBodyCbor: body(ref.txHash),
                maximumBodyBytes: input.transport.maximumTransactionBytes,
              });
            },
          }),
        );
        const prepared = Journal.prepareAppend(before, block, projection);
        checkpoint = await run(
          active.persist(
            Journal.append(
              input.binding,
              prepared,
              reconciled("forward", before),
            ).pipe(Effect.zipRight(requireCheckpoint)),
          ),
        );
      }
      await converge();
    };
    const rewind = async (point: LedgerSnapshotPoint | "origin") => {
      if (checkpoint === null || point === "origin")
        throw new Error("History rollback requires a retained journal anchor");
      const active = await handle;
      while (!samePoint(checkpoint.head, point)) {
        signal.throwIfAborted();
        if (
          checkpoint.head.slot <= point.slot ||
          checkpoint.headApplicationRevision === null
        )
          throw new Error("History rollback exceeds retained journal ancestry");
        const before: Journal.Checkpoint = checkpoint;
        checkpoint = await run(
          active.persist(
            Journal.undoHead(
              input.binding,
              before,
              reconciled("rollback", before),
            ).pipe(Effect.zipRight(requireCheckpoint)),
          ),
        );
      }
    };

    const start = async () => {
      checkpoint = await run(Journal.load(input.binding));
      signal.throwIfAborted();
      let intersections: readonly LedgerSnapshotPoint[];
      if (checkpoint === null) {
        activation = await locateEventHistoryActivation(transport, {
          binding: input.binding,
          capture: await capture(),
          expectedTransactionHash: input.expectedInitializationTransactionHash,
        });
        intersections = [activation.predecessor];
      } else {
        intersections = samePoint(checkpoint.head, checkpoint.anchor)
          ? [checkpoint.head]
          : [checkpoint.head, checkpoint.anchor];
      }
      signal.throwIfAborted();
      await followBoundEventHistoryChain({
        binding: input.binding,
        ogmiosUrl: input.transport.ogmiosUrl,
        webSocketFactory: input.transport.webSocketFactory,
        requestTimeoutMs: input.transport.timeoutMs,
        heartbeatIntervalMs: input.heartbeatIntervalMs,
        retainedPointLimit: input.retainedPointLimit,
        signal,
        intersections,
        onIntersection: (point) => {
          void enqueue(async () => {
            if (checkpoint !== null) await rewind(point);
          }).catch(fail);
        },
        onForward: (block) => {
          invalidate("history forward work pending");
          return enqueue(() => forward(block));
        },
        onRollback: (point) => {
          invalidate("history source rolled back");
          void enqueue(() => rewind(point)).catch(fail);
        },
        onTip: (observed) => {
          startupMonitor.abort();
          const changed =
            tip === undefined || tip === "origin" || observed === "origin"
              ? tip !== observed
              : !samePoint(tip, observed) || tip.height !== observed.height;
          tip = observed;
          if (changed) invalidate("history source frontier changed");
          // Renew independently of slow projection, only after this source's
          // successful heartbeat/ChainSync response. Never stack renewals.
          if (renewal === undefined) {
            renewal = run(recovery.renew)
              .catch(fail)
              .finally(() => {
                renewal = undefined;
              });
          }
          if (!convergenceQueued) {
            convergenceQueued = true;
            void enqueue(async () => {
              try {
                await converge();
              } finally {
                convergenceQueued = false;
              }
            }).catch(fail);
          }
        },
        onUnavailable: fail,
      });
    };
    const follower = start().catch(fail);
    const close = Effect.promise(async () => {
      if (!closing) {
        closing = true;
        ready = false;
        epoch += 1;
        controller.abort();
        rejectFirstReady(new Error("History owner closed"));
        notifyReadiness();
      }
      // Retire the coordinator generation before joining callbacks: completion
      // may currently be inside cache reload, and must not publish after close.
      const retirement = run(recovery.close);
      void retirement.catch(() => undefined);
      await follower;
      await startupHealth;
      // The chain reader can stop awaiting onForward on cancellation. Join the
      // owner's callback queue explicitly before recovery's scoped finalizer.
      await queue;
      await handle.catch(() => undefined);
      await renewal;
      await retirement;
    });
    yield* Effect.addFinalizer(() => close);
    const assertCurrent = (expected: number) =>
      Effect.suspend(() =>
        ready && !closing && !failed && epoch === expected
          ? Effect.void
          : Effect.fail(
              new HistoryRecoverySuperseded({
                message: "History source gate is closed",
              }),
            ),
      );
    return {
      close,
      reconciliationStatus: Effect.sync(() => pendingReconciliation),
      awaitReady: Effect.tryPromise({
        try: () => firstReady,
        catch: (cause) => new HistoryOwnerUnavailable({ cause }),
      }),
      /** Wait for this exact authenticated frontier. This is outside producer
       * registration: a producer must never wait for the recovery it prevents
       * from draining. A point already passed is refused, not treated as fresh.
       */
      awaitReadyAt: (point: LedgerSnapshotPoint) =>
        Effect.async<HistoryOwnerCoverage, HistoryOwnerUnavailable>(
          (resume) => {
            const check = () => {
              if (closing || failed) {
                readinessWaiters.delete(check);
                resume(
                  Effect.fail(
                    new HistoryOwnerUnavailable({
                      cause: failure ?? "History owner closed",
                    }),
                  ),
                );
              } else if (ready && checkpoint !== null) {
                if (samePoint(checkpoint.head, point)) {
                  readinessWaiters.delete(check);
                  resume(Effect.succeed(coverage()));
                } else if (checkpoint.head.slot >= point.slot) {
                  readinessWaiters.delete(check);
                  resume(
                    Effect.fail(
                      new HistoryOwnerUnavailable({
                        cause: "Requested history frontier was superseded",
                      }),
                    ),
                  );
                }
              }
            };
            readinessWaiters.add(check);
            check();
            return Effect.sync(() => {
              readinessWaiters.delete(check);
            });
          },
        ),
      awaitStopped: Effect.tryPromise({
        try: async () => {
          await follower;
          if (failed) throw failure;
        },
        catch: (cause) => new HistoryOwnerUnavailable({ cause }),
      }),
      runProducer: <A, E2, R2>(
        work: (
          token: Authority.Token,
          assert: Effect.Effect<void, HistoryRecoverySuperseded>,
          coverage: HistoryOwnerCoverage,
        ) => Effect.Effect<A, E2, R2>,
      ) =>
        Effect.suspend(() => {
          const expected = epoch;
          const current = assertCurrent(expected);
          return current.pipe(
            Effect.zipRight(
              recovery.runProducer((token, owned) => {
                const guard = current.pipe(Effect.zipRight(owned));
                return guard.pipe(
                  Effect.zipRight(
                    Effect.suspend(() => work(token, guard, coverage())),
                  ),
                  Effect.tap(() => guard),
                );
              }),
            ),
          );
        }),
    };
  });

export type EventHistoryOwner = Effect.Effect.Success<
  ReturnType<typeof makeEventHistoryOwner>
>;
