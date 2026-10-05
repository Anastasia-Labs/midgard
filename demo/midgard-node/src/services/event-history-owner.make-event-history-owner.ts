import type * as SDK from "@al-ft/midgard-sdk";
import { Effect, Runtime } from "effect";

import * as Authority from "../database/eventHistoryAuthority.js";
import * as ForeignCensus from "../database/eventHistoryForeignCensus.js";
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
import {
  type BoundHistoryCapture,
  projectEventHistoryBlock,
} from "../l1-event-history-projection.js";
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
} from "../l1-event-history-transport.js";
import {
  LEDGER_SCAN_TIMEOUT_MS,
  type LedgerSnapshotPoint,
} from "../l1-ledger-snapshot.js";
import type { Database } from "./database.js";
import { historyOwnerCoverage } from "./event-history-owner.coverage.js";
import {
  HISTORY_READY_MAXIMUM_LAG_BLOCKS,
  HistoryAppendNeedsRecovery,
  type HistoryOwnerChange,
  type HistoryOwnerCoverage,
  type HistoryOwnerFrontier,
  HistoryOwnerUnavailable,
  type HistoryReconciliationPending,
  PENDING_RECONCILIATION_BACKOFF_INITIAL_MS,
  PENDING_RECONCILIATION_BACKOFF_MAX_MS,
  PENDING_RECONCILIATION_BLOCKED_WARN_INTERVAL_MS,
  samePoint,
} from "./event-history-owner.history-owner-change.js";
import { makeHistoryOwnerNotices } from "./event-history-owner.notices.js";
import {
  makeRetainedHistoryAppender,
  requireRetainedHistoryCheckpoint,
  type Retained,
} from "./event-history-owner.retention.js";
import { isRecoverableHistorySourceFailure } from "./event-history-owner.source-failure.js";
import {
  HISTORY_SOURCE_RECONNECT_BOUNDS,
  type HistorySourceReconnectBounds,
  makeHistorySourceOutage,
} from "./event-history-owner.source-outage.js";
import {
  awaitHistorySourceReconnect,
  makeHistoryLeaseKeeper,
  monitorHistoryStartupHealth,
} from "./event-history-owner.source-session.js";
import { withCreatingBodies } from "./event-history-owner.with-creating-bodies.js";
import {
  type HistoryRecoveryPreparation,
  HistoryRecoverySuperseded,
  makeEventHistoryRecovery,
} from "./event-history-recovery.js";
import { makePendingReconciliationBackoff } from "./history-pending-backoff.js";
import type { MempoolLedgerCacheService } from "./mempool-ledger-cache.js";

/** One scoped source owner. Only low-level transports are replaceable: capture,
 * source authentication, whole-block replay, projection and SQL fencing always
 * run through the production implementations. Persisted state is local recovery
 * material; every start freshly authenticates and intersects the source.
 * Only a first start on an empty journal scans the ledger (once); readiness is
 * otherwise the journal having reached the follower's current tip.
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
  /** k: the deepest rollback recovered automatically (the manifest's
   * l1Finality.automaticRecoveryMaxDepth). Journal blocks deeper than this
   * behind the source tip are pruned behind an advancing anchor. */
  readonly rollbackHorizon: number;
  /** Overrides the source reconnect schedule (tests). */
  readonly sourceReconnect?: Partial<HistorySourceReconnectBounds>;
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
    if (
      !Number.isSafeInteger(input.rollbackHorizon) ||
      input.rollbackHorizon <= 0
    )
      return yield* Effect.fail(
        new HistoryOwnerUnavailable({
          cause: "History rollback horizon must be a positive block count",
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
    // The owner's lifetime, and one source session inside it at a time.
    const owner = new AbortController();
    const ownerSignal = owner.signal;
    let session = new AbortController();
    let signal = AbortSignal.any([ownerSignal, session.signal]);
    const outage = makeHistorySourceOutage({
      ...HISTORY_SOURCE_RECONNECT_BOUNDS,
      ...input.sourceReconnect,
    });
    // A lagging index waits up to one lease: renewal continues meanwhile.
    const sessionTransport = (owned: AbortSignal): HistoryTransportOptions => ({
      ...input.transport,
      signal: owned,
      indexLagCeilingMs: input.leaseDurationMs,
      onIndexLag: outage.indexLag,
    });
    let transport = sessionTransport(signal);
    let checkpoint: Journal.Checkpoint | null = null;
    let replay: EventHistoryListReplay | undefined;
    let reacquiringCensus = false;
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
    // Set at the first readiness; lag transitions are reported only after it.
    let everReady = false;
    // First start only: the single complete capture at point C. The replay seeds
    // the journal when it reaches exactly C, then this is released.
    let seedCapture: BoundHistoryCapture | undefined;
    const readinessWaiters = new Set<() => void>();
    const notifyReadiness = () => {
      for (const notify of [...readinessWaiters]) notify();
    };
    const coverage = () =>
      historyOwnerCoverage(checkpoint, input.slotToUnixTime);
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
        const owned = signal;
        handle = run(recovery.beginRecovery(reason));
        void handle.catch((cause: unknown) => settle(cause, owned));
      }
    };
    const fail = (cause: unknown) => {
      if (closing || failed) return;
      failed = true;
      failure = cause;
      invalidate("history source unavailable");
      owner.abort(cause);
      rejectFirstReady(cause);
      notifyReadiness();
    };
    // A failure inside an aborted session is that abort's echo. A recoverable
    // one ends the session with the gate closed for a reconnect; anything
    // else stops the owner.
    const settle = (cause: unknown, owned: AbortSignal) => {
      if (owned.aborted || closing || failed) return;
      if (!isRecoverableHistorySourceFailure(cause)) return fail(cause);
      outage.lost(cause);
      invalidate("history source unavailable");
      session.abort(cause);
      keeper.keep();
    };
    const renewLease = () =>
      (renewal ??= run(recovery.renew).finally(() => {
        renewal = undefined;
      }));
    const keeper = makeHistoryLeaseKeeper({
      intervalMs: input.heartbeatIntervalMs,
      signal: ownerSignal,
      outage,
      stopped: () => closing || failed,
      renew: renewLease,
      fail,
    });
    const enqueue = (work: () => Promise<void>) => {
      const owned = signal;
      const next = queue.then(async () => {
        owned.throwIfAborted();
        await work();
      });
      queue = next.catch((cause: unknown) => settle(cause, owned));
      return next;
    };
    const sourceWork = (work: () => Promise<void>) =>
      enqueue(async () => {
        await work();
        scheduleConvergence();
      });
    const lagBlocks = () =>
      checkpoint === null || tip === undefined || tip === "origin"
        ? 0
        : Math.max(0, tip.height - checkpoint.head.height);
    const notices = makeHistoryOwnerNotices({
      run,
      rollbackHorizon: input.rollbackHorizon,
    });
    const noteLag = () => {
      if (everReady) notices.lag(lagBlocks());
    };
    // A pending reconciliation whose preparation left it pending is retried
    // on a doubling delay while its reason is unchanged, never on every tip,
    // and named in a periodic warning while it stays blocked. The delay is
    // monotonic: a wall-clock step never stretches or skips it.
    const pendingBackoff = makePendingReconciliationBackoff({
      initialMs: PENDING_RECONCILIATION_BACKOFF_INITIAL_MS,
      maxMs: PENDING_RECONCILIATION_BACKOFF_MAX_MS,
      warnIntervalMs: PENDING_RECONCILIATION_BLOCKED_WARN_INTERVAL_MS,
      onDue: () => {
        if (!closing) scheduleConvergence();
      },
      warn: ({ reason, blockedMs, retryInMs }) => {
        void run(
          Effect.logWarning(
            `History reconciliation still blocked after ${Math.round(blockedMs / 1000).toString()} s; retrying in ${Math.round(retryInMs / 1000).toString()} s: ${reason}`,
          ).pipe(
            Effect.annotateLogs({
              event: "history_reconciliation_blocked",
              reason,
              blockedMs: Math.round(blockedMs),
              retryInMs,
            }),
          ),
        ).catch(() => undefined);
      },
    });
    const clearPendingBackoff = () => pendingBackoff.clear();
    const scheduleConvergence = () => {
      if (convergenceQueued) return;
      convergenceQueued = true;
      // Cleared on dequeue: a signal during this convergence queues another.
      // The queue settles a failure; this only detaches the rejection.
      void enqueue(async () => {
        convergenceQueued = false;
        await converge();
      }).catch(() => undefined);
    };
    // The one complete scan: first start on an empty journal only. It supplies
    // the activation locator and the complete five-address image at its point;
    // the ChainSync replay establishes everything else.
    const capture = () =>
      readBoundEventHistoryLedgerSnapshot({
        binding: input.binding,
        ogmiosUrl: input.transport.ogmiosUrl,
        timeoutMs: LEDGER_SCAN_TIMEOUT_MS,
        webSocketFactory: input.transport.webSocketFactory,
        signal: AbortSignal.any([
          signal,
          AbortSignal.timeout(LEDGER_SCAN_TIMEOUT_MS),
        ]),
      });
    // The follower's first tip ends each session's startup monitor.
    let startupMonitor = new AbortController();
    const monitorStartup = (owned: AbortSignal) => {
      startupMonitor = new AbortController();
      return monitorHistoryStartupHealth({
        binding: input.binding,
        transport: input.transport,
        heartbeatIntervalMs: input.heartbeatIntervalMs,
        signal: AbortSignal.any([owned, startupMonitor.signal]),
        renew: renewLease,
        onFailure: (cause) => settle(cause, owned),
      });
    };
    let startupHealth = monitorStartup(signal);
    const requireCheckpoint = requireRetainedHistoryCheckpoint(input.binding);
    const reconciled = (
      kind: "seed" | "rollback" | "resume",
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
    // A forward append hands its written checkpoint and staged changes to the
    // callback in the same transaction; no reload.
    const reconciledForward =
      (before: Journal.Checkpoint) =>
      ({ after, changes }: Journal.Appended) =>
        input.reconcile({ kind: "forward", before, after, changes }).pipe(
          Effect.tap((result) =>
            Effect.sync(() => {
              pendingReconciliation = result ?? undefined;
            }),
          ),
          Effect.as(after),
        );

    // Readiness opens once the follower has journaled through the current
    // source tip; later blocks append at its head without closing it. No ledger
    // scan: the journal is maintained block by block from this same follower.
    // Only a closed gate (first start, rollback, escalated append) runs this
    // full recovery: producer drain, preparation, cache reload and Ready.
    const converge = async () => {
      if (
        ready ||
        reacquiringCensus ||
        checkpoint === null ||
        tip === undefined ||
        tip === "origin"
      )
        return;
      const head = checkpoint.head;
      if (!samePoint(head, tip) || head.height !== tip.height) return;
      const expected = epoch;
      const active = await handle;
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
          // Before the retry's deadline (another trigger, or a timer that
          // fired early) the backoff re-arms for the remaining time.
          if (!pendingBackoff.due(pendingReconciliation.reason)) return;
          await prepare(input.preparePendingReconciliation);
          if (expected !== epoch) return;
          checkpoint = await run(
            active.persist(reconciled("resume", checkpoint)),
          );
          if (expected !== epoch) return;
          if (pendingReconciliation !== undefined) {
            pendingBackoff.arm(pendingReconciliation.reason);
            return outage.held();
          }
          clearPendingBackoff();
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
            { point: head, snapshotDigest: checkpoint.capture.snapshotDigest },
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
      // Blocks accepted during completion are queued behind it and append at
      // the head of this open gate: the published head is a journaled prefix.
      ready = true;
      everReady = true;
      outage.reopened();
      resolveFirstReady();
      notifyReadiness();
      noteLag();
    };

    const journal = makeRetainedHistoryAppender({
      binding: input.binding,
      rollbackHorizon: input.rollbackHorizon,
      requireCheckpoint,
    });
    // A forward block at the head of an open gate, journaled in the Ready
    // generation: no producer drain, no preparation, no cache reload. The
    // follower's own reconciliation must be a pure extension; anything else
    // rolls this append back and closes the gate for recovery to take the
    // block. Undefined means recovery takes it (the gate closed first).
    const appendReady = async (
      before: Journal.Checkpoint,
      prepared: ReturnType<typeof Journal.prepareAppend>,
      tipHeight: number,
    ): Promise<Retained | undefined> => {
      const expected = epoch;
      const outcome = await run(
        recovery
          .append(
            journal(prepared, tipHeight, ({ after, changes }) =>
              input.reconcile({ kind: "forward", before, after, changes }).pipe(
                Effect.flatMap((result) =>
                  result === undefined || result === null
                    ? Effect.succeed(after)
                    : Effect.fail(
                        new HistoryAppendNeedsRecovery({
                          reason: result.reason,
                        }),
                      ),
                ),
              ),
            ).pipe(
              Effect.tap((retained) =>
                Authority.advanceReadyPoint({
                  point: retained.result.head,
                  snapshotDigest: retained.result.capture.snapshotDigest,
                }),
              ),
            ),
          )
          .pipe(
            Effect.map((retained) => ({ retained }) as const),
            Effect.catchIf(
              (cause) => cause instanceof HistoryAppendNeedsRecovery,
              (needs) =>
                Effect.succeed({
                  needs: (needs as HistoryAppendNeedsRecovery).reason,
                } as const),
            ),
          ),
      ).catch((cause: unknown) => {
        signal.throwIfAborted();
        // A rollback or failure closed the gate while this was in flight.
        if (expected !== epoch) return undefined;
        throw cause;
      });
      if (outcome === undefined) return undefined;
      if ("needs" in outcome) {
        invalidate(`history append requires recovery: ${outcome.needs}`);
        return undefined;
      }
      return outcome.retained;
    };

    const forward = async (block: BoundHistoryChainBlock) => {
      if (checkpoint === null || reacquiringCensus) {
        const active = await handle;
        const step = await withCreatingBodies(
          transport,
          block,
          (getCreatingBody) => {
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
          },
        );
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
            }).pipe(
              Effect.zipRight(
                ForeignCensus.append({
                  binding: input.binding,
                  block,
                  receipt: step.receipt,
                  activation: step.state.activation,
                }),
              ),
            ),
          ),
        );
        replay = step.state;
        outage.replayed(block.point.height);
        if (reacquiringCensus) {
          if (checkpoint === null)
            throw new Error("Census reacquisition lost its journal checkpoint");
          if (samePoint(replay.point, checkpoint.head)) {
            if (replay.point.height !== checkpoint.head.height)
              throw new Error(
                "Census reacquisition height differs from journal",
              );
            reacquiringCensus = false;
            replay = undefined;
          } else if (replay.point.slot >= checkpoint.head.slot)
            throw new Error(
              "Census reacquisition passed the retained canonical checkpoint",
            );
          return;
        }
        if (seedCapture === undefined)
          throw new Error("History first-start capture is missing");
        const at = seedCapture.history.ledger.point;
        if (samePoint(replay.point, at)) {
          const origin = joinEventHistoryListReplay({
            state: replay,
            capture: seedCapture,
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
          seedCapture = undefined;
        } else if (replay.point.slot >= at.slot)
          throw new Error(
            "History first-start capture point left the chain before replay reached it; restart to capture again",
          );
      } else {
        const before = checkpoint;
        const transactions = new Map(
          block.transactions.map((tx) => [tx.txHash, tx]),
        );
        const projection = await withCreatingBodies(transport, block, (body) =>
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
        // The appended block is itself on the source chain, so it bounds the
        // tip from below even before this block's tip notification.
        const tipHeight = Math.max(
          block.point.height,
          tip === undefined || tip === "origin" ? 0 : tip.height,
        );
        let appended = ready
          ? await appendReady(before, prepared, tipHeight)
          : undefined;
        if (appended === undefined) {
          // Closed gate: preparation for the previous head is now stale. No
          // convergence is in flight (the queue is serial) and nothing is
          // published, so this fences only the local recovery attempt.
          epoch += 1;
          const active = await handle;
          appended = await run(
            active.persist(
              journal(prepared, tipHeight, reconciledForward(before)),
            ),
          );
        }
        checkpoint = appended.result;
        await notices.retention(appended.hold);
        noteLag();
        // Frontier waiters observe every appended head of an open gate.
        if (ready) notifyReadiness();
      }
    };
    const rewind = async (point: LedgerSnapshotPoint | "origin") => {
      if (checkpoint === null || point === "origin")
        throw new Error("History rollback requires a retained journal anchor");
      const active = await handle;
      // Refuse before undoing anything: a target that is neither the anchor nor
      // a retained canonical block cannot be reached by undoing heads.
      if (!(await run(Journal.retains(input.binding, checkpoint, point))))
        throw new Error("History rollback exceeds retained journal ancestry");
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
            ).pipe(Effect.map((undone) => undone.result)),
          ),
        );
      }
    };

    const start = async () => {
      const owned = signal;
      checkpoint = await run(Journal.load(input.binding));
      signal.throwIfAborted();
      let intersections: readonly LedgerSnapshotPoint[];
      if (checkpoint === null) {
        seedCapture = await capture();
        signal.throwIfAborted();
        activation = await locateEventHistoryActivation(transport, {
          binding: input.binding,
          capture: seedCapture,
          expectedTransactionHash: input.expectedInitializationTransactionHash,
        });
        intersections = [activation.predecessor];
      } else if (
        !(await run(ForeignCensus.covers(input.binding, checkpoint.head)))
      ) {
        // Upgrade/recovery reuses this owner's one follower. Keep the source
        // gate closed while independently rebuilding the complete census from
        // activation to the exact retained head; never seed from a current list.
        const active = await handle;
        await run(
          active.persist(ForeignCensus.beginReacquisition(input.binding)),
        );
        activation = await locateEventHistoryActivation(transport, {
          binding: input.binding,
          capture: checkpoint.capture,
          expectedTransactionHash: input.expectedInitializationTransactionHash,
        });
        reacquiringCensus = true;
        intersections = [activation.predecessor];
      } else
        intersections = await run(
          Journal.intersections(input.binding, checkpoint),
        );
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
          // Resuming exactly at the head keeps the journal; anything else
          // rewinds it and closes the gate like a rollback.
          if (
            !reacquiringCensus &&
            checkpoint !== null &&
            !samePoint(checkpoint.head, point)
          )
            invalidate("history source intersection rewinds the journal");
          void sourceWork(async () => {
            if (!reacquiringCensus && checkpoint !== null) await rewind(point);
          }).catch(() => undefined);
        },
        // Appending at the head never closes the gate (see appendReady).
        onForward: (block) => sourceWork(() => forward(block)),
        onRollback: (point) => {
          if (reacquiringCensus) {
            fail(
              new Error(
                "History source rolled back during census reacquisition; restart from authenticated activation",
              ),
            );
            return;
          }
          invalidate("history source rolled back");
          // The frontier legitimately moves back only here; the response that
          // carried this rollback reports the new tip next.
          tip = undefined;
          void sourceWork(() => rewind(point)).catch(() => undefined);
        },
        onTip: (observed) => {
          startupMonitor.abort();
          outage.answered();
          // A heartbeat answered before a newer ChainSync response can report
          // an older tip. Never regress the frontier except through a rollback.
          const stale =
            tip !== undefined &&
            tip !== "origin" &&
            (observed === "origin" || observed.height < tip.height);
          // A tip advance never closes the gate; it only measures the lag.
          if (!stale) tip = observed;
          noteLag();
          // Renew independently of slow projection, only after this source's
          // successful heartbeat/ChainSync response. Never stack renewals.
          void renewLease().catch((cause: unknown) => settle(cause, owned));
          scheduleConvergence();
        },
        // A silent socket closes the gate; its next answer reopens the path.
        onHeartbeatMiss: (misses, cause) => {
          if (owned.aborted) return;
          outage.lost(cause);
          invalidate("history source heartbeat missed");
          keeper.keep();
          warn("History source missed a heartbeat; the gate is closed", {
            event: "history_source_heartbeat_miss",
            misses,
            lastError: outage.status().lastError,
          });
        },
        onUnavailable: (cause) => settle(cause, owned),
      });
    };
    const warn = (message: string, annotations: Record<string, unknown>) =>
      void run(
        Effect.logWarning(message).pipe(Effect.annotateLogs(annotations)),
      ).catch(() => undefined);
    // One source session at a time. A recoverable failure ends a session with
    // the gate closed while the keeper renews the lease. The next session
    // first re-validates this owner's live lease (a new recovery generation),
    // then reloads the journal, re-authenticates the source and re-intersects
    // at the journal's retained points; readiness reopens only through
    // convergence at the tip; outages escalate while gated retries continue.
    const supervise = async () => {
      while (true) {
        const owned = signal;
        await start().catch((cause: unknown) => settle(cause, owned));
        session.abort();
        await startupHealth;
        await queue;
        const reconnect = await awaitHistorySourceReconnect({
          outage,
          signal: ownerSignal,
          stopped: () => closing || failed,
          revalidate: () =>
            (handle = run(
              recovery.beginRecovery("history source reconnecting"),
            )),
          fail,
          warn,
        });
        if (!reconnect) return;
        session = new AbortController();
        signal = AbortSignal.any([ownerSignal, session.signal]);
        transport = sessionTransport(signal);
        // Session-scoped source state; the journal is reloaded by start().
        tip = undefined;
        replay = undefined;
        // A census reacquisition restarts from activation in the next session.
        reacquiringCensus = false;
        seedCapture = undefined;
        activation = undefined;
        convergenceQueued = false;
        epoch += 1;
        startupHealth = monitorStartup(signal);
      }
    };
    const follower = supervise();
    const close = Effect.promise(async () => {
      clearPendingBackoff();
      if (!closing) {
        closing = true;
        ready = false;
        epoch += 1;
        owner.abort();
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
      await renewal?.catch(() => undefined);
      await keeper.joined();
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
      /** A requesting producer must return before this recovery can drain it.
       * Notification closes the gate synchronously and queues the existing
       * convergence path; it never awaits producer termination here. */
      requestReconciliation: (reason: string) =>
        Effect.sync(() => {
          if (closing || failed) return;
          invalidate(reason);
          notifyReadiness();
          clearPendingBackoff();
          scheduleConvergence();
        }),
      reconciliationStatus: Effect.sync(() => pendingReconciliation),
      /** Whether the source is following, reconnecting or waiting for Kupo,
       * with the current outage's start, attempts and last error. */
      sourceStatus: Effect.sync(() => outage.status()),
      /** Set while pending recovery holds retention more than k blocks back. */
      retentionHold: Effect.sync(() => notices.retentionHold()),
      /** The gate, and how far the journal head is behind the source tip. */
      frontier: Effect.sync(
        (): HistoryOwnerFrontier => ({
          ready: ready && !closing && !failed,
          headHeight: checkpoint?.head.height ?? null,
          tipHeight: tip === undefined || tip === "origin" ? null : tip.height,
          lagBlocks: lagBlocks(),
          maximumLagBlocks: HISTORY_READY_MAXIMUM_LAG_BLOCKS,
        }),
      ),
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
          const lag = lagBlocks();
          if (lag > HISTORY_READY_MAXIMUM_LAG_BLOCKS)
            return Effect.fail(
              new HistoryRecoverySuperseded({
                message: `History follower is ${lag} blocks behind the source tip`,
              }),
            );
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
