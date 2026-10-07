import { processedTxFromValidatedTx } from "@al-ft/midgard-validation";
import { SqlClient } from "@effect/sql/SqlClient";
import { Duration, Effect, Exit, Metric, Ref } from "effect";

import { TxAdmissionsDB, WithdrawalsDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import { l1SlotNow } from "../l1-heads.js";
import { runHistoryProducer } from "../services/event-history-producer.js";
import {
  Globals,
  Lucid,
  MempoolLedgerCache,
  NodeConfig,
  ValidationPool,
  WriteBehind,
} from "../services/index.js";
import {
  decideAdmissionBatch,
  summarizeRejections,
  validationAcceptCounter,
  validationBatchDurationSummary,
  validationBatchDurationTimer,
  validationBatchSizeGauge,
  validationClaimDurationTimer,
  validationClaimPayloadLoadDurationTimer,
  validationMempoolInsertDurationTimer,
  validationOldestQueuedTxAgeGauge,
  validationPhaseAConcurrencyGauge,
  validationPhaseADurationTimer,
  validationPhaseALatencyGauge,
  validationPhaseBDurationTimer,
  validationPhaseBLatencyGauge,
  validationQueueDepthGauge,
  validationQueueWaitDurationTimer,
  validationQueueWaitMaxGauge,
  validationRejectCounter,
  validationRejectionInsertDurationTimer,
  validationWorkerUtilizationGauge,
} from "./tx-queue-processor.classify-plutus-evaluation-failure.js";
import {
  admissionToQueuedTx,
  collectAcceptedProgramEnvelopes,
  runPhaseAForBatch,
  sampleValidationQueueWaits,
  selectValidationBatchSize,
  type TxQueueProcessorActionResult,
} from "./tx-queue-processor.run-phase-afor-batch.js";

export const txQueueProcessorAction = (
  recoverySweep: boolean,
): Effect.Effect<
  TxQueueProcessorActionResult,
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
    const { api: lucid } = yield* Lucid;
    const validationPool = yield* ValidationPool;
    const ledgerCache = yield* MempoolLedgerCache;
    yield* Ref.set(globals.HEARTBEAT_TX_QUEUE_PROCESSOR, Date.now());
    const nodeConfig = yield* NodeConfig;
    const localFinalizationPending = yield* Ref.get(
      globals.LOCAL_FINALIZATION_PENDING,
    );

    const expiredLeaseCount = recoverySweep
      ? yield* TxAdmissionsDB.requeueExpiredLeases
      : 0;
    const durableBacklog = recoverySweep
      ? yield* TxAdmissionsDB.countBacklog
      : BigInt(
          Math.min(
            nodeConfig.VALIDATION_BATCH_SIZE,
            nodeConfig.VALIDATION_BATCH_HARD_CAP,
          ) + 1,
        );
    const totalQueueDepth = Number(durableBacklog);
    if (recoverySweep) {
      yield* validationQueueDepthGauge(Effect.succeed(durableBacklog));
    }

    if (localFinalizationPending) {
      yield* validationBatchSizeGauge(Effect.succeed(0n));
      yield* validationWorkerUtilizationGauge(Effect.succeed(0));
      yield* Effect.logDebug(
        "tx-queue processor paused while local finalization recovery is pending",
      );
      return { processed: false, claimedCount: 0, batchSize: 0 };
    }

    if (durableBacklog === 0n) {
      yield* validationBatchSizeGauge(Effect.succeed(0n));
      yield* validationWorkerUtilizationGauge(Effect.succeed(0));
      yield* validationOldestQueuedTxAgeGauge(Effect.succeed(0));
      return { processed: false, claimedCount: 0, batchSize: 0 };
    }

    if (recoverySweep) {
      const oldestAgeMillis = yield* TxAdmissionsDB.oldestQueuedAgeMs;
      yield* validationOldestQueuedTxAgeGauge(
        Effect.succeed(Math.max(0, oldestAgeMillis)),
      );
    }
    const batchSize = selectValidationBatchSize(
      nodeConfig.VALIDATION_BATCH_SIZE,
      totalQueueDepth,
      nodeConfig.VALIDATION_BATCH_HARD_CAP,
      nodeConfig.VALIDATION_MIN_BATCH,
    );

    // Validity intervals are judged at the L1 `slotNow` (plan §3.6), read
    // before any row is claimed: while no L1 tip has been read the tick claims
    // nothing and the next tick retries.
    const nowSlot = yield* Effect.either(l1SlotNow(lucid));
    if (nowSlot._tag === "Left") {
      yield* Effect.logWarning(
        `tx-queue processor waiting for the L1 slot: ${nowSlot.left.message}`,
      );
      return { processed: false, claimedCount: 0, batchSize: 0 };
    }
    const leaseOwner = `tx-queue-processor:${process.pid}:${Date.now()}:${Math.random().toString(16).slice(2)}`;
    const claimStartedAt = Date.now();
    return yield* Effect.acquireUseRelease(
      ledgerCache.withClaimLock(
        Effect.gen(function* () {
          // Keep the globally ordered claim/sequence section small.  Fetching
          // multi-megabyte CBOR batches is lease-bound but does not mutate
          // cache state, so it intentionally runs after this lock releases.
          const claimedLeases = yield* TxAdmissionsDB.claimBatchLease({
            limit: batchSize,
            leaseOwner,
            leaseDurationMs: nodeConfig.VALIDATION_LEASE_MS,
          });
          const phaseBSequence =
            claimedLeases.length === 0
              ? undefined
              : yield* ledgerCache.registerPhaseBSequence;
          return { claimedLeases, phaseBSequence };
        }),
      ),
      ({ claimedLeases, phaseBSequence }) =>
        Effect.gen(function* () {
          yield* validationClaimDurationTimer(
            Effect.succeed(Duration.millis(Date.now() - claimStartedAt)),
          );

          if (claimedLeases.length === 0 || phaseBSequence === undefined) {
            yield* validationBatchSizeGauge(Effect.succeed(0n));
            yield* validationWorkerUtilizationGauge(Effect.succeed(0));
            return { processed: false, claimedCount: 0, batchSize };
          }

          // This rechecks both the lease and payload presence after the
          // ordered claim commits. Any mismatch fails the tick, which releases
          // the entire lease in the acquire/release finalizer below; it is
          // never converted into an ordinary transaction rejection.
          const payloadLoadStartedAt = Date.now();
          const admittedRows = yield* TxAdmissionsDB.loadClaimedPayloads({
            claimed: claimedLeases,
            leaseOwner,
          });
          yield* validationClaimPayloadLoadDurationTimer(
            Effect.succeed(Duration.millis(Date.now() - payloadLoadStartedAt)),
          );

          const queueWaits = admittedRows.map((row) =>
            Math.max(
              0,
              (row.validation_started_at ?? new Date()).getTime() -
                row.first_seen_at.getTime(),
            ),
          );
          for (const waitMs of sampleValidationQueueWaits(queueWaits)) {
            yield* validationQueueWaitDurationTimer(
              Effect.succeed(Duration.millis(waitMs)),
            );
          }
          const maxQueueWaitMs =
            queueWaits.length === 0 ? 0 : Math.max(...queueWaits);
          yield* validationQueueWaitMaxGauge(Effect.succeed(maxQueueWaitMs));

          yield* validationBatchSizeGauge(
            Effect.succeed(BigInt(admittedRows.length)),
          );
          const utilization = admittedRows.length / batchSize;
          yield* validationWorkerUtilizationGauge(Effect.succeed(utilization));

          yield* Effect.gen(function* () {
            const batchStart = Date.now();
            const queuedTxs = admittedRows.map(admissionToQueuedTx);

            const phaseAStart = Date.now();
            const phaseA = yield* runPhaseAForBatch(
              queuedTxs,
              nodeConfig,
              validationPool,
            );
            const phaseAConcurrency =
              validationPool.poolSize > 0 &&
              queuedTxs.length >= nodeConfig.VALIDATION_WORKER_INLINE_THRESHOLD
                ? validationPool.poolSize
                : nodeConfig.VALIDATION_PHASE_A_CONCURRENCY;
            yield* validationPhaseAConcurrencyGauge(
              Effect.succeed(BigInt(phaseAConcurrency)),
            );
            yield* validationPhaseALatencyGauge(
              Effect.succeed(Date.now() - phaseAStart),
            );
            yield* validationPhaseADurationTimer(
              Effect.succeed(Duration.millis(Date.now() - phaseAStart)),
            );

            const pendingWithdrawalOutRefHexes =
              yield* WithdrawalsDB.retrievePendingLedgerOutRefHexes;
            const { phaseB, allRejected, programEnvelopesByTxId } =
              yield* phaseBSequence.runDecision(
                Effect.gen(function* () {
                  const cachedState = yield* ledgerCache.currentState;
                  const phaseBStart = Date.now();
                  const { phaseB, allRejected } = yield* decideAdmissionBatch({
                    phaseA,
                    pendingWithdrawalOutRefHexes,
                    ledgerState: cachedState,
                    phaseBConfig: {
                      nowCardanoSlotNo: BigInt(nowSlot.right),
                      bucketConcurrency:
                        nodeConfig.VALIDATION_G4_BUCKET_CONCURRENCY,
                      enforceScriptBudget: true,
                    },
                  });
                  yield* validationPhaseBLatencyGauge(
                    Effect.succeed(Date.now() - phaseBStart),
                  );
                  yield* validationPhaseBDurationTimer(
                    Effect.succeed(Duration.millis(Date.now() - phaseBStart)),
                  );

                  const programEnvelopesByTxId =
                    collectAcceptedProgramEnvelopes(
                      phaseB.accepted,
                      cachedState,
                    );
                  yield* ledgerCache.applySpeculativePatch(
                    phaseBSequence.sequence,
                    phaseB.statePatch,
                  );
                  return {
                    phaseB,
                    allRejected,
                    programEnvelopesByTxId,
                  };
                }),
              );

            yield* phaseBSequence.runPersistence(
              Effect.gen(function* () {
                if (allRejected.length > 0) {
                  const rejectionInsertStart = Date.now();
                  yield* TxAdmissionsDB.markRejected({
                    rows: admittedRows,
                    leaseOwner,
                    rejectedTxs: allRejected,
                  });
                  yield* validationRejectionInsertDurationTimer(
                    Effect.succeed(
                      Duration.millis(Date.now() - rejectionInsertStart),
                    ),
                  );
                  yield* Metric.incrementBy(
                    validationRejectCounter,
                    BigInt(allRejected.length),
                  );
                }

                if (phaseB.accepted.length > 0) {
                  const mempoolInsertStart = Date.now();
                  yield* TxAdmissionsDB.markAccepted({
                    rows: admittedRows,
                    leaseOwner,
                    processedTxs: phaseB.accepted.map(
                      processedTxFromValidatedTx,
                    ),
                    programEnvelopesByTxId,
                  });
                  yield* validationMempoolInsertDurationTimer(
                    Effect.succeed(
                      Duration.millis(Date.now() - mempoolInsertStart),
                    ),
                  );
                  yield* Metric.incrementBy(
                    validationAcceptCounter,
                    BigInt(phaseB.accepted.length),
                  );
                }
              }),
            );

            yield* Effect.logInfo(
              `tx-queue validation batch done: queued=${admittedRows.length}, accepted=${phaseB.accepted.length}, rejected=${allRejected.length}, expired_leases_requeued=${expiredLeaseCount}, queue_wait_max_ms=${maxQueueWaitMs.toString()}, rejected_by_code=[${summarizeRejections(allRejected)}]`,
            );
            const batchDurationMs = Date.now() - batchStart;
            yield* validationBatchDurationTimer(
              Effect.succeed(Duration.millis(batchDurationMs)),
            );
            yield* validationBatchDurationSummary(
              Effect.succeed(batchDurationMs),
            );
          });
          return {
            processed: true,
            claimedCount: admittedRows.length,
            batchSize,
          };
        }),
      ({ claimedLeases, phaseBSequence }, exit) => {
        if (phaseBSequence === undefined) return Effect.void;
        if (Exit.isSuccess(exit)) return phaseBSequence.cancel;
        return phaseBSequence.cancel.pipe(
          Effect.zipRight(
            TxAdmissionsDB.releaseForRetry({
              txIds: claimedLeases.map((row) => row.tx_id),
              leaseOwner,
              baseDelayMs: nodeConfig.VALIDATION_RETRY_BACKOFF_BASE_MS,
              maxDelayMs: nodeConfig.VALIDATION_RETRY_BACKOFF_MAX_MS,
            }).pipe(Effect.catchAllCause(Effect.logWarning)),
          ),
          Effect.ensuring(
            ledgerCache.recoverPoisonedEpoch.pipe(
              Effect.catchAllCause(Effect.logError),
            ),
          ),
        );
      },
    );
  }).pipe(runHistoryProducer);
