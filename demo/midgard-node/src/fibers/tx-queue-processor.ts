import type { MidgardCekProgramEnvelopeV1 } from "@al-ft/midgard-core/cek-proof";
import { decodeMidgardNativeTxFullV1FromCanonicalCbor } from "@al-ft/midgard-core/codec";
import { collectMidgardV1ReferencedProgramEnvelopes } from "@al-ft/midgard-core/script-proof";
import {
  LedgerColumns,
  type PhaseAResult,
  processedTxFromValidatedTx,
  QueuedTx,
  RejectCode,
  RejectedTx,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { SqlClient } from "@effect/sql/SqlClient";
import { Duration, Effect, Exit, Metric, Ref, Schedule } from "effect";

import { TxAdmissionsDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import {
  BatchSql,
  Globals,
  Lucid,
  MempoolLedgerCache,
  NodeConfig,
  type NodeConfigDep,
  ValidationPool,
  type ValidationPoolService,
  WriteBehind,
} from "@/services/index.js";

/**
 * Background validation loop for queued L2 transactions.
 *
 * The processor batches queued payloads, runs phase-A and phase-B validation,
 * applies accepted state patches to the mempool ledger, and records rejections
 * for later inspection.
 */
const validationPhaseALatencyGauge = Metric.gauge(
  "validation_phase_a_latency_ms",
  {
    description: "Phase-A validation latency in milliseconds",
  },
);

const validationPhaseBLatencyGauge = Metric.gauge(
  "validation_phase_b_latency_ms",
  {
    description: "Phase-B validation latency in milliseconds",
  },
);

const validationBatchSizeGauge = Metric.gauge("validation_batch_size", {
  description: "Number of queued txs fetched for a validation batch",
  bigint: true,
});

const validationAcceptCounter = Metric.counter("validation_accept_count", {
  description: "Total number of txs accepted by phase-1 validation",
  bigint: true,
  incremental: true,
});

const validationRejectCounter = Metric.counter("validation_reject_count", {
  description: "Total number of txs rejected by phase-1 validation",
  bigint: true,
  incremental: true,
});

const validationQueueDepthGauge = Metric.gauge("validation_queue_depth", {
  description: "Current number of queued transactions awaiting validation",
  bigint: true,
});

const validationWorkerUtilizationGauge = Metric.gauge(
  "validation_worker_utilization",
  {
    description:
      "Fraction of configured batch capacity used in the latest validation batch (0-1)",
  },
);

const validationPhaseAConcurrencyGauge = Metric.gauge(
  "validation_phase_a_effective_concurrency",
  {
    description: "Effective Phase-A validation concurrency selected per batch",
    bigint: true,
  },
);

const validationOldestQueuedTxAgeGauge = Metric.gauge(
  "validation_oldest_queued_tx_age_ms",
  {
    description: "Age of the oldest transaction waiting for validation",
  },
);

const validationQueueWaitDurationTimer = Metric.timer(
  "validation_queue_wait_duration",
  "Deterministic uniform sample (up to 64 per batch, including endpoints and max) of first_seen_at to validation_started_at milliseconds",
);

const validationQueueWaitMaxGauge = Metric.gauge(
  "validation_queue_wait_max_ms",
  {
    description:
      "Maximum first_seen_at to validation_started_at wait in the latest claimed batch",
  },
);

const validationEventWakeupCounter = Metric.counter(
  "validation_event_wakeup_count",
  {
    description:
      "Number of event-driven tx queue wakeups requested after durable admission",
    bigint: true,
    incremental: true,
  },
);

const validationCoalescedWakeupCounter = Metric.counter(
  "validation_coalesced_wakeup_count",
  {
    description:
      "Number of tx queue wakeup requests coalesced behind an active processor",
    bigint: true,
    incremental: true,
  },
);

export const validationBatchDurationTimer = Metric.timer(
  "validation_batch_duration",
  "End-to-end validation batch duration in milliseconds",
);

export const validationClaimDurationTimer = Metric.timer(
  "validation_claim_duration",
  "Duration of one ordered durable admission lease claim",
);

export const validationClaimPayloadLoadDurationTimer = Metric.timer(
  "validation_claim_payload_load_duration",
  "Duration of loading CBOR payloads for an already claimed validation lease",
);

export const validationBatchDurationSummary = Metric.summary({
  name: "validation_batch_duration_summary_ms",
  maxAge: "24 hours",
  maxSize: 100_000,
  error: 0.001,
  quantiles: [0.5, 0.9, 0.99],
  description: "Validation batch duration quantiles in milliseconds",
});

export const validationPhaseADurationTimer = Metric.timer(
  "validation_phase_a_duration",
  "Phase-A validation duration in milliseconds",
);

export const validationPhaseBDurationTimer = Metric.timer(
  "validation_phase_b_duration",
  "Phase-B validation duration in milliseconds",
);

export const validationMempoolInsertDurationTimer = Metric.timer(
  "validation_mempool_insert_duration",
  "Duration of accepted transaction inserts into MempoolDB",
);

const validationRejectionInsertDurationTimer = Metric.timer(
  "validation_rejection_insert_duration",
  "Duration of rejected transaction inserts into TxRejectionsDB",
);

const validationDrainLoopsActiveGauge = Metric.gauge(
  "validation_drain_loops_active",
  {
    description: "Concurrent validation drain loops currently active",
    bigint: true,
  },
);

/**
 * Summarizes a rejection batch into a compact per-code counter string for
 * logs.
 */
const summarizeRejections = (rejected: readonly RejectedTx[]): string => {
  if (rejected.length === 0) {
    return "none";
  }

  const perCode = rejected.reduce((acc, r) => {
    const count = acc.get(r.code) ?? 0;
    acc.set(r.code, count + 1);
    return acc;
  }, new Map<RejectCode, number>());

  return Array.from(perCode.entries())
    .map(([code, count]) => `${code}:${count}`)
    .join(", ");
};

/**
 * Detects provider/runtime failures where no trustworthy validation result was
 * obtained and the batch should be retried rather than rejected.
 */
const isPlutusEvaluationInfrastructureFailure = (cause: unknown): boolean => {
  const message = String(cause);
  return [
    /configured lucid provider does not support evaluatetx/i,
    /\bfetch failed\b/i,
    /\bnetworkerror\b/i,
    /\btimeout\b/i,
    /\btimed out\b/i,
    /\babort(?:ed|error)?\b/i,
    /\beconn(?:reset|refused)\b/i,
    /\benotfound\b/i,
    /\b429\b/,
    /\b5\d\d\b/,
    /\brate limit/i,
    /\bservice unavailable\b/i,
    /\btemporar(?:y|ily)\b/i,
  ].some((pattern) => pattern.test(message));
};

/**
 * Normalizes provider-side Plutus validation failures into persisted rejection
 * details. Explicit infrastructure/runtime faults return `null` so the batch is
 * retried instead of poisoning the tx.
 */
export const classifyPlutusEvaluationFailure = (
  cause: unknown,
): string | null => {
  const message = String(cause);
  if (isPlutusEvaluationInfrastructureFailure(cause)) {
    return null;
  }

  const scriptHashMatch = message.match(/ScriptHash[^0-9a-f]*([0-9a-f]{56})/i);
  const scriptInfoMatch = message.match(/ScriptInfo:\s*([^\n"]+)/i);
  const reasonMatch = message.match(/Caused by:\s*([^\n"]+)/i);
  const txIdMatch = message.match(/TxId:\s*([0-9a-f]{64})/i);
  if (
    scriptHashMatch !== null ||
    scriptInfoMatch !== null ||
    reasonMatch !== null ||
    txIdMatch !== null
  ) {
    return [
      txIdMatch !== null ? `tx_id=${txIdMatch[1]}` : null,
      scriptHashMatch !== null ? `script_hash=${scriptHashMatch[1]}` : null,
      scriptInfoMatch !== null ? `script_info=${scriptInfoMatch[1]}` : null,
      reasonMatch !== null ? `reason=${reasonMatch[1]}` : null,
    ]
      .filter((value): value is string => value !== null)
      .join(",");
  }

  return message;
};

/**
 * Repeats a scheduled background action while logging and swallowing per-iteration
 * failures so the loop survives transient outages.
 */
export const repeatScheduledWithCauseLogging = <R>(
  action: Effect.Effect<void, unknown, R>,
  schedule: Schedule.Schedule<number>,
): Effect.Effect<void, never, R> =>
  Effect.repeat(action.pipe(Effect.catchAllCause(Effect.logWarning)), schedule);

export const withAdmissionLeaseRecovery = <A, E, R, E2, R2>(
  effect: Effect.Effect<A, E, R>,
  releaseForRetry: Effect.Effect<void, E2, R2>,
): Effect.Effect<A, E, R | R2> =>
  effect.pipe(
    Effect.catchAllCause((cause) =>
      releaseForRetry.pipe(
        Effect.catchAllCause(Effect.logWarning),
        Effect.zipRight(Effect.failCause(cause)),
      ),
    ),
  );

/**
 * Normalizes one queued payload into either a validated queue entry or an
 * immediate rejection describing malformed binary fields.
 */
const admissionToQueuedTx = (
  admission: TxAdmissionsDB.ClaimedEntry,
): QueuedTx => ({
  txId: admission.tx_id,
  txCbor: admission.tx_canonical_cbor,
  programMaterialSidecarCbor: admission.cek_program_material_sidecar_cbor,
  arrivalSeq: admission.arrival_seq,
  createdAt: admission.first_seen_at,
});

type AcceptedReferenceProgramCandidate = {
  readonly ledgerTx: { readonly txId: Uint8Array };
  readonly submission: { readonly txCbor: Uint8Array };
  readonly graph: {
    readonly produced: readonly {
      readonly [LedgerColumns.OUTREF]: Uint8Array;
      readonly [LedgerColumns.OUTPUT]: Uint8Array;
    }[];
  };
};

/**
 * Reconstructs only the reference-input program envelopes already resolved by
 * successful Phase B. This is persistence metadata, not a second validation
 * decision: missing or malformed state fails the acceptance transaction.
 */
export const collectAcceptedReferenceProgramEnvelopes = (
  accepted: readonly AcceptedReferenceProgramCandidate[],
  preState: ReadonlyMap<string, Buffer>,
): ReadonlyMap<string, readonly MidgardCekProgramEnvelopeV1[]> => {
  const resolvedOutputs = new Map<string, Uint8Array>(preState);
  for (const candidate of accepted) {
    for (const produced of candidate.graph.produced) {
      resolvedOutputs.set(
        Buffer.from(produced[LedgerColumns.OUTREF]).toString("hex"),
        produced[LedgerColumns.OUTPUT],
      );
    }
  }
  return new Map(
    accepted.map((candidate) => {
      const canonicalTx = decodeMidgardNativeTxFullV1FromCanonicalCbor(
        candidate.submission.txCbor,
      );
      return [
        Buffer.from(candidate.ledgerTx.txId).toString("hex"),
        collectMidgardV1ReferencedProgramEnvelopes(
          canonicalTx,
          resolvedOutputs,
        ),
      ] as const;
    }),
  );
};

/**
 * Clamps a numeric value into an inclusive range.
 */
const clamp = (value: number, min: number, max: number): number =>
  Math.min(max, Math.max(min, value));

/**
 * Chooses an effective validation batch size based on configured limits and
 * current queue depth.
 */
const selectValidationBatchSize = (
  configuredBatchSize: number,
  queueDepth: number,
  hardCap: number,
  configuredMinimum: number,
): number => {
  const maxBatchSize = clamp(configuredBatchSize, 1, hardCap);
  const minBatchSize = Math.min(maxBatchSize, configuredMinimum);
  if (queueDepth <= 0) {
    return maxBatchSize;
  }
  if (queueDepth <= minBatchSize) {
    return minBatchSize;
  }
  if (queueDepth <= maxBatchSize) {
    return clamp(Math.ceil(queueDepth / 2), minBatchSize, maxBatchSize);
  }
  return maxBatchSize;
};

export const sampleValidationQueueWaits = (
  waits: readonly number[],
  limit = 64,
): readonly number[] => {
  const sampleLimit = Math.max(1, Math.floor(limit));
  if (waits.length <= sampleLimit) {
    return waits;
  }
  if (sampleLimit === 1) {
    return [Math.max(...waits)];
  }

  const indices = new Set<number>();
  for (let sample = 0; sample < sampleLimit; sample += 1) {
    indices.add(Math.floor((sample * (waits.length - 1)) / (sampleLimit - 1)));
  }
  let maxIndex = 0;
  for (let index = 1; index < waits.length; index += 1) {
    if (waits[index]! > waits[maxIndex]!) maxIndex = index;
  }
  indices.add(maxIndex);
  if (indices.size > sampleLimit) {
    const protectedIndices = new Set([0, waits.length - 1, maxIndex]);
    for (const index of [...indices].sort((left, right) => right - left)) {
      if (!protectedIndices.has(index)) {
        indices.delete(index);
        break;
      }
    }
  }
  return [...indices]
    .sort((left, right) => left - right)
    .map((index) => waits[index]!);
};

const runPhaseAForBatch = (
  queuedTxs: readonly QueuedTx[],
  nodeConfig: NodeConfigDep,
  pool: ValidationPoolService,
): Effect.Effect<PhaseAResult, Error> => {
  const phaseAConfig = {
    expectedNetworkId: nodeConfig.NETWORK === "Mainnet" ? 1n : 0n,
    minFeeA: nodeConfig.MIN_FEE_A,
    minFeeB: nodeConfig.MIN_FEE_B,
    concurrency: nodeConfig.VALIDATION_PHASE_A_CONCURRENCY,
    strictnessProfile: nodeConfig.VALIDATION_STRICTNESS_PROFILE,
    consensusProfile: pool.consensusProfile,
  };
  if (
    pool.poolSize === 0 ||
    queuedTxs.length < nodeConfig.VALIDATION_WORKER_INLINE_THRESHOLD
  ) {
    return runPhaseAValidation(queuedTxs, phaseAConfig);
  }

  const chunks: QueuedTx[][] = [];
  for (
    let offset = 0;
    offset < queuedTxs.length;
    offset += nodeConfig.VALIDATION_WORKER_CHUNK_SIZE
  ) {
    chunks.push(
      queuedTxs.slice(offset, offset + nodeConfig.VALIDATION_WORKER_CHUNK_SIZE),
    );
  }
  return Effect.forEach(chunks, pool.runPhaseAChunk, {
    concurrency: pool.poolSize,
  }).pipe(
    Effect.map((results) => ({
      accepted: results.flatMap((result) => result.accepted),
      rejected: results.flatMap((result) => result.rejected),
    })),
  );
};

/**
 * Runs one queue-processing tick, draining queued payloads and validating an
 * effective batch against the current mempool-ledger pre-state.
 */
type TxQueueProcessorActionResult = {
  readonly processed: boolean;
  readonly claimedCount: number;
  readonly batchSize: number;
};

const txQueueProcessorAction = (
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

            const { phaseB, allRejected, referenceProgramEnvelopesByTxId } =
              yield* phaseBSequence.runDecision(
                Effect.gen(function* () {
                  const cachedState = yield* ledgerCache.currentState;
                  const phaseBStart = Date.now();
                  const phaseB = yield* runPhaseBValidationWithPatch(
                    phaseA.accepted,
                    cachedState,
                    {
                      nowCardanoSlotNo: BigInt(lucid.currentSlot()),
                      bucketConcurrency:
                        nodeConfig.VALIDATION_G4_BUCKET_CONCURRENCY,
                      enforceScriptBudget: true,
                      ...(nodeConfig.VALIDATION_UPLC_IN_WORKERS &&
                      validationPool.poolSize > 0
                        ? { evaluateScript: validationPool.evaluateScript }
                        : {}),
                    },
                  );
                  yield* validationPhaseBLatencyGauge(
                    Effect.succeed(Date.now() - phaseBStart),
                  );
                  yield* validationPhaseBDurationTimer(
                    Effect.succeed(Duration.millis(Date.now() - phaseBStart)),
                  );

                  const allRejected = [...phaseA.rejected, ...phaseB.rejected];
                  const referenceProgramEnvelopesByTxId =
                    collectAcceptedReferenceProgramEnvelopes(
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
                    referenceProgramEnvelopesByTxId,
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
                    referenceProgramEnvelopesByTxId,
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
  });

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

export const txQueueProcessorDrainOnce = (): Effect.Effect<
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
    const started = yield* Ref.modify(
      globals.TX_QUEUE_PROCESSOR_ACTIVE,
      (active) =>
        active < config.VALIDATION_DRAIN_LOOPS
          ? [true, active + 1]
          : [false, active],
    );
    if (!started) {
      yield* Metric.increment(validationCoalescedWakeupCounter);
      return;
    }
    const active = yield* Ref.get(globals.TX_QUEUE_PROCESSOR_ACTIVE);
    yield* validationDrainLoopsActiveGauge(Effect.succeed(BigInt(active)));
    let handledGeneration = yield* Ref.get(globals.TX_QUEUE_WAKE_GENERATION);
    yield* txQueueProcessorDrainLoop().pipe(
      Effect.tap((generation) =>
        Effect.sync(() => {
          handledGeneration = generation;
        }),
      ),
      Effect.asVoid,
      Effect.ensuring(
        Effect.gen(function* () {
          const count = yield* Ref.updateAndGet(
            globals.TX_QUEUE_PROCESSOR_ACTIVE,
            (activeCount) => Math.max(0, activeCount - 1),
          );
          yield* validationDrainLoopsActiveGauge(Effect.succeed(BigInt(count)));
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
        }),
      ),
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
