import {
  applyUTxOStatePatch,
  processedTxFromValidatedTx,
  QueuedTx,
  RejectCode,
  RejectedTx,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { SqlClient } from "@effect/sql/SqlClient";
import { Duration, Effect, Metric, Ref, Schedule } from "effect";

import { MempoolLedgerDB, TxAdmissionsDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Globals, Lucid, NodeConfig } from "@/services/index.js";

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
  "Time from durable admission first_seen_at to validation_started_at in milliseconds",
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

const validationBatchDurationTimer = Metric.timer(
  "validation_batch_duration",
  "End-to-end validation batch duration in milliseconds",
);

const validationPhaseADurationTimer = Metric.timer(
  "validation_phase_a_duration",
  "Phase-A validation duration in milliseconds",
);

const validationPhaseBDurationTimer = Metric.timer(
  "validation_phase_b_duration",
  "Phase-B validation duration in milliseconds",
);

const validationMempoolInsertDurationTimer = Metric.timer(
  "validation_mempool_insert_duration",
  "Duration of accepted transaction inserts into MempoolDB",
);

const validationRejectionInsertDurationTimer = Metric.timer(
  "validation_rejection_insert_duration",
  "Duration of rejected transaction inserts into TxRejectionsDB",
);

const VALIDATION_BATCH_HARD_CAP = 1600;
const VALIDATION_MIN_BATCH = 128;
const VALIDATION_PHASE_A_MAX_EFFECTIVE_CONCURRENCY = 8;

let cachedUtxoState: Map<string, Buffer> | undefined;
let cachedUtxoStateVersion = -1;

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

/**
 * Normalizes one queued payload into either a validated queue entry or an
 * immediate rejection describing malformed binary fields.
 */
const admissionToQueuedTx = (admission: TxAdmissionsDB.Entry): QueuedTx => ({
  txId: admission.tx_id,
  txCbor: admission.tx_canonical_cbor,
  arrivalSeq: admission.arrival_seq,
  createdAt: admission.first_seen_at,
});

/**
 * Loads and caches the current mempool-ledger pre-state until the version ref
 * changes.
 */
const ensureCachedUtxoState = (): Effect.Effect<
  Map<string, Buffer>,
  DatabaseError,
  SqlClient | Globals
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const currentVersion = yield* Ref.get(globals.MEMPOOL_LEDGER_VERSION);

    if (
      cachedUtxoState !== undefined &&
      cachedUtxoStateVersion === currentVersion
    ) {
      return cachedUtxoState;
    }

    const preStateEntries = yield* MempoolLedgerDB.retrieveSpendable;
    const state = new Map<string, Buffer>();
    for (const entry of preStateEntries) {
      state.set(entry.outref.toString("hex"), entry.output);
    }
    cachedUtxoState = state;
    cachedUtxoStateVersion = currentVersion;
    return state;
  });

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
): number => {
  const maxBatchSize = clamp(configuredBatchSize, 1, VALIDATION_BATCH_HARD_CAP);
  const minBatchSize = Math.min(maxBatchSize, VALIDATION_MIN_BATCH);
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

/**
 * Chooses the phase-A concurrency level for the current batch size.
 */
const selectPhaseAConcurrency = (
  configuredConcurrency: number,
  batchLength: number,
): number => {
  const configured = clamp(
    configuredConcurrency,
    1,
    VALIDATION_PHASE_A_MAX_EFFECTIVE_CONCURRENCY,
  );
  if (configured === 1) {
    return 1;
  }
  if (batchLength < 256) {
    return 1;
  }
  if (batchLength < 512) {
    return Math.min(configured, 2);
  }
  if (batchLength < 1024) {
    return Math.min(configured, 4);
  }
  return configured;
};

/**
 * Runs one queue-processing tick, draining queued payloads and validating an
 * effective batch against the current mempool-ledger pre-state.
 */
const txQueueProcessorAction = (): Effect.Effect<
  void,
  DatabaseError,
  SqlClient | NodeConfig | Globals | Lucid
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const { api: lucid } = yield* Lucid;
    yield* Ref.set(globals.HEARTBEAT_TX_QUEUE_PROCESSOR, Date.now());
    const nodeConfig = yield* NodeConfig;
    const localFinalizationPending = yield* Ref.get(
      globals.LOCAL_FINALIZATION_PENDING,
    );

    const expiredLeaseCount = yield* TxAdmissionsDB.requeueExpiredLeases;
    const durableBacklog = yield* TxAdmissionsDB.countBacklog;
    const totalQueueDepth = Number(durableBacklog);
    yield* validationQueueDepthGauge(Effect.succeed(durableBacklog));

    if (localFinalizationPending) {
      yield* validationBatchSizeGauge(Effect.succeed(0n));
      yield* validationWorkerUtilizationGauge(Effect.succeed(0));
      yield* Effect.logDebug(
        "tx-queue processor paused while local finalization recovery is pending",
      );
      return;
    }

    if (durableBacklog === 0n) {
      yield* validationBatchSizeGauge(Effect.succeed(0n));
      yield* validationWorkerUtilizationGauge(Effect.succeed(0));
      yield* validationOldestQueuedTxAgeGauge(Effect.succeed(0));
      return;
    }

    const oldestAgeMillis = yield* TxAdmissionsDB.oldestQueuedAgeMs;
    yield* validationOldestQueuedTxAgeGauge(
      Effect.succeed(Math.max(0, oldestAgeMillis)),
    );
    const batchSize = selectValidationBatchSize(
      nodeConfig.VALIDATION_BATCH_SIZE,
      totalQueueDepth,
    );

    const leaseOwner = `tx-queue-processor:${process.pid}:${Date.now()}`;
    const admittedRows = yield* TxAdmissionsDB.claimBatch({
      limit: batchSize,
      leaseOwner,
      leaseDurationMs: nodeConfig.VALIDATION_LEASE_MS,
    });

    if (admittedRows.length === 0) {
      yield* validationBatchSizeGauge(Effect.succeed(0n));
      yield* validationWorkerUtilizationGauge(Effect.succeed(0));
      return;
    }

    const queueWaits = admittedRows.map((row) =>
      Math.max(
        0,
        (row.validation_started_at ?? new Date()).getTime() -
          row.first_seen_at.getTime(),
      ),
    );
    for (const waitMs of queueWaits) {
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

    try {
      const batchStart = Date.now();
      const queuedTxs = admittedRows.map(admissionToQueuedTx);

      const phaseAStart = Date.now();
      const phaseAConcurrency = selectPhaseAConcurrency(
        nodeConfig.VALIDATION_PHASE_A_CONCURRENCY,
        queuedTxs.length,
      );
      const phaseA = yield* runPhaseAValidation(queuedTxs, {
        expectedNetworkId: nodeConfig.NETWORK === "Mainnet" ? 1n : 0n,
        minFeeA: nodeConfig.MIN_FEE_A,
        minFeeB: nodeConfig.MIN_FEE_B,
        concurrency: phaseAConcurrency,
        strictnessProfile: nodeConfig.VALIDATION_STRICTNESS_PROFILE,
      });
      yield* validationPhaseAConcurrencyGauge(
        Effect.succeed(BigInt(phaseAConcurrency)),
      );
      yield* validationPhaseALatencyGauge(
        Effect.succeed(Date.now() - phaseAStart),
      );
      yield* validationPhaseADurationTimer(
        Effect.succeed(Duration.millis(Date.now() - phaseAStart)),
      );

      const cachedState = yield* ensureCachedUtxoState();
      const phaseBStart = Date.now();
      const phaseB = yield* runPhaseBValidationWithPatch(
        phaseA.accepted,
        cachedState,
        {
          nowCardanoSlotNo: BigInt(lucid.currentSlot()),
          bucketConcurrency: nodeConfig.VALIDATION_G4_BUCKET_CONCURRENCY,
          enforceScriptBudget: true,
        },
      );
      yield* validationPhaseBLatencyGauge(
        Effect.succeed(Date.now() - phaseBStart),
      );
      yield* validationPhaseBDurationTimer(
        Effect.succeed(Duration.millis(Date.now() - phaseBStart)),
      );

      const allRejected = [...phaseA.rejected, ...phaseB.rejected];

      if (allRejected.length > 0) {
        const rejectionInsertStart = Date.now();
        yield* TxAdmissionsDB.markRejected({
          rows: admittedRows,
          leaseOwner,
          rejectedTxs: allRejected,
        });
        yield* validationRejectionInsertDurationTimer(
          Effect.succeed(Duration.millis(Date.now() - rejectionInsertStart)),
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
          processedTxs: phaseB.accepted.map(processedTxFromValidatedTx),
        });
        yield* validationMempoolInsertDurationTimer(
          Effect.succeed(Duration.millis(Date.now() - mempoolInsertStart)),
        );
        yield* Metric.incrementBy(
          validationAcceptCounter,
          BigInt(phaseB.accepted.length),
        );
      }
      applyUTxOStatePatch(cachedState, phaseB.statePatch);
      cachedUtxoState = cachedState;
      cachedUtxoStateVersion = yield* Ref.get(globals.MEMPOOL_LEDGER_VERSION);

      yield* Effect.logInfo(
        `tx-queue validation batch done: queued=${admittedRows.length}, accepted=${phaseB.accepted.length}, rejected=${allRejected.length}, expired_leases_requeued=${expiredLeaseCount}, queue_wait_max_ms=${maxQueueWaitMs.toString()}, rejected_by_code=[${summarizeRejections(allRejected)}]`,
      );
      yield* validationBatchDurationTimer(
        Effect.succeed(Duration.millis(Date.now() - batchStart)),
      );
    } catch (error) {
      yield* TxAdmissionsDB.releaseForRetry({
        txIds: admittedRows.map((row) => row.tx_id),
        leaseOwner,
        delayMs: nodeConfig.VALIDATION_RETRY_BACKOFF_BASE_MS,
      });
      throw error;
    }
  });

const txQueueProcessorDrainLoop = (): Effect.Effect<
  void,
  DatabaseError,
  SqlClient | NodeConfig | Globals | Lucid
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    let keepDraining = true;
    while (keepDraining) {
      yield* Ref.set(globals.TX_QUEUE_WAKE_REQUESTED, false);
      yield* txQueueProcessorAction();
      keepDraining = yield* Ref.get(globals.TX_QUEUE_WAKE_REQUESTED);
      if (keepDraining) {
        yield* Metric.increment(validationCoalescedWakeupCounter);
      }
    }
  });

export const txQueueProcessorDrainOnce = (): Effect.Effect<
  void,
  DatabaseError,
  SqlClient | NodeConfig | Globals | Lucid
> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const active = yield* Ref.get(globals.TX_QUEUE_PROCESSOR_ACTIVE);
    if (active) {
      yield* Ref.set(globals.TX_QUEUE_WAKE_REQUESTED, true);
      yield* Metric.increment(validationCoalescedWakeupCounter);
      return;
    }
    yield* Ref.set(globals.TX_QUEUE_PROCESSOR_ACTIVE, true);
    yield* txQueueProcessorDrainLoop().pipe(
      Effect.ensuring(Ref.set(globals.TX_QUEUE_PROCESSOR_ACTIVE, false)),
    );
  });

export const requestTxQueueProcessorWakeup: Effect.Effect<
  void,
  never,
  SqlClient | NodeConfig | Globals | Lucid
> = Effect.gen(function* () {
  const globals = yield* Globals;
  yield* Ref.set(globals.TX_QUEUE_WAKE_REQUESTED, true);
  yield* Metric.increment(validationEventWakeupCounter);
  yield* Effect.forkDaemon(
    txQueueProcessorDrainOnce().pipe(Effect.catchAllCause(Effect.logWarning)),
  );
});

/**
 * Fiber wrapper that repeats queue-drain and validation work on the provided
 * schedule.
 */
export const txQueueProcessorFiber = (
  schedule: Schedule.Schedule<number>,
): Effect.Effect<void, never, SqlClient | NodeConfig | Globals | Lucid> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔶 Tx queue processor fiber started.");
    yield* repeatScheduledWithCauseLogging(
      txQueueProcessorDrainOnce(),
      schedule,
    );
  });
