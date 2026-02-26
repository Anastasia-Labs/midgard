import { Chunk, Effect, Metric, pipe, Queue, Ref, Schedule } from "effect";
import {
  MempoolDB,
  MempoolLedgerDB,
  TxRejectionsDB,
} from "@/database/index.js";
import { SqlClient } from "@effect/sql/SqlClient";
import { DatabaseError } from "@/database/utils/common.js";
import { Globals, NodeConfig } from "@/services/index.js";
import {
  QueuedTx,
  QueuedTxPayload,
  RejectCode,
  RejectCodes,
  RejectedTx,
  applyUTxOStatePatch,
  runPhaseAValidation,
  runPhaseBValidationWithPatch,
} from "@/validation/index.js";

const txQueueSizeGauge = Metric.gauge("tx_queue_size", {
  description: "A tracker for the size of the tx queue before processing",
  bigint: true,
});

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

const VALIDATION_BATCH_HARD_CAP = 800;
const VALIDATION_MIN_BATCH = 128;

let nextArrivalSeq = 0n;
let pendingPayloads: QueuedTxPayload[] = [];
let pendingSinceMillis: number | undefined;
let cachedUtxoState: Map<string, Buffer> | undefined;

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

const payloadToQueuedTx = (payload: QueuedTxPayload): QueuedTx | RejectedTx => {
  if (!Buffer.isBuffer(payload.txId) || !Buffer.isBuffer(payload.txCbor)) {
    return {
      txId: Buffer.alloc(32, 0),
      code: RejectCodes.CborDeserialization,
      detail: "Queued payload missing binary tx fields",
    };
  }
  if (
    payload.txBodyHashForWitnesses !== undefined &&
    (!Buffer.isBuffer(payload.txBodyHashForWitnesses) ||
      payload.txBodyHashForWitnesses.length !== 32)
  ) {
    return {
      txId: payload.txId,
      code: RejectCodes.CborDeserialization,
      detail: "Queued payload has invalid txBodyHashForWitnesses",
    };
  }

  const queuedTx: QueuedTx = {
    txId: payload.txId,
    txCbor: payload.txCbor,
    txBodyHashForWitnesses: payload.txBodyHashForWitnesses,
    arrivalSeq: nextArrivalSeq,
    createdAt: new Date(payload.createdAtMillis),
  };
  nextArrivalSeq += 1n;
  return queuedTx;
};

const ensureCachedUtxoState = (): Effect.Effect<
  Map<string, Buffer>,
  DatabaseError,
  SqlClient
> =>
  Effect.gen(function* () {
    if (cachedUtxoState !== undefined) {
      return cachedUtxoState;
    }

    const preStateEntries = yield* MempoolLedgerDB.retrieve;
    const state = new Map<string, Buffer>();
    for (const entry of preStateEntries) {
      state.set(entry.outref.toString("hex"), entry.output);
    }
    cachedUtxoState = state;
    return state;
  });

const clamp = (value: number, min: number, max: number): number =>
  Math.min(max, Math.max(min, value));

const selectValidationBatchSize = (
  configuredBatchSize: number,
  queueDepth: number,
): number => {
  const maxBatchSize = clamp(
    configuredBatchSize,
    1,
    Math.max(1, VALIDATION_BATCH_HARD_CAP),
  );
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

const selectPhaseAConcurrency = (
  configuredConcurrency: number,
  batchLength: number,
): number => {
  const configured = Math.max(1, configuredConcurrency);
  if (configured === 1) {
    return 1;
  }
  if (batchLength < 1600) {
    return 1;
  }
  if (batchLength < 3200) {
    return Math.min(configured, 4);
  }
  return Math.min(configured, 8);
};

const txQueueProcessorAction = (
  txQueue: Queue.Dequeue<QueuedTxPayload>,
  withMonitoring?: boolean,
): Effect.Effect<void, DatabaseError, SqlClient | NodeConfig | Globals> =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    yield* Ref.set(globals.HEARTBEAT_TX_QUEUE_PROCESSOR, Date.now());
    const nodeConfig = yield* NodeConfig;
    const configuredBatchSize = Math.max(1, nodeConfig.VALIDATION_BATCH_SIZE);
    const queueSize = yield* txQueue.size;
    const localFinalizationPending = yield* Ref.get(
      globals.LOCAL_FINALIZATION_PENDING,
    );

    const totalQueueDepth = queueSize + pendingPayloads.length;
    if (withMonitoring) {
      yield* txQueueSizeGauge(Effect.succeed(BigInt(totalQueueDepth)));
    }

    yield* validationQueueDepthGauge(Effect.succeed(BigInt(totalQueueDepth)));

    if (localFinalizationPending) {
      yield* validationBatchSizeGauge(Effect.succeed(0n));
      yield* validationWorkerUtilizationGauge(Effect.succeed(0));
      yield* Effect.logDebug(
        "tx-queue processor paused while local finalization recovery is pending",
      );
      return;
    }

    const drainedChunk: Chunk.Chunk<QueuedTxPayload> =
      yield* Queue.takeAll(txQueue);
    const drainedPayloads = Chunk.toReadonlyArray(drainedChunk);
    if (drainedPayloads.length > 0) {
      if (pendingPayloads.length === 0) {
        pendingSinceMillis = Date.now();
      }
      pendingPayloads.push(...drainedPayloads);
    }

    if (pendingPayloads.length === 0) {
      yield* validationBatchSizeGauge(Effect.succeed(0n));
      yield* validationWorkerUtilizationGauge(Effect.succeed(0));
      return;
    }

    const nowMillis = Date.now();
    const oldestAgeMillis =
      pendingSinceMillis === undefined ? 0 : nowMillis - pendingSinceMillis;
    const batchSize = selectValidationBatchSize(
      configuredBatchSize,
      pendingPayloads.length,
    );
    const shouldRunBatch =
      pendingPayloads.length >= batchSize ||
      oldestAgeMillis >= nodeConfig.VALIDATION_MAX_QUEUE_AGE_MS;

    if (!shouldRunBatch) {
      yield* validationBatchSizeGauge(
        Effect.succeed(BigInt(pendingPayloads.length)),
      );
      yield* validationWorkerUtilizationGauge(
        Effect.succeed(pendingPayloads.length / batchSize),
      );
      return;
    }

    const txPayloads = pendingPayloads.splice(0, batchSize);
    if (pendingPayloads.length === 0) {
      pendingSinceMillis = undefined;
    } else {
      pendingSinceMillis = nowMillis;
    }

    yield* validationBatchSizeGauge(Effect.succeed(BigInt(txPayloads.length)));
    const utilization = txPayloads.length / batchSize;
    yield* validationWorkerUtilizationGauge(Effect.succeed(utilization));

    const queuedTxs: QueuedTx[] = [];
    const decodeRejected: RejectedTx[] = [];
    for (const payload of txPayloads) {
      const decoded = payloadToQueuedTx(payload);
      if ("arrivalSeq" in decoded) {
        queuedTxs.push(decoded);
      } else {
        decodeRejected.push(decoded);
      }
    }

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

    const cachedState = yield* ensureCachedUtxoState();
    const phaseBStart = Date.now();
    const phaseB = yield* runPhaseBValidationWithPatch(
      phaseA.accepted,
      cachedState,
      {
        nowMillis: BigInt(Date.now()),
        bucketConcurrency: nodeConfig.VALIDATION_G4_BUCKET_CONCURRENCY,
      },
    );
    yield* validationPhaseBLatencyGauge(
      Effect.succeed(Date.now() - phaseBStart),
    );

    const allRejected = [
      ...decodeRejected,
      ...phaseA.rejected,
      ...phaseB.rejected,
    ];

    if (allRejected.length > 0) {
      yield* TxRejectionsDB.insertMany(
        allRejected.map((rejectedTx) => ({
          tx_id: rejectedTx.txId,
          reject_code: rejectedTx.code,
          reject_detail: rejectedTx.detail,
        })),
      );
      yield* Metric.incrementBy(
        validationRejectCounter,
        BigInt(allRejected.length),
      );
    }

    if (phaseB.accepted.length > 0) {
      yield* MempoolDB.insertMultiple(
        phaseB.accepted.map((acceptedTx) => acceptedTx.processedTx),
      );
      yield* Metric.incrementBy(
        validationAcceptCounter,
        BigInt(phaseB.accepted.length),
      );
    }
    applyUTxOStatePatch(cachedState, phaseB.statePatch);
    cachedUtxoState = cachedState;

    yield* Effect.logInfo(
      `tx-queue validation batch done: queued=${txPayloads.length}, accepted=${phaseB.accepted.length}, rejected=${allRejected.length}, rejected_by_code=[${summarizeRejections(allRejected)}]`,
    );
  });

export const txQueueProcessorFiber = (
  schedule: Schedule.Schedule<number>,
  txQueue: Queue.Dequeue<QueuedTxPayload>,
  withMonitoring?: boolean,
): Effect.Effect<void, never, SqlClient | NodeConfig | Globals> =>
  pipe(
    Effect.gen(function* () {
      yield* Effect.logInfo("🔶 Tx queue processor fiber started.");
      yield* Effect.repeat(
        txQueueProcessorAction(txQueue, withMonitoring),
        schedule,
      );
    }),
    Effect.catchAllCause(Effect.logWarning),
  );
