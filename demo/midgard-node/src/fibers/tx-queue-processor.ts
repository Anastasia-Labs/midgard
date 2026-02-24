import { Chunk, Effect, Metric, pipe, Queue, Schedule } from "effect";
import {
  MempoolDB,
  MempoolLedgerDB,
  TxRejectionsDB,
} from "@/database/index.js";
import { SqlClient } from "@effect/sql/SqlClient";
import { DatabaseError } from "@/database/utils/common.js";
import { NodeConfig } from "@/services/index.js";
import {
  QueuedTx,
  QueuedTxPayload,
  RejectCode,
  RejectCodes,
  RejectedTx,
  runPhaseAValidation,
  runPhaseBValidation,
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

  const queuedTx: QueuedTx = {
    txId: payload.txId,
    txCbor: payload.txCbor,
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

const txQueueProcessorAction = (
  txQueue: Queue.Dequeue<QueuedTxPayload>,
  withMonitoring?: boolean,
): Effect.Effect<void, DatabaseError, SqlClient | NodeConfig> =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const batchSize = Math.max(1, nodeConfig.VALIDATION_BATCH_SIZE);
    const queueSize = yield* txQueue.size;

    const totalQueueDepth = queueSize + pendingPayloads.length;
    if (withMonitoring) {
      yield* txQueueSizeGauge(Effect.succeed(BigInt(totalQueueDepth)));
    }

    yield* validationQueueDepthGauge(Effect.succeed(BigInt(totalQueueDepth)));

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
    const phaseA = yield* runPhaseAValidation(queuedTxs, {
      expectedNetworkId: nodeConfig.NETWORK === "Mainnet" ? 1n : 0n,
      minFeeA: nodeConfig.MIN_FEE_A,
      minFeeB: nodeConfig.MIN_FEE_B,
      concurrency: nodeConfig.VALIDATION_PHASE_A_CONCURRENCY,
      strictnessProfile: nodeConfig.VALIDATION_STRICTNESS_PROFILE,
    });
    yield* validationPhaseALatencyGauge(
      Effect.succeed(Date.now() - phaseAStart),
    );

    const cachedState = yield* ensureCachedUtxoState();
    const workingState = new Map(cachedState);
    const phaseBStart = Date.now();
    const phaseB = yield* runPhaseBValidation(phaseA.accepted, workingState, {
      nowMillis: BigInt(Date.now()),
      bucketConcurrency: nodeConfig.VALIDATION_G4_BUCKET_CONCURRENCY,
    });
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
    cachedUtxoState = workingState;

    yield* Effect.logInfo(
      `tx-queue validation batch done: queued=${txPayloads.length}, accepted=${phaseB.accepted.length}, rejected=${allRejected.length}, rejected_by_code=[${summarizeRejections(allRejected)}]`,
    );
  });

export const txQueueProcessorFiber = (
  schedule: Schedule.Schedule<number>,
  txQueue: Queue.Dequeue<QueuedTxPayload>,
  withMonitoring?: boolean,
): Effect.Effect<void, never, SqlClient | NodeConfig> =>
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
