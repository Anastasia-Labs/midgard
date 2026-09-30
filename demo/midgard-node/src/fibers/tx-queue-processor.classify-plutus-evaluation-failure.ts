import {
  type PhaseAResult,
  type PhaseAValidatedTx,
  runPhaseBValidationWithPatch,
} from "@al-ft/midgard-validation";
import { Effect, Metric } from "effect";

import { TxAdmissionsDB } from "../database/index.js";

/**
 * Background validation loop for queued L2 transactions.
 *
 * The processor batches queued payloads, runs phase-A and phase-B validation,
 * applies accepted state patches to the mempool ledger, and records rejections
 * for later inspection.
 */
export const validationPhaseALatencyGauge = Metric.gauge(
  "validation_phase_a_latency_ms",
  {
    description: "Phase-A validation latency in milliseconds",
  },
);

export const validationPhaseBLatencyGauge = Metric.gauge(
  "validation_phase_b_latency_ms",
  {
    description: "Phase-B validation latency in milliseconds",
  },
);

export const validationBatchSizeGauge = Metric.gauge("validation_batch_size", {
  description: "Number of queued txs fetched for a validation batch",
  bigint: true,
});

export const validationAcceptCounter = Metric.counter(
  "validation_accept_count",
  {
    description: "Total number of txs accepted by phase-1 validation",
    bigint: true,
    incremental: true,
  },
);

export const validationRejectCounter = Metric.counter(
  "validation_reject_count",
  {
    description: "Total number of txs rejected by phase-1 validation",
    bigint: true,
    incremental: true,
  },
);

export const validationQueueDepthGauge = Metric.gauge(
  "validation_queue_depth",
  {
    description: "Current number of queued transactions awaiting validation",
    bigint: true,
  },
);

export const validationWorkerUtilizationGauge = Metric.gauge(
  "validation_worker_utilization",
  {
    description:
      "Fraction of configured batch capacity used in the latest validation batch (0-1)",
  },
);

export const validationPhaseAConcurrencyGauge = Metric.gauge(
  "validation_phase_a_effective_concurrency",
  {
    description: "Effective Phase-A validation concurrency selected per batch",
    bigint: true,
  },
);

export const validationOldestQueuedTxAgeGauge = Metric.gauge(
  "validation_oldest_queued_tx_age_ms",
  {
    description: "Age of the oldest transaction waiting for validation",
  },
);

export const validationQueueWaitDurationTimer = Metric.timer(
  "validation_queue_wait_duration",
  "Deterministic uniform sample (up to 64 per batch, including endpoints and max) of first_seen_at to validation_started_at milliseconds",
);

export const validationQueueWaitMaxGauge = Metric.gauge(
  "validation_queue_wait_max_ms",
  {
    description:
      "Maximum first_seen_at to validation_started_at wait in the latest claimed batch",
  },
);

export const validationEventWakeupCounter = Metric.counter(
  "validation_event_wakeup_count",
  {
    description:
      "Number of event-driven tx queue wakeups requested after durable admission",
    bigint: true,
    incremental: true,
  },
);

export const validationCoalescedWakeupCounter = Metric.counter(
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

export const validationRejectionInsertDurationTimer = Metric.timer(
  "validation_rejection_insert_duration",
  "Duration of rejected transaction inserts into TxRejectionsDB",
);

export const validationDrainLoopsActiveGauge = Metric.gauge(
  "validation_drain_loops_active",
  {
    description: "Concurrent validation drain loops currently active",
    bigint: true,
  },
);

export const ADMISSION_REJECT_CODE_PENDING_WITHDRAWAL_INPUT =
  "E_ADMISSION_PENDING_WITHDRAWAL_INPUT";

/**
 * Refuses Phase-A survivors that spend an outref named by a pending
 * withdrawal. The commit stage rejects such a spend, and by then its effects
 * would already be in mempool_ledger.
 */
export const refusePendingWithdrawalInputs = (
  candidates: readonly PhaseAValidatedTx[],
  pendingWithdrawalOutRefHexes: ReadonlySet<string>,
): {
  readonly accepted: readonly PhaseAValidatedTx[];
  readonly rejected: readonly TxAdmissionsDB.AdmissionRejection[];
} => {
  const accepted: PhaseAValidatedTx[] = [];
  const rejected: TxAdmissionsDB.AdmissionRejection[] = [];
  for (const candidate of candidates) {
    const outRef = candidate.graph.spentOutRefHexes.find((spent) =>
      pendingWithdrawalOutRefHexes.has(spent),
    );
    if (outRef === undefined) accepted.push(candidate);
    else
      rejected.push({
        txId: Buffer.from(candidate.ledgerTx.txId),
        code: ADMISSION_REJECT_CODE_PENDING_WITHDRAWAL_INPUT,
        detail: `Transaction spends L2 outref ${outRef}, which a pending withdrawal names`,
      });
  }
  return { accepted, rejected };
};

/**
 * Decides one admission batch against the ledger state: Phase-A survivors
 * that spend an outref named by a pending withdrawal are refused, and the rest
 * run Phase B. Returns Phase B's result and every rejection of the batch.
 */
export const decideAdmissionBatch = ({
  phaseA,
  pendingWithdrawalOutRefHexes,
  ledgerState,
  phaseBConfig,
}: {
  readonly phaseA: PhaseAResult;
  readonly pendingWithdrawalOutRefHexes: ReadonlySet<string>;
  readonly ledgerState: Parameters<typeof runPhaseBValidationWithPatch>[1];
  readonly phaseBConfig: Parameters<typeof runPhaseBValidationWithPatch>[2];
}) =>
  Effect.gen(function* () {
    const admissible = refusePendingWithdrawalInputs(
      phaseA.accepted,
      pendingWithdrawalOutRefHexes,
    );
    const phaseB = yield* runPhaseBValidationWithPatch(
      admissible.accepted,
      ledgerState,
      phaseBConfig,
    );
    const allRejected: readonly TxAdmissionsDB.AdmissionRejection[] = [
      ...phaseA.rejected,
      ...admissible.rejected,
      ...phaseB.rejected,
    ];
    return { phaseB, allRejected };
  });

/**
 * Summarizes a rejection batch into a compact per-code counter string for
 * logs.
 */
export const summarizeRejections = (
  rejected: readonly TxAdmissionsDB.AdmissionRejection[],
): string => {
  if (rejected.length === 0) {
    return "none";
  }

  const perCode = rejected.reduce((acc, r) => {
    const count = acc.get(r.code) ?? 0;
    acc.set(r.code, count + 1);
    return acc;
  }, new Map<string, number>());

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
