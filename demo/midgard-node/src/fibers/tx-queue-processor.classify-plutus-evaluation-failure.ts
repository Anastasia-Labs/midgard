import {
  type PhaseAResult,
  type PhaseAValidatedTx,
  type PhaseBLocalRefusalError,
  RejectCodes,
  runPhaseBValidationWithPatch,
  ScriptWallCapExceededError,
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

export const ADMISSION_REJECT_CODE_SCRIPT_WALL_CAP =
  "E_ADMISSION_SCRIPT_WALL_CAP";

export const validationSubmitWallCapRefusalCounter = Metric.counter(
  "validation_submit_wall_cap_refusal_count",
  {
    description:
      "Total number of txs deferred at admission because their script runs passed the submit wall cap",
    bigint: true,
    incremental: true,
  },
);

export const ADMISSION_REJECT_CODE_SCRIPT_OUTSIDE_GATED_SET =
  "E_ADMISSION_SCRIPT_OUTSIDE_GATED_SET";

export const validationSubmitOutsideGatedSetRefusalCounter = Metric.counter(
  "validation_submit_outside_gated_set_refusal_count",
  {
    description:
      "Total number of txs refused at admission because their script runs reached a builtin or machine arm outside the gated set",
    bigint: true,
    incremental: true,
  },
);

/**
 * The admission code and counter of a node-local Phase B refusal. A wall-cap
 * refusal is deferred: it depends on this node's load, not on the
 * transaction, so the transaction stays queued and is decided again later. A
 * gated-set refusal is terminal: the same script reaches the same arm under
 * the same gated set.
 */
const localRefusalOf = (failure: PhaseBLocalRefusalError) =>
  failure instanceof ScriptWallCapExceededError
    ? {
        code: ADMISSION_REJECT_CODE_SCRIPT_WALL_CAP,
        counter: validationSubmitWallCapRefusalCounter,
        deferred: true,
      }
    : {
        code: ADMISSION_REJECT_CODE_SCRIPT_OUTSIDE_GATED_SET,
        counter: validationSubmitOutsideGatedSetRefusalCounter,
        deferred: false,
      };

/** A transaction left queued, with why, to be decided again later. */
export type AdmissionDeferral = Readonly<{
  txId: Buffer;
  code: string;
  detail: string;
}>;

/**
 * Runs Phase B once, refusing each candidate whose script runs pass the
 * submit wall cap (`scriptWallCapMs`) or leave the gated set (`cekGatedSet`)
 * and deciding the rest as if the refused ones were absent. The refusal is
 * the node's own, under its own code, and records no script verdict. A
 * gated-set refusal is terminal, and every candidate that depends on the
 * refused transaction's outputs is rejected as depending on it. A wall-cap
 * refusal is deferred together with those dependents, so that all of them are
 * decided again later. Every other Phase B failure fails the batch as before.
 */
export const runPhaseBRefusingLocalBreaches = (
  candidates: readonly PhaseAValidatedTx[],
  ledgerState: Parameters<typeof runPhaseBValidationWithPatch>[1],
  phaseBConfig: Parameters<typeof runPhaseBValidationWithPatch>[2],
) =>
  Effect.gen(function* () {
    const phaseB = yield* runPhaseBValidationWithPatch(
      candidates,
      ledgerState,
      { ...phaseBConfig, reportLocalRefusals: true },
    );
    const refused: TxAdmissionsDB.AdmissionRejection[] = [];
    const deferred: AdmissionDeferral[] = [];
    for (const {
      refusal: breach,
      candidate,
      dependents,
    } of phaseB.localRefusals ?? []) {
      const refusal = localRefusalOf(breach);
      const txId = Buffer.from(candidate.ledgerTx.txId);
      const txIdHex = txId.toString("hex");
      yield* Metric.increment(refusal.counter);
      yield* Effect.logWarning(
        `${refusal.deferred ? "Deferring" : "Refusing"} tx ${txIdHex} at admission: ${breach.message}`,
      );
      (refusal.deferred ? deferred : refused).push({
        txId,
        code: refusal.code,
        detail: breach.message,
      });
      for (const dependent of dependents) {
        const dependentTxId = Buffer.from(dependent.ledgerTx.txId);
        if (refusal.deferred) {
          const detail = `depends on deferred tx ${txIdHex}`;
          yield* Effect.logWarning(
            `Deferring tx ${dependentTxId.toString("hex")} at admission: ${detail}`,
          );
          deferred.push({ txId: dependentTxId, code: refusal.code, detail });
        } else {
          refused.push({
            txId: dependentTxId,
            code: RejectCodes.DependsOnRejectedTx,
            detail: `depends on refused tx ${txIdHex}`,
          });
        }
      }
    }
    return { phaseB, refused, deferred };
  });

/**
 * Decides one admission batch against the ledger state: Phase-A survivors
 * that spend an outref named by a pending withdrawal are refused, and the rest
 * run Phase B, which refuses any whose scripts pass the submit wall cap or
 * leave the gated set.
 * Returns Phase B's result, every rejection of the batch, and the
 * transactions deferred to a later batch.
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
    const { phaseB, refused, deferred } = yield* runPhaseBRefusingLocalBreaches(
      admissible.accepted,
      ledgerState,
      phaseBConfig,
    );
    const allRejected: readonly TxAdmissionsDB.AdmissionRejection[] = [
      ...phaseA.rejected,
      ...admissible.rejected,
      ...refused,
      ...phaseB.rejected,
    ];
    return { phaseB, allRejected, deferred };
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
