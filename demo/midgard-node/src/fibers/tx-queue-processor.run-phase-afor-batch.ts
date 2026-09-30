import "./tx-queue-processor.classify-plutus-evaluation-failure.js";

import type { MidgardCekProgramEnvelope } from "@al-ft/midgard-core/cek-proof";
import { decodeMidgardNativeTxFullFromCanonicalCbor } from "@al-ft/midgard-core/codec";
import { collectMidgardReferencedProgramEnvelopes } from "@al-ft/midgard-core/script-proof";
import {
  LedgerColumns,
  type PhaseAResult,
  QueuedTx,
  runPhaseAValidation,
} from "@al-ft/midgard-validation";
import { Cause, Chunk, Effect, Ref, Schedule } from "effect";

import { TxAdmissionsDB } from "../database/index.js";
import { isHistoryProducerGateClosed } from "../services/event-history-producer.js";
import {
  type NodeConfigDep,
  type ValidationPoolService,
} from "../services/index.js";

/** True when every failure in `cause` is a producer refused by a closed
 * history gate, with no defect: the concurrent drains of one iteration fail
 * (and interrupt each other) together while the owner recovers. */
export const isHistoryGateClosedCause = (cause: Cause.Cause<unknown>) => {
  const failures = Chunk.toReadonlyArray(Cause.failures(cause));
  return (
    failures.length > 0 &&
    Chunk.isEmpty(Cause.defects(cause)) &&
    failures.every(isHistoryProducerGateClosed)
  );
};

export const HISTORY_GATE_CLOSED_MESSAGE =
  "Transaction queue paused while the history owner recovers; admission resumes when its gate reopens.";

/**
 * Repeats a scheduled background action while logging and swallowing per-iteration
 * failures so the loop survives transient outages. A history gate closed for
 * a planned recovery is expected: it is logged once per closure at info,
 * without a stack, and then at debug until an iteration succeeds. Every other
 * failure keeps its full cause at WARN.
 */
export const repeatScheduledWithCauseLogging = <R>(
  action: Effect.Effect<void, unknown, R>,
  schedule: Schedule.Schedule<number>,
): Effect.Effect<void, never, R> =>
  Effect.gen(function* () {
    const gateClosedReported = yield* Ref.make(false);
    yield* Effect.repeat(
      action.pipe(
        Effect.zipRight(Ref.set(gateClosedReported, false)),
        Effect.catchAllCause((cause) =>
          isHistoryGateClosedCause(cause)
            ? Ref.getAndSet(gateClosedReported, true).pipe(
                Effect.flatMap((reported) =>
                  reported
                    ? Effect.logDebug(HISTORY_GATE_CLOSED_MESSAGE)
                    : Effect.logInfo(HISTORY_GATE_CLOSED_MESSAGE),
                ),
              )
            : Effect.logWarning(cause),
        ),
      ),
      schedule,
    );
  });

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
export const admissionToQueuedTx = (
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
): ReadonlyMap<string, readonly MidgardCekProgramEnvelope[]> => {
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
      const canonicalTx = decodeMidgardNativeTxFullFromCanonicalCbor(
        candidate.submission.txCbor,
      );
      return [
        Buffer.from(candidate.ledgerTx.txId).toString("hex"),
        collectMidgardReferencedProgramEnvelopes(canonicalTx, resolvedOutputs),
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
export const selectValidationBatchSize = (
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

export const runPhaseAForBatch = (
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
export type TxQueueProcessorActionResult = {
  readonly processed: boolean;
  readonly claimedCount: number;
  readonly batchSize: number;
};
