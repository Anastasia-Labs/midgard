import type { MidgardCekProgramEnvelope } from "@al-ft/midgard-core/cek-proof";
import {
  type PhaseAValidatedTx,
  processedTxFromValidatedTx,
} from "@al-ft/midgard-validation";
import { Duration, Effect, Metric } from "effect";

import { TxAdmissionsDB } from "../database/index.js";
import { NodeConfig } from "../services/index.js";
import {
  type AdmissionDeferral,
  validationAcceptCounter,
  validationMempoolInsertDurationTimer,
  validationRejectCounter,
  validationRejectionInsertDurationTimer,
} from "./tx-queue-processor.classify-plutus-evaluation-failure.js";

/**
 * Persists one decided admission batch under its validation lease: every
 * deferred transaction goes back to the queue with its submitted bytes and
 * the node's retry backoff, every rejection becomes terminal, and every
 * accepted transaction enters the mempool.
 */
export const persistAdmissionDecision = ({
  rows,
  leaseOwner,
  decision,
  programEnvelopesByTxId,
}: {
  readonly rows: readonly Pick<
    TxAdmissionsDB.Entry,
    TxAdmissionsDB.Columns.TX_ID
  >[];
  readonly leaseOwner: string;
  readonly decision: {
    readonly phaseB: { readonly accepted: readonly PhaseAValidatedTx[] };
    readonly allRejected: readonly TxAdmissionsDB.AdmissionRejection[];
    readonly deferred: readonly AdmissionDeferral[];
  };
  readonly programEnvelopesByTxId: ReadonlyMap<
    string,
    readonly MidgardCekProgramEnvelope[]
  >;
}) =>
  Effect.gen(function* () {
    const { phaseB, allRejected, deferred } = decision;
    if (deferred.length > 0) {
      const nodeConfig = yield* NodeConfig;
      yield* TxAdmissionsDB.releaseForRetry({
        txIds: deferred.map((deferral) => deferral.txId),
        leaseOwner,
        baseDelayMs: nodeConfig.VALIDATION_RETRY_BACKOFF_BASE_MS,
        maxDelayMs: nodeConfig.VALIDATION_RETRY_BACKOFF_MAX_MS,
      });
    }
    if (allRejected.length > 0) {
      const rejectionInsertStart = Date.now();
      yield* TxAdmissionsDB.markRejected({
        rows,
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
        rows,
        leaseOwner,
        processedTxs: phaseB.accepted.map(processedTxFromValidatedTx),
        programEnvelopesByTxId,
      });
      yield* validationMempoolInsertDurationTimer(
        Effect.succeed(Duration.millis(Date.now() - mempoolInsertStart)),
      );
      yield* Metric.incrementBy(
        validationAcceptCounter,
        BigInt(phaseB.accepted.length),
      );
    }
  });
