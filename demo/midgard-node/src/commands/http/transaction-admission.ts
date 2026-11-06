/**
 * The checks every transaction passes before durable admission, whoever sent
 * it: a user through `/submit` or another operator through `/peer/txs`. Both
 * routes run exactly these; the tx-queue processor then validates the
 * admitted bytes against this node's own ledger.
 */
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { validateMidgardConsensusTxCbor } from "@al-ft/midgard-core/consensus-validation";
import { SqlClient } from "@effect/sql/SqlClient";
import { Cause, Effect, Exit, Option } from "effect";

import { TxAdmissionsDB } from "../../database/index.js";
import { DatabaseError } from "../../database/utils/common.js";
import {
  admissionFailureDefinitelyDidNotInsert,
  commitAdmissionBacklogSlot,
  releaseAdmissionBacklogSlot,
  reserveAdmissionBacklogSlot,
} from "../../fibers/index.js";
import {
  AdmissionWriter,
  type AdmissionWriterShutdownError,
  NodeConfig,
} from "../../services/index.js";
import { publicSubmissionDiagnostic } from "../listen-public-error-presentation.js";
import {
  type NormalizedSubmitTx,
  normalizeSubmitTxCanonicalCborToNative,
  validateSubmitTxCanonicalCbor,
} from "../listen-utils.js";

export type SubmittedTxCheck =
  | {
      readonly ok: true;
      readonly normalized: Extract<NormalizedSubmitTx, { ok: true }>;
    }
  | {
      readonly ok: false;
      readonly status: 400 | 413;
      readonly body: Readonly<Record<string, unknown>>;
      /** What the node logs about the refusal. */
      readonly log: readonly string[];
    };

/** Size, canonical form, native normalization and the consensus profile. */
export const checkSubmittedTransaction = (
  transactionBytes: Uint8Array,
  maxSubmitTxCborBytes: number,
): SubmittedTxCheck => {
  const validation = validateSubmitTxCanonicalCbor(
    transactionBytes,
    Math.min(
      maxSubmitTxCborBytes,
      MIDGARD_CONSENSUS_LIMITS.maxTxCanonicalCborBytes,
    ),
  );
  if (!validation.ok)
    return {
      ok: false,
      status: validation.status,
      body: {
        error: publicSubmissionDiagnostic(
          validation.error,
          "E_INVALID_CANONICAL_TX",
        ),
      },
      log: [`Submit rejected: ${validation.error}`],
    };
  const normalized = normalizeSubmitTxCanonicalCborToNative(
    validation.txCanonicalCbor,
  );
  if (!normalized.ok)
    return {
      ok: false,
      status: 400,
      body: {
        error: publicSubmissionDiagnostic(
          normalized.error,
          "E_INVALID_NATIVE_TX",
        ),
      },
      log: [normalized.error, normalized.detail],
    };
  const proofViolation = validateMidgardConsensusTxCbor(
    normalized.txCanonicalCbor,
  );
  if (proofViolation !== null)
    return {
      ok: false,
      status: 400,
      body: {
        error: proofViolation.code,
        feature: proofViolation.featureId,
        detail: proofViolation.detail,
      },
      log: [],
    };
  return { ok: true, normalized };
};

/**
 * Durably admits a checked transaction under `submitSource`, within the
 * node's admission backlog bounds, and wakes the tx-queue processor when the
 * row is new.
 */
export const admitCheckedTransaction = <R>({
  normalized,
  submitSource,
  wakeTxQueueProcessor,
}: {
  readonly normalized: Extract<NormalizedSubmitTx, { ok: true }>;
  readonly submitSource: "native" | "peer";
  readonly wakeTxQueueProcessor: Effect.Effect<void, never, R>;
}) =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const reservation = yield* reserveAdmissionBacklogSlot(
      nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG,
    );
    const admissionWriter = yield* AdmissionWriter;
    const admissionEffect: Effect.Effect<
      TxAdmissionsDB.AdmitResult,
      | DatabaseError
      | TxAdmissionsDB.TxAdmissionConflictError
      | TxAdmissionsDB.TxAdmissionBacklogFullError
      | AdmissionWriterShutdownError,
      SqlClient
    > = reservation.reserved
      ? admissionWriter.admitReserved({
          txId: normalized.txId,
          txCanonicalCbor: normalized.txCanonicalCbor,
          submitSource,
          maxBacklogBytes: nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG_BYTES,
        })
      : TxAdmissionsDB.admit({
          txId: normalized.txId,
          txCanonicalCbor: normalized.txCanonicalCbor,
          submitSource,
          currentBacklog: reservation.currentBacklog,
          maxBacklog: nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG,
          maxBacklogBytes: nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG_BYTES,
        });
    const admitted = yield* admissionEffect.pipe(
      Effect.onExit((exit) => {
        if (!reservation.reserved) return Effect.void;
        if (Exit.isSuccess(exit)) {
          return exit.value.kind === "new"
            ? commitAdmissionBacklogSlot
            : releaseAdmissionBacklogSlot;
        }
        const failure = Option.getOrUndefined(Cause.failureOption(exit.cause));
        // Conflict/backlog errors prove no row was inserted. A SqlError,
        // interruption, or defect can be observed after PostgreSQL committed,
        // so retain the slot conservatively until the next live-count refresh.
        return admissionFailureDefinitelyDidNotInsert(failure)
          ? releaseAdmissionBacklogSlot
          : commitAdmissionBacklogSlot;
      }),
    );
    if (admitted.kind === "new") {
      if (!reservation.reserved) {
        return yield* Effect.dieMessage(
          "Durable admission inserted without a reserved backlog slot",
        );
      }
      yield* wakeTxQueueProcessor;
    }
    return admitted;
  });
