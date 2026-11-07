/**
 * The checks every transaction passes before durable admission, whoever sent
 * it: a user through `/submit` or another operator through `/peer/txs`. Both
 * routes run exactly these; the tx-queue processor then validates the
 * admitted bytes against this node's own ledger.
 */
import { computeMidgardNativeTxFullHashFromCanonicalCbor } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { validateMidgardConsensusTx } from "@al-ft/midgard-core/consensus-validation";
import { midgardOutRefToCborHex } from "@al-ft/midgard-validation";
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
  decodeSubmitTxCanonicalCbor,
  validateSubmitTxCanonicalCbor,
} from "../listen-utils.js";

/**
 * Everything later stages need about a checked tx, so none of them decodes it
 * again. Plain data, so it can cross a worker boundary.
 */
export type CheckedSubmittedTx = {
  readonly txId: Buffer;
  readonly txIdHex: string;
  /** The submitted canonical bytes; the check does not re-encode them. */
  readonly txCanonicalCbor: Buffer;
  /** `computeMidgardNativeTxFullHashFromCanonicalCbor` of those bytes. */
  readonly txFullHash: Buffer;
  /** The §5.3 outref items (hex) the tx spends, in field order. */
  readonly spentOutRefs: readonly string[];
  /** The §5.3 outref items (hex) of the outputs the tx produces. */
  readonly producedOutRefs: readonly string[];
  readonly fee: bigint;
  readonly byteLength: number;
  readonly source: "native";
};

export type SubmittedTxCheck =
  | {
      readonly ok: true;
      readonly normalized: CheckedSubmittedTx;
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
  const decoded = decodeSubmitTxCanonicalCbor(validation.txCanonicalCbor);
  if (!decoded.ok)
    return {
      ok: false,
      status: 400,
      body: {
        error: publicSubmissionDiagnostic(decoded.error, "E_INVALID_NATIVE_TX"),
      },
      log: [decoded.error, decoded.detail],
    };
  // The consensus check runs on the decode the envelope was built from; its
  // `…Cbor` wrapper would decode the same bytes a second time.
  const proofViolation = validateMidgardConsensusTx(
    decoded.nativeTx,
    validation.byteLength,
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
  const { ledgerTx, txCbor } = decoded.submittedTx;
  return {
    ok: true,
    normalized: {
      txId: ledgerTx.txId,
      txIdHex: ledgerTx.txId.toString("hex"),
      txCanonicalCbor: txCbor,
      txFullHash: computeMidgardNativeTxFullHashFromCanonicalCbor(txCbor),
      spentOutRefs: ledgerTx.spendInputs.map(midgardOutRefToCborHex),
      producedOutRefs: ledgerTx.outputs.map((_, index) =>
        midgardOutRefToCborHex({ txId: ledgerTx.txId, index: BigInt(index) }),
      ),
      fee: ledgerTx.fee,
      byteLength: validation.byteLength,
      source: "native",
    },
  };
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
  readonly normalized: CheckedSubmittedTx;
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
