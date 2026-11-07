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
import { Effect, Either, Metric, Option } from "effect";

import { TxAdmissionsDB } from "../../database/index.js";
import {
  commitAdmissionBacklogSlot,
  releaseAdmissionBacklogSlot,
  reserveAdmissionBacklogSlot,
} from "../../fibers/index.js";
import {
  type AdmissionAck,
  AdmissionWriter,
  type AdmissionWriteRefusal,
  type AdmissionWriteRequest,
  Globals,
  MempoolLedgerCache,
  NodeConfig,
  UNKNOWN_SPENDABLE_VIEW,
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
 * The part of a checked transaction admission needs. Both the checked value
 * `checkSubmittedTransaction` returns and a peer-forwarded tx that passed
 * the same checks fit it; `txFullHash` is recomputed only when absent.
 */
export type AdmissionCandidate = {
  readonly txId: Buffer;
  readonly txIdHex: string;
  readonly txCanonicalCbor: Buffer;
  readonly txFullHash?: Buffer;
  /** The outref items (hex) the tx spends and produces: its input claims. */
  readonly spentOutRefs: readonly string[];
  readonly producedOutRefs: readonly string[];
};

/** Why admission refused a checked tx; nothing is held for it. */
export type AdmissionRefusal =
  | TxAdmissionsDB.TxAdmissionBacklogFullError
  | AdmissionWriteRefusal;

/**
 * Admits a checked transaction under `submitSource` (design D9): the batch
 * admission below, for one tx.
 */
export const admitCheckedTransaction = <R>({
  normalized,
  submitSource,
  wakeTxQueueProcessor,
}: {
  readonly normalized: AdmissionCandidate;
  readonly submitSource: "native" | "peer";
  readonly wakeTxQueueProcessor: Effect.Effect<void, never, R>;
}): Effect.Effect<
  AdmissionAck,
  AdmissionRefusal,
  AdmissionWriter | NodeConfig | Globals | R
> =>
  Effect.flatMap(
    admitCheckedTransactions({
      candidates: [normalized],
      submitSource,
      wakeTxQueueProcessor,
    }),
    ([result]) => result!,
  );

type BatchEntry =
  | { readonly _tag: "Answered"; readonly result: AdmissionResult }
  | {
      readonly _tag: "Reserved";
      readonly request: AdmissionWriteRequest;
      /** Gives back the backlog slot and any input claims this call took. */
      readonly release: Effect.Effect<void>;
    };

type AdmissionResult = Either.Either<AdmissionAck, AdmissionRefusal>;

/**
 * Admits checked transactions under `submitSource`, in order, in one writer
 * submission (design D9, D5 batch admit). It answers from memory and never
 * waits for Postgres; each tx gets its own answer, and one tx's refusal
 * stops no other:
 *
 * - a tx id the writer holds pending, or wrote recently, answers its
 *   duplicate ack (or a `TxAdmissionConflictError` when the bytes differ)
 *   with no backlog reservation and no database read;
 * - otherwise it reserves a backlog slot (a full backlog refuses with
 *   `TxAdmissionBacklogFullError`), and every reserved tx goes to the writer
 *   at once, which acknowledges each as `new` once it is in the pending set;
 * - in the same step each tx takes its input claims (design D6): a `native`
 *   tx spending an input another pending tx claimed, or one that is neither
 *   in the mempool ledger state nor produced by a pending tx, is refused
 *   with `AdmissionInputSpentError`. A `peer` tx is never refused for its
 *   inputs. The ledger view is read without the Phase B lock.
 *
 * A `new` ack means accepted, not stored: the row is written by the
 * writer's next group commit, which then wakes the tx-queue processor. A
 * crash before that commit loses the tx.
 */
export const admitCheckedTransactions = <R>({
  candidates,
  submitSource,
  wakeTxQueueProcessor,
}: {
  readonly candidates: readonly AdmissionCandidate[];
  readonly submitSource: "native" | "peer";
  readonly wakeTxQueueProcessor: Effect.Effect<void, never, R>;
}): Effect.Effect<
  readonly AdmissionResult[],
  never,
  AdmissionWriter | NodeConfig | Globals | R
> =>
  Effect.gen(function* () {
    const admissionWriter = yield* AdmissionWriter;
    const nodeConfig = yield* NodeConfig;
    const claims = (yield* Globals).ADMISSION_INPUT_CLAIMS;
    const cache = yield* Effect.serviceOption(MempoolLedgerCache);
    const context = yield* Effect.context<Globals | R>();
    const wakeOnCommit = wakeTxQueueProcessor.pipe(Effect.provide(context));
    // Reserving and registering run uninterruptibly, so a reserved slot is
    // always either owned by the writer or released here.
    return yield* Effect.uninterruptible(
      Effect.gen(function* () {
        const entries: BatchEntry[] = [];
        for (const normalized of candidates) {
          const txFullHash =
            normalized.txFullHash ??
            computeMidgardNativeTxFullHashFromCanonicalCbor(
              normalized.txCanonicalCbor,
            );
          const known = admissionWriter.lookup(normalized.txId, txFullHash);
          if (known._tag !== "Unknown") {
            entries.push({
              _tag: "Answered",
              result:
                known._tag === "Duplicate"
                  ? Either.right(known.ack)
                  : Either.left(known.error),
            });
            continue;
          }
          const reservation = yield* reserveAdmissionBacklogSlot({
            maxRows: nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG,
            maxBytes: nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG_BYTES,
            bytes: normalized.txCanonicalCbor.length,
          });
          if (!reservation.reserved) {
            yield* Metric.increment(
              TxAdmissionsDB.admissionBacklogRejectCounter,
            );
            entries.push({
              _tag: "Answered",
              result: Either.left(
                new TxAdmissionsDB.TxAdmissionBacklogFullError({
                  backlog: reservation.backlog,
                  maxBacklog: reservation.maxBacklog,
                  unit: reservation.unit,
                  message:
                    reservation.unit === "bytes"
                      ? "Durable submission admission byte backlog is full; retry later"
                      : "Durable submission admission backlog is full; retry later",
                }),
              ),
            });
            continue;
          }
          // Set when this call took the claims, so a write that stores no
          // new row gives them back; claims a pending copy holds stay with it.
          let claimed = false;
          const release = Effect.zipRight(
            Effect.sync(() => {
              if (claimed) claims.release(normalized.txIdHex);
              claimed = false;
            }),
            releaseAdmissionBacklogSlot(reservation),
          ).pipe(Effect.provide(context));
          entries.push({
            _tag: "Reserved",
            release,
            request: {
              txId: normalized.txId,
              txCanonicalCbor: normalized.txCanonicalCbor,
              txFullHash,
              submitSource,
              settle: (written) =>
                written
                  ? commitAdmissionBacklogSlot(reservation).pipe(
                      Effect.provide(context),
                    )
                  : release,
              wakeOnCommit,
              claimInputs: () => {
                const outcome = claims.take({
                  txIdHex: normalized.txIdHex,
                  spentOutRefs: normalized.spentOutRefs,
                  producedOutRefs: normalized.producedOutRefs,
                  check: submitSource === "native",
                  spendable,
                });
                if (outcome._tag === "Refused") return outcome.error;
                claimed = outcome._tag === "Claimed";
                return undefined;
              },
            },
          });
        }
        const reserved = entries.filter((entry) => entry._tag === "Reserved");
        // Read right before the writer's synchronous section, so the view
        // is as fresh as it can be when the claims are checked.
        const spendable =
          reserved.length > 0 && Option.isSome(cache)
            ? yield* cache.value.spendableView
            : UNKNOWN_SPENDABLE_VIEW;
        const written =
          reserved.length === 0
            ? []
            : yield* admissionWriter.admitBatch(
                reserved.map((entry) => entry.request),
              );
        let next = 0;
        return yield* Effect.forEach(entries, (entry) => {
          if (entry._tag === "Answered") return Effect.succeed(entry.result);
          const result = written[next++]!;
          // The writer owns the reservation only after answering `new`.
          return Either.isRight(result) && result.right.kind === "new"
            ? Effect.succeed<AdmissionResult>(result)
            : Effect.as(entry.release, result as AdmissionResult);
        });
      }),
    );
  });
