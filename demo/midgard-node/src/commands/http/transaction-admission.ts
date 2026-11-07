/**
 * The checks every transaction passes before durable admission, whoever sent
 * it: a user through `/submit` or another operator through `/peer/txs`. Both
 * routes run exactly these; the tx-queue processor then validates the
 * admitted bytes against this node's own ledger.
 */
import { LedgerSignatureVerifierUnavailableError } from "@al-ft/midgard-core";
import { computeMidgardNativeTxFullHashFromCanonicalCbor } from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_LIMITS } from "@al-ft/midgard-core/consensus-profile";
import { validateMidgardConsensusTx } from "@al-ft/midgard-core/consensus-validation";
import {
  midgardOutRefToCborHex,
  type RejectCode,
  RejectCodes,
  validatePhaseASingle,
} from "@al-ft/midgard-validation";
import type { SlotConfig } from "@lucid-evolution/lucid";
import { Effect, Either, Metric, Option } from "effect";

import { TxAdmissionsDB } from "../../database/index.js";
import {
  commitAdmissionBacklogSlot,
  releaseAdmissionBacklogSlot,
  reserveAdmissionBacklogSlot,
} from "../../fibers/index.js";
import { canonicalSlotConfigForLucid } from "../../lucid-time.js";
import { admissionBacklogLimitsFor } from "../../services/config.admission-backlog.js";
import {
  type AdmissionAck,
  AdmissionWriter,
  type AdmissionWriteRefusal,
  type AdmissionWriteRequest,
  Globals,
  Lucid,
  MempoolLedgerCache,
  NodeConfig,
  UNKNOWN_SPENDABLE_VIEW,
} from "../../services/index.js";
import { wallSlotAt } from "../../services/l1-ledger-tip.js";
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
      /** 503 only while the signature verifier cannot answer: retry. */
      readonly status: 400 | 413 | 503;
      readonly body: Readonly<Record<string, unknown>>;
      /** What the node logs about the refusal. */
      readonly log: readonly string[];
    };

/**
 * The node settings the stateless checks read (design D6). Plain data, so it
 * can cross a worker boundary with the bytes.
 */
export type SubmittedTxRules = {
  /** The body network id the validation pool expects. */
  readonly expectedNetworkId: bigint;
  readonly minFeeA: bigint;
  readonly minFeeB: bigint;
  /** The L1 slot mapping a tx's validity interval is read in. */
  readonly slotConfig: SlotConfig;
};

/**
 * How long past its validity end a tx must be, by this node's wall clock,
 * before ingress refuses it. It covers a node clock running ahead and the
 * tx-queue processor's L1 slot, which trails wall time by the age of the
 * last tip it saw: a tx inside the margin is admitted and Phase B rules on it.
 */
export const SUBMIT_EXPIRY_CLOCK_MARGIN_MS = 60_000;

/** The rules from this node's config and L1 slot mapping. */
export const submittedTxRules = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const lucid = yield* Lucid;
  return {
    // As the validation pool reads it (`makeValidationPool`).
    expectedNetworkId: nodeConfig.NETWORK === "Mainnet" ? 1n : 0n,
    minFeeA: nodeConfig.MIN_FEE_A,
    minFeeB: nodeConfig.MIN_FEE_B,
    slotConfig: canonicalSlotConfigForLucid(lucid.api),
  } satisfies SubmittedTxRules;
});

/**
 * The Phase A verdicts ingress refuses on (design D6): a forged or wrong
 * signature, a fee under the minimum and a foreign network. Phase A's other
 * verdicts are left to the validation pool, which reaches the same ones.
 */
const INGRESS_PHASE_A_REFUSALS: ReadonlySet<RejectCode> = new Set([
  RejectCodes.InvalidSignature,
  RejectCodes.MinFee,
  RejectCodes.NetworkIdMismatch,
]);

/**
 * Phase A on the submitted bytes with the validation pool's settings, then
 * expiry. Each refusal is one validation would make, with validation's code,
 * so ingress and validation cannot disagree (design D6). Undefined when the
 * tx passes.
 */
const statelessRefusal = (
  txId: Buffer,
  txCbor: Buffer,
  validityIntervalEnd: bigint | undefined,
  rules: SubmittedTxRules,
  nowMs: number,
): Extract<SubmittedTxCheck, { ok: false }> | undefined => {
  let verdict: ReturnType<typeof validatePhaseASingle>;
  try {
    verdict = validatePhaseASingle(
      { txId, txCbor, arrivalSeq: 0n, createdAt: new Date(nowMs) },
      {
        expectedNetworkId: rules.expectedNetworkId,
        minFeeA: rules.minFeeA,
        minFeeB: rules.minFeeB,
        concurrency: 1,
        // Phase A reads no strictness profile; the type asks for one.
        strictnessProfile: "phase1_midgard",
      },
    );
  } catch (cause) {
    if (!(cause instanceof LedgerSignatureVerifierUnavailableError))
      throw cause;
    // A local fault, not a verdict on the tx: the sender retries.
    return {
      ok: false,
      status: 503,
      body: { error: "Signature verification is unavailable" },
      log: [cause.message],
    };
  }
  if ("code" in verdict && INGRESS_PHASE_A_REFUSALS.has(verdict.code))
    return {
      ok: false,
      status: 400,
      body:
        verdict.detail === null
          ? { error: verdict.code }
          : { error: verdict.code, detail: verdict.detail },
      log: [],
    };
  // Phase B's expiry check, judged early on the wall clock less the margin.
  const pastSlot = wallSlotAt(
    rules.slotConfig,
    nowMs - SUBMIT_EXPIRY_CLOCK_MARGIN_MS,
  );
  if (
    validityIntervalEnd !== undefined &&
    BigInt(pastSlot) >= validityIntervalEnd
  )
    return {
      ok: false,
      status: 400,
      body: {
        error: RejectCodes.ValidityIntervalMismatch,
        detail: `${pastSlot.toString()} >= ${validityIntervalEnd.toString()}`,
      },
      log: [],
    };
  return undefined;
};

/**
 * Size, canonical form, native normalization, the consensus profile, then
 * the stateless validation checks (`statelessRefusal`). A pure function of
 * the bytes, the rules and the clock.
 */
export const checkSubmittedTransaction = (
  transactionBytes: Uint8Array,
  maxSubmitTxCborBytes: number,
  rules: SubmittedTxRules,
  nowMs: number,
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
  const refusal = statelessRefusal(
    ledgerTx.txId,
    txCbor,
    ledgerTx.validityIntervalEnd,
    rules,
    nowMs,
  );
  if (refusal !== undefined) return refusal;
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
    const limits = admissionBacklogLimitsFor(
      submitSource,
      nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG,
      nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG_BYTES,
    );
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
          // Load shedding (D5): an anonymous tx is refused from memory once
          // the backlog passes its share; a peer tx only once it is full.
          const reservation = yield* reserveAdmissionBacklogSlot({
            ...limits,
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
