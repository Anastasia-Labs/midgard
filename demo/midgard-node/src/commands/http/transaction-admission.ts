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
import { Effect } from "effect";

import { canonicalSlotConfigForLucid } from "../../lucid-time.js";
import { Lucid, NodeConfig } from "../../services/index.js";
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
