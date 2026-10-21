/** The fee of a challenger's thread move, and who pays it. A move whose
 * ledger minimum fee fits under the deployment's per-move cap is paid by its
 * thread at that minimum: the validators let a thread that is its move's only
 * input (with its case, for a close) pay up to the cap. A move over the cap
 * is paid entirely from our plain-ADA wallet: its other inputs leave the
 * thread no allowance, so its successor keeps all of the thread's lovelace.
 * The minimum is that of its signed transaction, by the formula the funding
 * permit checks a funded transaction's fee with, over the current protocol
 * parameters. Only a wallet-paid move whose fee passes the funding permit's
 * per-transaction fee ceiling is refused, with a typed alarm. */
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import {
  applySingleCborEncoding,
  CML,
  coreToTxOutput,
  type LucidEvolution,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import {
  workflowRuntimeFundingMaximumFee,
  workflowRuntimeFundingMinimumFee,
} from "../workflow/runtime-funding-policy.js";

/** The deployment's per-move fee cap: the most a thread pays for a move. */
export const VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.limits.max_dispute_move_fee_lovelace,
);

/** A wallet-paid move whose fee would pass the funding permit's
 * per-transaction fee ceiling: no funding can pay it. */
export const VALIDATION_TRACE_MOVE_FEE_OVER_CAP =
  "validation-trace-move-fee-over-cap";

/**
 * A move refused because the fee our wallet would pay for it passes the
 * funding permit's per-transaction fee ceiling. The run resumes on the next
 * observation and moves once the fee is back under the ceiling; until then
 * it raises the alarm named by its reason.
 */
export class ValidationTraceMoveFeeOverCapError extends WorkflowActionChangedError {
  constructor(
    readonly stage: string,
    readonly feeLovelace: bigint,
    readonly ceilingLovelace: bigint,
  ) {
    super(
      `${VALIDATION_TRACE_MOVE_FEE_OVER_CAP}: the ${stage} move's fee ${feeLovelace.toString()} passes the funding permit's per-transaction fee ceiling ${ceilingLovelace.toString()}`,
    );
    this.name = "ValidationTraceMoveFeeOverCapError";
  }
}

/** The ledger's reference-script fee tiers (Conway): 25,600 bytes, ×1.2,
 * up to 204,800 bytes a transaction. */
const REFERENCE_SCRIPT_TIER = Object.freeze({
  range: "25600",
  multiplier: Object.freeze({ numerator: "6", denominator: "5" }),
  maximumSizeBytes: "204800",
});

/** A protocol price as lucid holds it, as the exact rational lucid uses. */
const price = (value: number) => ({
  numerator: BigInt(Math.round(value * 1e8)).toString(),
  denominator: "100000000",
});

/**
 * The ledger minimum fee of the move `transactionCborHex` as the prover's
 * key alone signs it (the one key a move needs: it lends the collateral),
 * reading the reference scripts of `referenced`, at `lucid`'s protocol
 * parameters.
 */
export const validationTraceMoveMinimumFee = ({
  lucid,
  transactionCborHex,
  referenced,
}: {
  readonly lucid: LucidEvolution;
  readonly transactionCborHex: string;
  readonly referenced: readonly UTxO[];
}): bigint => {
  const parameters = lucid.config().protocolParameters;
  if (parameters === undefined)
    throw new Error("validation-trace move has no protocol parameters");
  const transaction = CML.Transaction.from_cbor_hex(transactionCborHex);
  const body = transaction.body();
  const witnesses = transaction.witness_set();
  const keys = CML.VkeywitnessList.new();
  keys.add(
    CML.make_vkey_witness(
      CML.hash_transaction(body),
      CML.PrivateKey.generate_ed25519(),
    ),
  );
  witnesses.set_vkeywitnesses(keys);
  const signed = CML.Transaction.new(
    body,
    witnesses,
    true,
    transaction.auxiliary_data(),
  );
  let memory = 0n;
  let steps = 0n;
  const redeemers = witnesses.redeemers()?.to_flat_format();
  for (let index = 0; index < (redeemers?.len() ?? 0); index += 1) {
    const units = redeemers!.get(index).ex_units();
    memory += units.mem();
    steps += units.steps();
  }
  const referenceScriptBytes = referenced.reduce((sum, { scriptRef }) => {
    if (scriptRef == null) return sum;
    const cbor =
      scriptRef.type === "Native"
        ? scriptRef.script
        : applySingleCborEncoding(scriptRef.script);
    return sum + BigInt(cbor.length / 2);
  }, 0n);
  return workflowRuntimeFundingMinimumFee({
    parameters: {
      minFeeA: parameters.minFeeA.toString(),
      minFeeB: parameters.minFeeB.toString(),
      priceMemory: price(parameters.priceMem),
      priceSteps: price(parameters.priceStep),
      referenceScriptFee: {
        ...REFERENCE_SCRIPT_TIER,
        base: price(parameters.minFeeRefScriptCostPerByte),
      },
    },
    transactionBytes: BigInt(signed.to_cbor_bytes().length),
    memory,
    steps,
    referenceScriptBytes,
  });
};

/** The funding permit's per-transaction fee ceiling at `lucid`'s protocol
 * parameters, by the rule the permit derives it with. */
export const validationTraceMoveFeeCeiling = (
  lucid: LucidEvolution,
): bigint => {
  const parameters = lucid.config().protocolParameters;
  if (parameters === undefined)
    throw new Error("validation-trace move has no protocol parameters");
  return workflowRuntimeFundingMaximumFee({
    minFeeA: parameters.minFeeA.toString(),
    minFeeB: parameters.minFeeB.toString(),
    priceMemory: price(parameters.priceMem),
    priceSteps: price(parameters.priceStep),
    maxTxSize: parameters.maxTxSize.toString(),
    maxTxExUnits: {
      memory: parameters.maxTxExMem.toString(),
      steps: parameters.maxTxExSteps.toString(),
    },
    referenceScriptFee: {
      ...REFERENCE_SCRIPT_TIER,
      base: price(parameters.minFeeRefScriptCostPerByte),
    },
  });
};

const complete = async (tx: TxBuilder, fee: bigint): Promise<TxSignBuilder> =>
  await tx
    .setMinFee(fee)
    .complete({ coinSelection: false, localUPLCEval: true });

/**
 * What the wallet's funding adds to the minimum fee of `paid`, a move the
 * wallet paid in full: the bytes of its inputs from the wallet and of the
 * change returned to it (the last output, at the wallet's address), at the
 * per-byte fee. Without them `paid` is about the move its thread pays.
 */
const walletFundingFee = async (
  lucid: LucidEvolution,
  paid: TxSignBuilder,
): Promise<bigint> => {
  const wallet = lucid.wallet();
  const own = new Set(
    (await wallet.getUtxos()).map(
      ({ txHash, outputIndex }) => `${txHash}#${outputIndex.toString()}`,
    ),
  );
  const body = CML.Transaction.from_cbor_hex(paid.toCBOR()).body();
  let bytes = 0;
  const inputs = body.inputs();
  for (let index = 0; index < inputs.len(); index += 1) {
    const input = inputs.get(index);
    if (
      own.has(`${input.transaction_id().to_hex()}#${input.index().toString()}`)
    )
      bytes += input.to_cbor_bytes().length;
  }
  const outputs = body.outputs();
  const change = outputs.get(outputs.len() - 1);
  if (coreToTxOutput(change).address === (await wallet.address()))
    bytes += change.to_cbor_bytes().length;
  return BigInt(lucid.config().protocolParameters!.minFeeA) * BigInt(bytes);
};

/** How many times a move is rebuilt on its way down to its minimum fee. */
const FEE_ROUNDS = 8;

/**
 * Completes the `stage` move `build(fee)`, in which the thread pays `fee`,
 * deciding who pays. The thread pays when the move's minimum fee fits
 * under the cap: it is built at the cap first, with the wallet only lending
 * collateral, then rebuilt at the fee that build needs. The builder may price
 * a move a few lovelace above the ledger; a rebuild it refuses is retried a
 * little higher, and the cap's build stands when no cheaper one does. A move
 * that needs more than the cap cannot be built at the cap. When the cap build
 * fails, the move is built with the thread paying nothing and our wallet's
 * coin selection paying the whole fee (`build(0n)`), and its fee measured
 * less what the wallet's funding adds to it. When that passes the cap the
 * wallet-paid move is the move, unless its fee passes the funding permit's
 * per-transaction fee ceiling, which refuses it with
 * `ValidationTraceMoveFeeOverCapError`. Otherwise the cap build failed for
 * another reason and keeps its own error.
 */
export const completeValidationTraceMove = async ({
  lucid,
  stage,
  build,
  referenced,
}: {
  readonly lucid: LucidEvolution;
  readonly stage: string;
  readonly build: (fee: bigint) => TxBuilder;
  readonly referenced: readonly UTxO[];
}): Promise<TxSignBuilder> => {
  const cap = VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE;
  const needs = (unsigned: TxSignBuilder) =>
    validationTraceMoveMinimumFee({
      lucid,
      transactionCborHex: unsigned.toCBOR(),
      referenced,
    });
  let best: TxSignBuilder;
  try {
    best = await complete(build(cap), cap);
  } catch (cause) {
    let paid: TxSignBuilder;
    let needed: bigint;
    try {
      paid = await build(0n).complete({ localUPLCEval: true });
      needed = needs(paid) - (await walletFundingFee(lucid, paid));
    } catch {
      throw cause;
    }
    if (needed <= cap) throw cause;
    const fee = CML.Transaction.from_cbor_hex(paid.toCBOR()).body().fee();
    const ceiling = validationTraceMoveFeeCeiling(lucid);
    if (fee > ceiling)
      throw new ValidationTraceMoveFeeOverCapError(stage, fee, ceiling);
    return paid;
  }
  // A build that completes at the cap needs no more than the cap: the
  // builder prices a move at or above its ledger minimum.
  const needed = needs(best);
  let fee = needed;
  let step = 0n;
  for (let round = 0; round < FEE_ROUNDS && fee < cap; round += 1) {
    let candidate: TxSignBuilder;
    try {
      candidate = await complete(build(fee), fee);
    } catch {
      step = step === 0n ? 16n : step * 4n;
      fee = needed + step;
      continue;
    }
    const again = needs(candidate);
    if (again <= fee) return candidate;
    fee = again;
  }
  return best;
};
