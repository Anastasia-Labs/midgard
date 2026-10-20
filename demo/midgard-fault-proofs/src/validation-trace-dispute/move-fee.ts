/** The fee of a challenger's thread move. The thread pays it, up to the
 * deployment's per-move cap (the game and case validators let a thread pay
 * at most that), so a move pays its ledger minimum fee: the minimum of its
 * signed transaction, by the formula the funding permit checks a funded
 * transaction's fee with, over the current protocol parameters. A move whose
 * minimum fee would pass the cap cannot be paid by its thread; it is refused
 * with a typed alarm instead of a generic hold. */
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import {
  applySingleCborEncoding,
  CML,
  type LucidEvolution,
  type TxBuilder,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import { WorkflowActionChangedError } from "../workflow/action-changed.js";
import { workflowRuntimeFundingMinimumFee } from "../workflow/runtime-funding-policy.js";

/** The deployment's per-move fee cap: the most a thread pays for a move. */
export const VALIDATION_TRACE_MOVE_FEE_CAP_LOVELACE = BigInt(
  SELECTED_DEPLOYMENT_PROFILE.limits.max_dispute_move_fee_lovelace,
);

/** A move whose fee would pass the per-move cap: its thread cannot pay it. */
export const VALIDATION_TRACE_MOVE_FEE_OVER_CAP =
  "validation-trace-move-fee-over-cap";

/**
 * A move refused because its minimum fee passes the per-move cap. The run
 * resumes on the next observation and moves once the fee is back under the
 * cap; until then it raises the alarm named by its reason.
 */
export class ValidationTraceMoveFeeOverCapError extends WorkflowActionChangedError {
  constructor(
    readonly stage: string,
    readonly feeLovelace: bigint,
    readonly capLovelace: bigint,
  ) {
    super(
      `${VALIDATION_TRACE_MOVE_FEE_OVER_CAP}: the ${stage} move's minimum fee ${feeLovelace.toString()} passes the per-move cap ${capLovelace.toString()}`,
    );
    this.name = "ValidationTraceMoveFeeOverCapError";
  }
}

/** The ledger's reference-script fee tiers (Conway): 25,600 bytes, ×1.2. */
const REFERENCE_SCRIPT_TIER = Object.freeze({
  range: "25600",
  multiplier: Object.freeze({ numerator: "6", denominator: "5" }),
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

const complete = async (tx: TxBuilder, fee: bigint): Promise<TxSignBuilder> =>
  await tx
    .setMinFee(fee)
    .complete({ coinSelection: false, localUPLCEval: true });

/** How many times a move is rebuilt on its way down to its minimum fee. */
const FEE_ROUNDS = 8;

/**
 * Completes the `stage` move `build(fee)`, in which the thread pays `fee`
 * and the wallet only lends collateral, at the move's minimum fee. It is
 * built at the cap first, then rebuilt at the fee that build needs. The
 * builder may price a move a few lovelace above the ledger; a rebuild it
 * refuses is retried a little higher, and the cap's build stands when no
 * cheaper one does. A move that needs more than the cap cannot be built at
 * the cap; its fee is then measured on the same move paid from the wallet
 * (`build(0n)` with coin selection), which needs at least as much, and when
 * that passes the cap the move is refused with
 * `ValidationTraceMoveFeeOverCapError`.
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
    let needed: bigint;
    try {
      needed = needs(await build(0n).complete({ localUPLCEval: true }));
    } catch {
      throw cause;
    }
    if (needed > cap)
      throw new ValidationTraceMoveFeeOverCapError(stage, needed, cap);
    throw cause;
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
