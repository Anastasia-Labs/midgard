/**
 * Exact-fee completion for the operator-exit endpoints whose validators read
 * the transaction fee as a protocol payment.
 *
 * `SlashDuplicateOperator` requires `fee == slashing_penalty` and the forced
 * (inactivity) retirement requires `fee == inactivity_slashing_penalty`: both
 * are equalities, so a transaction whose fee Lucid raised on its own is
 * refused on chain rather than merely being expensive.
 *
 * Lucid cannot hit an exact fee while it still has to add a change output:
 * `add_change_if_needed` charges the change output's own marginal fee on top
 * of the pinned `setMinFee` value, and that surplus depends on the change
 * output's size. The only construction that lands on the fee exactly is one
 * where nothing is left over — the transaction pays every lovelace it does not
 * burn as the fee to an output the builder itself declares.
 *
 * So these builders balance themselves: this module computes what the
 * transaction's declared outputs really cost (Lucid tops every output up to
 * its minimum-ADA floor, which is why the floor is computed here with the same
 * CML helper Lucid uses), picks plain-ADA wallet UTxOs to cover any shortfall
 * and to back the collateral, and returns the single remainder output that
 * makes inputs, outputs and the pinned fee add up.
 */
import {
  type Assets,
  calculateMinLovelaceFromUTxO,
  type LucidEvolution,
  type TxSignBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import type { TxCompleteOptions } from "../tx-completion.js";

/** An output the builder declares itself, before Lucid's min-ADA top-up. */
export type ExactFeeOutputPlan = {
  readonly address: string;
  readonly assets: Assets;
  /** Inline datum CBOR, when the output carries one. */
  readonly datumCbor?: string;
};

export type ExactFeeBalancePlan = {
  readonly feeLovelace: bigint;
  /** Plain-ADA wallet UTxOs the transaction must spend to balance. */
  readonly fundingInputs: readonly UTxO[];
  /** Plain-ADA wallet UTxOs offered to Lucid as collateral candidates. */
  readonly collateralInputs: readonly UTxO[];
  readonly totalCollateralLovelace: bigint;
  /** `0n` when the declared outputs and the fee already consume every input. */
  readonly remainderLovelace: bigint;
  readonly remainderAddress: string;
  /** Declared outputs plus the remainder output, if any. */
  readonly expectedOutputCount: number;
};

const lovelaceOf = (assets: Assets): bigint => assets["lovelace"] ?? 0n;

const totalLovelace = (utxos: readonly UTxO[]): bigint =>
  utxos.reduce((sum, utxo) => sum + lovelaceOf(utxo.assets), 0n);

/**
 * Whether the UTxO is a plain wallet coin: lovelace only, no datum, no
 * reference script. Anything else is somebody's state, not funding.
 */
const isPlainAdaUtxo = (utxo: UTxO): boolean =>
  Object.keys(utxo.assets).every((unit) => unit === "lovelace") &&
  lovelaceOf(utxo.assets) > 0n &&
  utxo.datum == null &&
  utxo.datumHash == null &&
  utxo.scriptRef == null;

/** Lucid's collateral selector refuses to attach more inputs than this. */
const MAX_COLLATERAL_INPUTS = 3;

const requireProtocolParameters = (lucid: LucidEvolution, label: string) => {
  const parameters = lucid.config().protocolParameters;
  if (parameters === undefined) {
    throw new Error(`${label} needs live protocol parameters`);
  }
  return parameters;
};

/**
 * The minimum lovelace Lucid will silently raise this output to. Same CML
 * computation `pay.ToContract` / `pay.ToAddress` apply when they build it.
 */
const minOutputLovelace = (
  lucid: LucidEvolution,
  output: ExactFeeOutputPlan,
  label: string,
): bigint =>
  calculateMinLovelaceFromUTxO(
    requireProtocolParameters(lucid, label).coinsPerUtxoByte,
    {
      txHash: "00".repeat(32),
      outputIndex: 0,
      address: output.address,
      assets: output.assets,
      ...(output.datumCbor === undefined ? {} : { datum: output.datumCbor }),
    },
  );

/** What the output actually costs once Lucid has applied the min-ADA floor. */
const settledOutputLovelace = (
  lucid: LucidEvolution,
  output: ExactFeeOutputPlan,
  label: string,
): bigint => {
  const floor = minOutputLovelace(lucid, output, label);
  const declared = lovelaceOf(output.assets);
  return declared > floor ? declared : floor;
};

/**
 * The same output with its min-ADA floor declared explicitly.
 *
 * A prediction of Lucid's own floor is not enough: Lucid computes it from the
 * *canonical* encoding of the inline datum, which can be a few bytes shorter
 * than the datum CBOR the builder hands it, so the floor computed here can
 * exceed the one Lucid applies. The difference would be left over, and a
 * leftover Lucid cannot turn into a change output is added to the fee —
 * breaking the exact-fee equality the validator checks. Paying the settled
 * amount outright removes the guess: `addOutput` keeps a declared value that
 * is at least its own floor, so the transaction pays exactly what the balance
 * reserved.
 */
export const withSettledLovelace = <T extends ExactFeeOutputPlan>(
  lucid: LucidEvolution,
  output: T,
  label: string,
): T => ({
  ...output,
  assets: {
    ...output.assets,
    lovelace: settledOutputLovelace(lucid, output, label),
  },
});

/**
 * Balances an exact-fee transaction.
 *
 * `scriptInputs` are the inputs the endpoint must spend anyway (the list
 * elements it removes and updates); `declaredOutputs` are the outputs the
 * builder pays before the remainder. Funding and collateral both come from the
 * plain-ADA coins in `walletUtxos`, largest first, and one coin may serve as
 * both: the ledger checks collateral against the pre-transaction UTxO set and
 * only takes it when phase-2 validation fails, so a wallet holding a single
 * large coin can still submit.
 */
export const planExactFeeBalance = ({
  lucid,
  label,
  feeLovelace,
  scriptInputs,
  declaredOutputs,
  walletUtxos,
  remainderAddress,
}: {
  readonly lucid: LucidEvolution;
  readonly label: string;
  readonly feeLovelace: bigint;
  readonly scriptInputs: readonly UTxO[];
  readonly declaredOutputs: readonly ExactFeeOutputPlan[];
  readonly walletUtxos: readonly UTxO[];
  readonly remainderAddress: string;
}): ExactFeeBalancePlan => {
  const parameters = requireProtocolParameters(lucid, label);
  const outputsLovelace = declaredOutputs.reduce(
    (sum, output) => sum + settledOutputLovelace(lucid, output, label),
    0n,
  );
  const collateralPercentage = BigInt(parameters.collateralPercentage);
  const totalCollateralLovelace =
    (feeLovelace * collateralPercentage + 99n) / 100n;
  const plain = walletUtxos
    .filter(isPlainAdaUtxo)
    .sort((a, b) => (lovelaceOf(b.assets) > lovelaceOf(a.assets) ? 1 : -1));

  const collateralInputs: UTxO[] = [];
  while (
    totalLovelace(collateralInputs) < totalCollateralLovelace &&
    collateralInputs.length < plain.length
  ) {
    collateralInputs.push(plain[collateralInputs.length]!);
  }
  if (
    totalLovelace(collateralInputs) < totalCollateralLovelace ||
    collateralInputs.length > MAX_COLLATERAL_INPUTS
  ) {
    throw new Error(
      `${label} needs ${totalCollateralLovelace.toString()} lovelace of plain-ADA wallet collateral (${collateralPercentage.toString()}% of the pinned fee) in at most ${MAX_COLLATERAL_INPUTS.toString()} coins; the ${Math.min(collateralInputs.length, MAX_COLLATERAL_INPUTS).toString()} largest hold ${totalLovelace(collateralInputs.slice(0, MAX_COLLATERAL_INPUTS)).toString()}`,
    );
  }

  const remainderFits = (remainder: bigint): boolean =>
    remainder === 0n ||
    (remainder > 0n &&
      remainder >=
        minOutputLovelace(
          lucid,
          { address: remainderAddress, assets: { lovelace: remainder } },
          label,
        ));
  const fundingInputs: UTxO[] = [];
  let remainder = totalLovelace(scriptInputs) - outputsLovelace - feeLovelace;
  while (!remainderFits(remainder) && fundingInputs.length < plain.length) {
    const next = plain[fundingInputs.length]!;
    fundingInputs.push(next);
    remainder += lovelaceOf(next.assets);
  }
  if (!remainderFits(remainder)) {
    throw new Error(
      `${label} cannot balance a pinned fee of ${feeLovelace.toString()} lovelace: ${remainder < 0n ? `short by ${(-remainder).toString()} lovelace` : `the ${remainder.toString()} lovelace left over is below the minimum output`}, and no further plain-ADA wallet input is available`,
    );
  }

  return {
    feeLovelace,
    fundingInputs,
    collateralInputs,
    totalCollateralLovelace,
    remainderLovelace: remainder,
    remainderAddress,
    expectedOutputCount: declaredOutputs.length + (remainder === 0n ? 0 : 1),
  };
};

/**
 * Completion options for an exact-fee build: coin selection off (Lucid must
 * not add inputs the balance does not know about) and the collateral taken
 * from the plan's reserved coins.
 */
export const exactFeeCompleteOptions = (
  plan: ExactFeeBalancePlan,
): TxCompleteOptions => ({
  localUPLCEval: true,
  coinSelection: false,
  presetWalletInputs: [...plan.collateralInputs],
  setCollateral: plan.totalCollateralLovelace,
});

/**
 * Verifies the completed transaction still pays exactly the pinned fee and
 * that Lucid added no output of its own. Returns an error message, or `null`
 * when the transaction is as planned.
 */
export const exactFeeViolation = (
  tx: TxSignBuilder,
  plan: ExactFeeBalancePlan,
): string | null => {
  const body = tx.toTransaction().body();
  const fee = body.fee();
  if (fee !== plan.feeLovelace) {
    return `fee=${fee.toString()},expected=${plan.feeLovelace.toString()}`;
  }
  const outputs = body.outputs().len();
  if (outputs !== plan.expectedOutputCount) {
    return `outputs=${outputs.toString()},expected=${plan.expectedOutputCount.toString()}`;
  }
  return null;
};
