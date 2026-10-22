/** The funding permit's fee rule: a transaction's minimum fee at the
 * signed protocol's exact prices, the per-transaction fee ceiling, and the
 * reserve a validation dispute keeps for a thread move our wallet pays. */
import type { DeploymentManifestCardanoProtocolParameters } from "@al-ft/midgard-core/deployment-manifest-identity";
import { CML } from "@lucid-evolution/lucid";

type Rational = Readonly<{ numerator: bigint; denominator: bigint }>;
const rational = (value: {
  numerator: string;
  denominator: string;
}): Rational => ({
  numerator: BigInt(value.numerator),
  denominator: BigInt(value.denominator),
});
const add = (left: Rational, right: Rational): Rational => ({
  numerator:
    left.numerator * right.denominator + right.numerator * left.denominator,
  denominator: left.denominator * right.denominator,
});
const multiply = (left: Rational, right: Rational): Rational => ({
  numerator: left.numerator * right.numerator,
  denominator: left.denominator * right.denominator,
});
const scale = (value: Rational, quantity: bigint): Rational => ({
  numerator: value.numerator * quantity,
  denominator: value.denominator,
});
const ceil = (value: Rational): bigint =>
  (value.numerator + value.denominator - 1n) / value.denominator;

/** Uses the signed protocol's exact rational prices, including reference tiers. */
export const workflowRuntimeFundingMinimumFee = ({
  parameters,
  transactionBytes,
  memory,
  steps,
  referenceScriptBytes,
}: {
  parameters: Pick<
    DeploymentManifestCardanoProtocolParameters,
    "minFeeA" | "minFeeB" | "priceMemory" | "priceSteps"
  > & {
    referenceScriptFee: Pick<
      DeploymentManifestCardanoProtocolParameters["referenceScriptFee"],
      "base" | "range" | "multiplier"
    >;
  };
  transactionBytes: bigint;
  memory: bigint;
  steps: bigint;
  referenceScriptBytes: bigint;
}): bigint => {
  let remaining = referenceScriptBytes;
  let price = rational(parameters.referenceScriptFee.base);
  let referenceFee: Rational = { numerator: 0n, denominator: 1n };
  const range = BigInt(parameters.referenceScriptFee.range);
  while (remaining > 0n) {
    const bytes = remaining > range ? range : remaining;
    referenceFee = add(referenceFee, scale(price, bytes));
    remaining -= bytes;
    price = multiply(price, rational(parameters.referenceScriptFee.multiplier));
  }
  return (
    BigInt(parameters.minFeeA) * transactionBytes +
    BigInt(parameters.minFeeB) +
    ceil(
      add(
        scale(rational(parameters.priceMemory), memory),
        scale(rational(parameters.priceSteps), steps),
      ),
    ) +
    ceil(referenceFee)
  );
};

/**
 * The funding permit's per-transaction fee ceiling: the minimum fee of a
 * transaction at the protocol's maximum signed size, execution units and
 * reference-script bytes. The permit refuses a funded transaction whose fee
 * passes it.
 */
export const workflowRuntimeFundingMaximumFee = (
  parameters: Pick<
    DeploymentManifestCardanoProtocolParameters,
    "minFeeA" | "minFeeB" | "priceMemory" | "priceSteps" | "maxTxSize"
  > & {
    maxTxExUnits: DeploymentManifestCardanoProtocolParameters["maxTxExUnits"];
    referenceScriptFee: Pick<
      DeploymentManifestCardanoProtocolParameters["referenceScriptFee"],
      "base" | "range" | "multiplier" | "maximumSizeBytes"
    >;
  },
): bigint =>
  workflowRuntimeFundingMinimumFee({
    parameters,
    transactionBytes: BigInt(parameters.maxTxSize),
    memory: BigInt(parameters.maxTxExUnits.memory),
    steps: BigInt(parameters.maxTxExUnits.steps),
    referenceScriptBytes: BigInt(
      parameters.referenceScriptFee.maximumSizeBytes,
    ),
  });

/**
 * The reference-script bytes of the largest thread move of a validation-trace
 * dispute measured so far: a script-sources output proof finalize's hop
 * that attaches two descriptor yields
 * (tests/validation-trace-dispute-one-step-script-sources-68.test.ts).
 * Whether a move's thread or our wallet pays for it turns on the live
 * prices, so this bounds every measured move, not only those over the
 * per-move cap at today's prices.
 */
export const VALIDATION_DISPUTE_WALLET_MOVE = Object.freeze({
  referenceScriptBytes: 34_659n,
});

/**
 * What a reservation must hold for one move our wallet pays, under
 * `parameters`: the fee of a move of the protocol's largest signed size, our
 * wallet's inputs and change included, at the protocol's per-transaction
 * execution limit, reading the largest measured move's reference scripts,
 * and the minimum ada of its change, a plain coin returned to the funding
 * key. The builder selects our wallet's coins with the move's scripts
 * budgeted at that limit before it evaluates them, so a reserve priced at
 * the move's own execution units would not let it select. The funding
 * permit keeps it when the case opens; a later move our wallet cannot fund
 * does not build, and raises the operator's alarm.
 */
export const validationDisputeWalletMoveReserve = ({
  parameters,
  fundingPaymentKeyHash,
}: {
  readonly parameters: DeploymentManifestCardanoProtocolParameters;
  readonly fundingPaymentKeyHash: string;
}): bigint => {
  const fee = workflowRuntimeFundingMinimumFee({
    parameters,
    transactionBytes: BigInt(parameters.maxTxSize),
    memory: BigInt(parameters.maxTxExUnits.memory),
    steps: BigInt(parameters.maxTxExUnits.steps),
    referenceScriptBytes: VALIDATION_DISPUTE_WALLET_MOVE.referenceScriptBytes,
  });
  // The largest plain change: a base address, the largest coin.
  const key = CML.Credential.new_pub_key(
    CML.Ed25519KeyHash.from_hex(fundingPaymentKeyHash),
  );
  const change = CML.TransactionOutput.new(
    CML.BaseAddress.new(0, key, key).to_address(),
    CML.Value.from_coin(0xffff_ffff_ffff_ffffn),
  );
  return (
    fee + CML.min_ada_required(change, BigInt(parameters.coinsPerUtxoByte))
  );
};
