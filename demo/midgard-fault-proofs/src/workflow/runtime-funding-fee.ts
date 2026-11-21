/** The workflow's fee rule: a transaction's minimum fee at the signed
 * protocol's exact prices, and the per-transaction fee ceiling. */
import type { DeploymentManifestCardanoProtocolParameters } from "@al-ft/midgard-core/deployment-manifest-identity";

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
 * The per-transaction fee ceiling: the minimum fee of a transaction at the
 * protocol's maximum signed size, execution units and reference-script
 * bytes. A wallet-paid dispute move whose fee passes it is refused
 * (`ValidationTraceMoveFeeOverCapError`).
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
