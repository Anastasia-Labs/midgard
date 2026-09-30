import { type WorkflowFundingScope } from "@al-ft/midgard-fault-proofs";

export const WATCHER_PROVER_FUNDING_CALCULATION =
  "midgard-watcher-production-prover-funding-calculation-v1" as const;

export type WatcherProverFundingActionCalculation = Readonly<{
  actionKind: string;
  transactionFeeLovelace: string;
  minimumProtocolFeeLovelace: string;
  linearFeeLovelace: string;
  executionFeeLovelace: string;
  referenceScriptFeeLovelace: string;
  outputMinAdaLovelace: string;
  requiredBondLovelace: string;
  requiredRewardCustodyLovelace: string;
  collateralLovelace: string;
  collateralInputCount: string;
  collateralReturnLovelace: string | null;
  ordinaryInputCount: string;
  fundingControlledInputCount: string;
  walletFundingInputCount: string;
  walletChangeLovelace: string;
  lockedCapitalLovelace: string;
  releasedCapitalLovelace: string;
  lockedNativeAssets: readonly Readonly<{ unit: string; quantity: string }>[];
  releasedNativeAssets: readonly Readonly<{ unit: string; quantity: string }>[];
  attemptCount: string;
  feeHeadroomLovelace: string;
}>;

export type WatcherProverFundingCalculation = Readonly<{
  schemaVersion: typeof WATCHER_PROVER_FUNDING_CALCULATION;
  scope: WorkflowFundingScope;
  deploymentFingerprint: string;
  profileDigest: string;
  protocolParametersDigest: string;
  economicsPolicyDigest: string;
  fundingPaymentKeyHash: string;
  actions: readonly WatcherProverFundingActionCalculation[];
  totals: Readonly<{
    feeHeadroomLovelace: string;
    outputMinAdaLovelace: string;
    requiredBondLovelace: string;
    requiredRewardCustodyLovelace: string;
    reusableCollateralLovelace: string;
    peakCapitalLovelace: string;
    endingCapitalLovelace: string;
    requiredLovelace: string;
    requiredNativeAssets: readonly Readonly<{
      unit: string;
      quantity: string;
    }>[];
    maximumCollateralInputs: string;
    maximumOrdinaryInputs: string;
    maximumFundingInputs: string;
  }>;
  calculationDigest: string;
}>;

export const admittedCalculations = new WeakSet<object>();

export const assertWatcherProverFundingCalculation = (
  calculation: WatcherProverFundingCalculation,
): void => {
  if (!admittedCalculations.has(calculation)) {
    throw new Error("prover funding calculation is not admitted");
  }
};

type Rational = Readonly<{ numerator: bigint; denominator: bigint }>;

export const rational = (value: {
  readonly numerator: string;
  readonly denominator: string;
}): Rational => ({
  numerator: BigInt(value.numerator),
  denominator: BigInt(value.denominator),
});

export const add = (left: Rational, right: Rational): Rational => ({
  numerator:
    left.numerator * right.denominator + right.numerator * left.denominator,
  denominator: left.denominator * right.denominator,
});

const multiply = (left: Rational, right: Rational): Rational => ({
  numerator: left.numerator * right.numerator,
  denominator: left.denominator * right.denominator,
});

export const multiplyNatural = (
  value: Rational,
  natural: bigint,
): Rational => ({
  numerator: value.numerator * natural,
  denominator: value.denominator,
});

export const ceil = (value: Rational): bigint =>
  (value.numerator + value.denominator - 1n) / value.denominator;

export const tieredReferenceScriptFee = (input: {
  readonly bytes: bigint;
  readonly base: Rational;
  readonly range: bigint;
  readonly multiplier: Rational;
}): bigint => {
  let remaining = input.bytes;
  let price = input.base;
  let total: Rational = { numerator: 0n, denominator: 1n };
  while (remaining > 0n) {
    const tierBytes = remaining < input.range ? remaining : input.range;
    total = add(total, multiplyNatural(price, tierBytes));
    remaining -= tierBytes;
    price = multiply(price, input.multiplier);
  }
  return ceil(total);
};

export const ceilPercentage = (value: bigint, percentage: bigint): bigint =>
  (value * percentage + 99n) / 100n;

export const sum = (values: readonly bigint[]): bigint =>
  values.reduce((total, value) => total + value, 0n);

type AssetQuantity = Readonly<{ unit: string; quantity: string }>;

export const assetEntries = (
  values: ReadonlyMap<string, bigint>,
): readonly AssetQuantity[] =>
  Object.freeze(
    [...values]
      .filter(([, quantity]) => quantity > 0n)
      .sort(([left], [right]) => left.localeCompare(right))
      .map(([unit, quantity]) =>
        Object.freeze({ unit, quantity: quantity.toString() }),
      ),
  );

const applyAssetDelta = (
  current: Map<string, bigint>,
  added: readonly AssetQuantity[],
  released: readonly AssetQuantity[],
): void => {
  for (const { unit, quantity } of added) {
    current.set(unit, (current.get(unit) ?? 0n) + BigInt(quantity));
  }
  for (const { unit, quantity } of released) {
    const next = (current.get(unit) ?? 0n) - BigInt(quantity);
    if (next < 0n) {
      throw new Error(
        "prover funding releases more native capital than was locked",
      );
    }
    current.set(unit, next);
  }
};

export const capitalFlow = (
  actions: readonly WatcherProverFundingActionCalculation[],
): Readonly<{
  peakLovelace: bigint;
  endingLovelace: bigint;
  peakNativeAssets: readonly AssetQuantity[];
}> => {
  let currentLovelace = 0n;
  let peakLovelace = 0n;
  const currentAssets = new Map<string, bigint>();
  const peakAssets = new Map<string, bigint>();
  for (const action of actions) {
    currentLovelace +=
      BigInt(action.feeHeadroomLovelace) +
      BigInt(action.lockedCapitalLovelace) -
      BigInt(action.releasedCapitalLovelace);
    if (currentLovelace < 0n) {
      throw new Error("prover funding releases more capital than was locked");
    }
    if (currentLovelace > peakLovelace) peakLovelace = currentLovelace;
    applyAssetDelta(
      currentAssets,
      action.lockedNativeAssets,
      action.releasedNativeAssets,
    );
    for (const [unit, quantity] of currentAssets) {
      if (quantity > (peakAssets.get(unit) ?? 0n)) {
        peakAssets.set(unit, quantity);
      }
    }
  }
  return Object.freeze({
    peakLovelace,
    endingLovelace: currentLovelace,
    peakNativeAssets: assetEntries(peakAssets),
  });
};
