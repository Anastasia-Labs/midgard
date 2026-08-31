import { computeDeploymentManifestV1JsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity-v1";
import {
  assertAdmittedProductionWorkflowFundingRequirementsV1,
  type ProductionWorkflowFundingRequirementsV1,
  type ProductionWorkflowFundingScopeV1,
} from "@al-ft/midgard-fault-proofs";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";
import { CML } from "@lucid-evolution/lucid";

import {
  assertVerifiedWatcherDeploymentIdentityV1,
  type VerifiedWatcherDeploymentIdentityV1,
  watcherDeploymentReleaseEconomicsAuthorityV1,
} from "./deployment-identity.js";
import {
  assertWatcherProductionProtocolParameterRuntimeAuthorityV1,
  type WatcherProductionProtocolParameterRuntimeAuthorityV1,
} from "./production-prover-funding-v1.js";

export const WATCHER_PRODUCTION_PROVER_FUNDING_CALCULATION_V1 =
  "midgard-watcher-production-prover-funding-calculation-v1" as const;

export type WatcherProductionProverFundingActionCalculationV1 = Readonly<{
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

export type WatcherProductionProverFundingCalculationV1 = Readonly<{
  schemaVersion: typeof WATCHER_PRODUCTION_PROVER_FUNDING_CALCULATION_V1;
  scope: ProductionWorkflowFundingScopeV1;
  deploymentFingerprint: string;
  profileDigest: string;
  protocolParametersDigest: string;
  economicsPolicyDigest: string;
  fundingPaymentKeyHash: string;
  actions: readonly WatcherProductionProverFundingActionCalculationV1[];
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

const admittedCalculations = new WeakSet<object>();

export const assertWatcherProductionProverFundingCalculationV1 = (
  calculation: WatcherProductionProverFundingCalculationV1,
): void => {
  if (!admittedCalculations.has(calculation)) {
    throw new Error("prover funding calculation is not admitted");
  }
};

type RationalV1 = Readonly<{ numerator: bigint; denominator: bigint }>;

const rational = (value: {
  readonly numerator: string;
  readonly denominator: string;
}): RationalV1 => ({
  numerator: BigInt(value.numerator),
  denominator: BigInt(value.denominator),
});

const add = (left: RationalV1, right: RationalV1): RationalV1 => ({
  numerator:
    left.numerator * right.denominator + right.numerator * left.denominator,
  denominator: left.denominator * right.denominator,
});

const multiply = (left: RationalV1, right: RationalV1): RationalV1 => ({
  numerator: left.numerator * right.numerator,
  denominator: left.denominator * right.denominator,
});

const multiplyNatural = (value: RationalV1, natural: bigint): RationalV1 => ({
  numerator: value.numerator * natural,
  denominator: value.denominator,
});

const ceil = (value: RationalV1): bigint =>
  (value.numerator + value.denominator - 1n) / value.denominator;

const tieredReferenceScriptFee = (input: {
  readonly bytes: bigint;
  readonly base: RationalV1;
  readonly range: bigint;
  readonly multiplier: RationalV1;
}): bigint => {
  let remaining = input.bytes;
  let price = input.base;
  let total: RationalV1 = { numerator: 0n, denominator: 1n };
  while (remaining > 0n) {
    const tierBytes = remaining < input.range ? remaining : input.range;
    total = add(total, multiplyNatural(price, tierBytes));
    remaining -= tierBytes;
    price = multiply(price, input.multiplier);
  }
  return ceil(total);
};

const ceilPercentage = (value: bigint, percentage: bigint): bigint =>
  (value * percentage + 99n) / 100n;

const sum = (values: readonly bigint[]): bigint =>
  values.reduce((total, value) => total + value, 0n);

type AssetQuantityV1 = Readonly<{ unit: string; quantity: string }>;

const assetEntries = (
  values: ReadonlyMap<string, bigint>,
): readonly AssetQuantityV1[] =>
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
  added: readonly AssetQuantityV1[],
  released: readonly AssetQuantityV1[],
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

const capitalFlow = (
  actions: readonly WatcherProductionProverFundingActionCalculationV1[],
): Readonly<{
  peakLovelace: bigint;
  endingLovelace: bigint;
  peakNativeAssets: readonly AssetQuantityV1[];
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

/**
 * Calculates the release-authenticated maximum-shape funding requirement.
 * The measured profile must have been bound by a fixed admitted workflow
 * factory; a digest-valid caller-authored profile is insufficient.
 */
export const calculateWatcherProductionProverFundingV1 = async (input: {
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentityV1;
  readonly protocolParameters: WatcherProductionProtocolParameterRuntimeAuthorityV1;
  readonly requirements: ProductionWorkflowFundingRequirementsV1;
}): Promise<WatcherProductionProverFundingCalculationV1> => {
  assertVerifiedWatcherDeploymentIdentityV1(input.deploymentIdentity);
  assertWatcherProductionProtocolParameterRuntimeAuthorityV1(
    input.protocolParameters,
  );
  assertAdmittedProductionWorkflowFundingRequirementsV1(input.requirements);
  if (input.requirements.scope.kind === "da_availability_lifecycle") {
    throw new Error(
      "DA availability funding requires signed availability-challenge authority",
    );
  }
  if (
    input.protocolParameters.deploymentFingerprint !==
      input.deploymentIdentity.manifestId ||
    input.requirements.deploymentFingerprint !==
      input.deploymentIdentity.manifestId
  ) {
    throw new Error("prover funding deployment identity mismatch");
  }
  if (
    input.requirements.protocolParametersDigest !==
    input.protocolParameters.snapshotDigest
  ) {
    throw new Error("prover funding protocol-parameters digest mismatch");
  }
  const economics = await watcherDeploymentReleaseEconomicsAuthorityV1(
    input.deploymentIdentity,
  ).verifyForWorkflow({
    deploymentFingerprint: input.deploymentIdentity.manifestId,
  });
  if (input.requirements.economicsPolicyDigest !== economics.policyDigest) {
    throw new Error("prover funding economics-policy digest mismatch");
  }

  const parameters = input.protocolParameters.snapshot;
  const maxTxSize = BigInt(parameters.maxTxSize);
  const maxValueSize = BigInt(parameters.maxValueSize);
  const maxMemory = BigInt(parameters.maxTxExUnits.memory);
  const maxSteps = BigInt(parameters.maxTxExUnits.steps);
  const maxReferenceScriptBytes = BigInt(
    parameters.referenceScriptFee.maximumSizeBytes,
  );
  const referenceRange = BigInt(parameters.referenceScriptFee.range);
  const collateralPercentage = BigInt(parameters.collateralPercentage);
  const collateralFloor = BigInt(
    economics.policy.proverCollateralFloorLovelace,
  );
  const coinsPerUtxoByte = BigInt(parameters.coinsPerUtxoByte);
  const priceMemory = rational(parameters.priceMemory);
  const priceSteps = rational(parameters.priceSteps);
  const referenceBase = rational(parameters.referenceScriptFee.base);
  const referenceMultiplier = rational(
    parameters.referenceScriptFee.multiplier,
  );
  const actions = input.requirements.actions.map((action) => {
    const signedBytes = BigInt(action.signedTransactionBytes);
    const memory = BigInt(action.executionUnits.memory);
    const steps = BigInt(action.executionUnits.steps);
    const referenceScriptBytes = BigInt(action.referenceScriptBytes);
    if (signedBytes > maxTxSize) {
      throw new Error(`${action.actionKind} exceeds signed maxTxSize`);
    }
    if (memory > maxMemory || steps > maxSteps) {
      throw new Error(`${action.actionKind} exceeds maxTxExUnits`);
    }
    if (referenceScriptBytes > maxReferenceScriptBytes) {
      throw new Error(
        `${action.actionKind} exceeds maximum reference-script bytes`,
      );
    }
    let transaction: CML.Transaction;
    try {
      transaction = CML.Transaction.from_cbor_hex(
        action.signedTransactionCborHex,
      );
    } catch {
      throw new Error(`${action.actionKind} transaction cannot be re-admitted`);
    }
    const outputMinAda: bigint[] = [];
    for (const outputCborHex of action.outputCborHex) {
      const output = CML.TransactionOutput.from_cbor_hex(outputCborHex);
      if (
        BigInt(output.amount().to_canonical_cbor_hex().length / 2) >
        maxValueSize
      ) {
        throw new Error(`${action.actionKind} output exceeds maxValueSize`);
      }
      const minimum = CML.min_ada_required(output, coinsPerUtxoByte);
      if (output.amount().coin() < minimum) {
        throw new Error(`${action.actionKind} output is below exact min-Ada`);
      }
      outputMinAda.push(minimum);
    }
    const linearFee =
      BigInt(parameters.minFeeA) * signedBytes + BigInt(parameters.minFeeB);
    const executionFee = ceil(
      add(
        multiplyNatural(priceMemory, memory),
        multiplyNatural(priceSteps, steps),
      ),
    );
    const referenceFee = tieredReferenceScriptFee({
      bytes: referenceScriptBytes,
      base: referenceBase,
      range: referenceRange,
      multiplier: referenceMultiplier,
    });
    const minimumProtocolFee = linearFee + executionFee + referenceFee;
    const transactionFee = transaction.body().fee();
    const ordinaryInputCount = transaction.body().inputs().len();
    if (ordinaryInputCount < 1) {
      throw new Error(
        `${action.actionKind} transaction has no ordinary inputs`,
      );
    }
    if (transactionFee < minimumProtocolFee) {
      throw new Error(
        `${action.actionKind} signed fee is below the live protocol minimum`,
      );
    }
    const collateral = action.collateralRequired
      ? (() => {
          const derived = ceilPercentage(transactionFee, collateralPercentage);
          return derived > collateralFloor ? derived : collateralFloor;
        })()
      : 0n;
    const collateralInputs = transaction.body().collateral_inputs();
    const collateralInputCount = collateralInputs?.len() ?? 0;
    const totalCollateral = transaction.body().total_collateral();
    const collateralReturn = transaction.body().collateral_return();
    if (action.collateralRequired) {
      if (
        collateralInputCount < 1 ||
        collateralInputCount > Number(parameters.maxCollateralInputs)
      ) {
        throw new Error(
          `${action.actionKind} collateral input count differs from the signed release bound`,
        );
      }
      if (totalCollateral === undefined || totalCollateral !== collateral) {
        throw new Error(
          `${action.actionKind} total collateral differs from the exact requirement`,
        );
      }
      if (
        collateralReturn !== undefined &&
        collateralReturn.amount().has_multiassets()
      ) {
        throw new Error(
          `${action.actionKind} collateral return is not pure Ada`,
        );
      }
    } else if (
      collateralInputCount !== 0 ||
      totalCollateral !== undefined ||
      collateralReturn !== undefined
    ) {
      throw new Error(`${action.actionKind} unexpectedly declares collateral`);
    }
    let walletChange = 0n;
    let lockedCapital = 0n;
    let releasedCapital = 0n;
    const lockedAssets = new Map<string, bigint>();
    const releasedAssets = new Map<string, bigint>();
    for (const controlled of action.fundingControlledOutputs) {
      if (controlled.role === "protocol") continue;
      const lovelace = BigInt(controlled.fundingLovelace);
      if (controlled.role === "wallet_change") walletChange += lovelace;
      else {
        lockedCapital += lovelace;
        for (const { unit, quantity } of controlled.fundingAssets) {
          lockedAssets.set(
            unit,
            (lockedAssets.get(unit) ?? 0n) + BigInt(quantity),
          );
        }
      }
    }
    for (const controlled of action.fundingControlledInputs) {
      if (controlled.role !== "released_locked") continue;
      releasedCapital += BigInt(controlled.fundingLovelace);
      for (const { unit, quantity } of controlled.fundingAssets) {
        releasedAssets.set(
          unit,
          (releasedAssets.get(unit) ?? 0n) + BigInt(quantity),
        );
      }
    }
    const attemptCount = BigInt(action.conflictRetryCount) + 1n;
    return Object.freeze({
      actionKind: action.actionKind,
      transactionFeeLovelace: transactionFee.toString(),
      minimumProtocolFeeLovelace: minimumProtocolFee.toString(),
      linearFeeLovelace: linearFee.toString(),
      executionFeeLovelace: executionFee.toString(),
      referenceScriptFeeLovelace: referenceFee.toString(),
      outputMinAdaLovelace: sum(outputMinAda).toString(),
      requiredBondLovelace: action.requiredBondLovelace,
      requiredRewardCustodyLovelace: action.requiredRewardCustodyLovelace,
      collateralLovelace: collateral.toString(),
      collateralInputCount: collateralInputCount.toString(),
      collateralReturnLovelace:
        collateralReturn?.amount().coin().toString() ?? null,
      ordinaryInputCount: ordinaryInputCount.toString(),
      fundingControlledInputCount:
        action.fundingControlledInputs.length.toString(),
      walletFundingInputCount: action.fundingControlledInputs
        .filter(({ role }) => role === "wallet_funding")
        .length.toString(),
      walletChangeLovelace: walletChange.toString(),
      lockedCapitalLovelace: lockedCapital.toString(),
      releasedCapitalLovelace: releasedCapital.toString(),
      lockedNativeAssets: assetEntries(lockedAssets),
      releasedNativeAssets: assetEntries(releasedAssets),
      attemptCount: attemptCount.toString(),
      feeHeadroomLovelace: (transactionFee * attemptCount).toString(),
    });
  });

  const feeHeadroom = sum(
    actions.map((action) => BigInt(action.feeHeadroomLovelace)),
  );
  const outputMinAda = sum(
    actions.map((action) => BigInt(action.outputMinAdaLovelace)),
  );
  const requiredBond = sum(
    actions.map((action) => BigInt(action.requiredBondLovelace)),
  );
  const rewardCustody = sum(
    actions.map((action) => BigInt(action.requiredRewardCustodyLovelace)),
  );
  const reusableCollateral = actions.reduce((maximum, action) => {
    const observed = BigInt(action.collateralLovelace);
    return observed > maximum ? observed : maximum;
  }, 0n);
  const flow = capitalFlow(actions);
  const calculationInput = Object.freeze({
    schemaVersion: WATCHER_PRODUCTION_PROVER_FUNDING_CALCULATION_V1,
    scope: input.requirements.scope,
    deploymentFingerprint: input.deploymentIdentity.manifestId,
    profileDigest: input.requirements.profileDigest,
    protocolParametersDigest: input.protocolParameters.snapshotDigest,
    economicsPolicyDigest: economics.policyDigest,
    fundingPaymentKeyHash: input.requirements.fundingPaymentKeyHash,
    actions: Object.freeze(actions),
    totals: Object.freeze({
      feeHeadroomLovelace: feeHeadroom.toString(),
      outputMinAdaLovelace: outputMinAda.toString(),
      requiredBondLovelace: requiredBond.toString(),
      requiredRewardCustodyLovelace: rewardCustody.toString(),
      reusableCollateralLovelace: reusableCollateral.toString(),
      peakCapitalLovelace: flow.peakLovelace.toString(),
      endingCapitalLovelace: flow.endingLovelace.toString(),
      requiredLovelace: (flow.peakLovelace + reusableCollateral).toString(),
      requiredNativeAssets: flow.peakNativeAssets,
      maximumCollateralInputs: parameters.maxCollateralInputs,
      maximumOrdinaryInputs: actions
        .reduce((maximum, action) => {
          const observed = BigInt(action.ordinaryInputCount);
          return observed > maximum ? observed : maximum;
        }, 0n)
        .toString(),
      maximumFundingInputs: actions
        .reduce((maximum, action) => {
          const observed = BigInt(action.walletFundingInputCount);
          return observed > maximum ? observed : maximum;
        }, 0n)
        .toString(),
    }),
  });
  const calculation = Object.freeze({
    ...calculationInput,
    calculationDigest: computeDeploymentManifestV1JsonDigest(calculationInput),
  });
  admittedCalculations.add(calculation);
  return calculation;
};

export const WATCHER_PRODUCTION_PROVER_FUNDING_SWEEP_V1 =
  "midgard-watcher-production-prover-funding-sweep-v1" as const;

export type WatcherProductionProverFundingSweepV1 = Readonly<{
  schemaVersion: typeof WATCHER_PRODUCTION_PROVER_FUNDING_SWEEP_V1;
  deploymentFingerprint: string;
  protocolParametersDigest: string;
  economicsPolicyDigest: string;
  fundingPaymentKeyHash: string;
  categoryCalculationDigests: Readonly<
    Record<FraudProofCatalogueCategoryName, string>
  >;
  availabilityCalculationDigest: string;
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
  }>;
  sweepDigest: string;
}>;

const admittedSweeps = new WeakSet<object>();

export const assertWatcherProductionProverFundingSweepV1 = (
  sweep: WatcherProductionProverFundingSweepV1,
): void => {
  if (!admittedSweeps.has(sweep)) {
    throw new Error("prover funding sweep is not admitted");
  }
};

/**
 * Exact C80 funding authority for all 32 catalogue families plus Q58. Every
 * non-reusable cost is summed; collateral is a single reusable maximum. Native
 * assets are summed because no cross-workflow custody-reuse proof is currently
 * authenticated, which is the conservative complete-sweep requirement.
 */
export const aggregateWatcherProductionProverFundingSweepV1 = (
  calculations: readonly WatcherProductionProverFundingCalculationV1[],
): WatcherProductionProverFundingSweepV1 => {
  for (const calculation of calculations) {
    assertWatcherProductionProverFundingCalculationV1(calculation);
  }
  if (
    calculations.length !== FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length + 1 ||
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.some(
      (category, index) =>
        calculations[index]?.scope.kind !== "fraud_proof_category" ||
        calculations[index]?.scope.category !== category,
    ) ||
    calculations.at(-1)?.scope.kind !== "da_availability_lifecycle"
  ) {
    throw new Error(
      "prover funding sweep requires exact canonical 32-category order followed by Q58",
    );
  }
  const categoryCalculations = new Map<
    FraudProofCatalogueCategoryName,
    WatcherProductionProverFundingCalculationV1
  >();
  let availability: WatcherProductionProverFundingCalculationV1 | undefined;
  for (const calculation of calculations) {
    if (calculation.scope.kind === "fraud_proof_category") {
      if (categoryCalculations.has(calculation.scope.category)) {
        throw new Error("prover funding sweep has duplicate category profiles");
      }
      categoryCalculations.set(calculation.scope.category, calculation);
    } else {
      if (availability !== undefined) {
        throw new Error(
          "prover funding sweep has duplicate availability profiles",
        );
      }
      availability = calculation;
    }
  }
  const actualCategories = [...categoryCalculations.keys()];
  if (
    actualCategories.length !== FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.length ||
    FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.some(
      (category, index) => actualCategories[index] !== category,
    )
  ) {
    throw new Error(
      "prover funding sweep requires exact canonical 32-category order",
    );
  }
  if (availability === undefined) {
    throw new Error("prover funding sweep requires the admitted Q58 lifecycle");
  }
  const first = calculations[0];
  if (first === undefined) throw new Error("prover funding sweep is empty");
  if (
    calculations.some(
      (calculation) =>
        calculation.deploymentFingerprint !== first.deploymentFingerprint ||
        calculation.protocolParametersDigest !==
          first.protocolParametersDigest ||
        calculation.economicsPolicyDigest !== first.economicsPolicyDigest ||
        calculation.fundingPaymentKeyHash !== first.fundingPaymentKeyHash,
    )
  ) {
    throw new Error("prover funding sweep identities differ");
  }
  const all = calculations;
  const sumField = (
    field:
      | "feeHeadroomLovelace"
      | "outputMinAdaLovelace"
      | "requiredBondLovelace"
      | "requiredRewardCustodyLovelace",
  ): bigint => sum(all.map((calculation) => BigInt(calculation.totals[field])));
  const reusableCollateral = all.reduce((maximum, calculation) => {
    const observed = BigInt(calculation.totals.reusableCollateralLovelace);
    return observed > maximum ? observed : maximum;
  }, 0n);
  const flow = capitalFlow(all.flatMap(({ actions }) => actions));
  const feeHeadroom = sumField("feeHeadroomLovelace");
  const outputMinAda = sumField("outputMinAdaLovelace");
  const requiredBond = sumField("requiredBondLovelace");
  const rewardCustody = sumField("requiredRewardCustodyLovelace");
  const sweepInput = Object.freeze({
    schemaVersion: WATCHER_PRODUCTION_PROVER_FUNDING_SWEEP_V1,
    deploymentFingerprint: first.deploymentFingerprint,
    protocolParametersDigest: first.protocolParametersDigest,
    economicsPolicyDigest: first.economicsPolicyDigest,
    fundingPaymentKeyHash: first.fundingPaymentKeyHash,
    categoryCalculationDigests: Object.freeze(
      Object.fromEntries(
        FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.map((category) => [
          category,
          categoryCalculations.get(category)!.calculationDigest,
        ]),
      ) as Record<FraudProofCatalogueCategoryName, string>,
    ),
    availabilityCalculationDigest: availability.calculationDigest,
    totals: Object.freeze({
      feeHeadroomLovelace: feeHeadroom.toString(),
      outputMinAdaLovelace: outputMinAda.toString(),
      requiredBondLovelace: requiredBond.toString(),
      requiredRewardCustodyLovelace: rewardCustody.toString(),
      reusableCollateralLovelace: reusableCollateral.toString(),
      peakCapitalLovelace: flow.peakLovelace.toString(),
      endingCapitalLovelace: flow.endingLovelace.toString(),
      requiredLovelace: (flow.peakLovelace + reusableCollateral).toString(),
      requiredNativeAssets: flow.peakNativeAssets,
    }),
  });
  const sweep = Object.freeze({
    ...sweepInput,
    sweepDigest: computeDeploymentManifestV1JsonDigest(sweepInput),
  });
  admittedSweeps.add(sweep);
  return sweep;
};
