import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  readWorkflowRuntimeFundingPolicy,
  type WorkflowRuntimeFundingPolicy,
} from "@al-ft/midgard-fault-proofs";
import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentReleaseEconomicsAuthority,
} from "../runtime/deployment-identity.js";
import {
  assertWatcherProtocolParameterRuntimeAuthority,
  type WatcherProtocolParameterRuntimeAuthority,
} from "./prover-funding.js";
import { WATCHER_PROVER_FUNDING_SWEEP } from "./prover-funding-calculation.calculate-watcher-prover-funding.js";
import {
  assertWatcherProverFundingCalculation,
  capitalFlow,
  sum,
  type WatcherProverFundingCalculation,
} from "./prover-funding-calculation.capital-flow.js";

export type WatcherProverFundingSweep = Readonly<{
  schemaVersion: typeof WATCHER_PROVER_FUNDING_SWEEP;
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

export const assertWatcherProverFundingSweep = (
  sweep: WatcherProverFundingSweep,
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
export const aggregateWatcherProverFundingSweep = (
  calculations: readonly WatcherProverFundingCalculation[],
): WatcherProverFundingSweep => {
  for (const calculation of calculations) {
    assertWatcherProverFundingCalculation(calculation);
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
    WatcherProverFundingCalculation
  >();
  let availability: WatcherProverFundingCalculation | undefined;
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
    schemaVersion: WATCHER_PROVER_FUNDING_SWEEP,
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
    sweepDigest: computeDeploymentManifestJsonDigest(sweepInput),
  });
  admittedSweeps.add(sweep);
  return sweep;
};

export const WATCHER_RUNTIME_PROVER_FUNDING_CALCULATION =
  "midgard-watcher-runtime-prover-funding-calculation-v1" as const;

/** Reservation bounds from live protocol and deployed economics, without a measured recipe. */
export type WatcherRuntimeProverFundingCalculation = Readonly<{
  schemaVersion: typeof WATCHER_RUNTIME_PROVER_FUNDING_CALCULATION;
  deploymentFingerprint: string;
  policyDigest: string;
  protocolParametersDigest: string;
  economicsPolicyDigest: string;
  fundingPaymentKeyHash: string;
  collateralFloorLovelace: string;
  maximumCollateralInputs: string;
  maximumFeeLovelace: string;
  maximumCollateralLovelace: string;
  maximumSlashCollateralLovelace: string;
  reservationBasisDigest: string;
}>;

const admittedRuntimeCalculations = new WeakSet<object>();

export const assertWatcherRuntimeProverFundingCalculation = (
  calculation: WatcherRuntimeProverFundingCalculation,
): void => {
  if (!admittedRuntimeCalculations.has(calculation))
    throw new Error("runtime prover funding calculation is not admitted");
};

export const calculateWatcherRuntimeProverFunding = async (input: {
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly protocolParameters: WatcherProtocolParameterRuntimeAuthority;
  readonly policy: WorkflowRuntimeFundingPolicy;
}): Promise<WatcherRuntimeProverFundingCalculation> => {
  assertVerifiedWatcherDeploymentIdentity(input.deploymentIdentity);
  assertWatcherProtocolParameterRuntimeAuthority(input.protocolParameters);
  const policy = readWorkflowRuntimeFundingPolicy(input.policy);
  const deploymentFingerprint = input.deploymentIdentity.manifestId;
  if (
    policy.deploymentFingerprint !== deploymentFingerprint ||
    input.protocolParameters.deploymentFingerprint !== deploymentFingerprint
  )
    throw new Error("runtime prover funding deployment identity mismatch");
  if (
    policy.protocolParametersDigest !== input.protocolParameters.snapshotDigest
  )
    throw new Error("runtime prover funding protocol parameters mismatch");
  const economics = await watcherDeploymentReleaseEconomicsAuthority(
    input.deploymentIdentity,
  ).verifyForWorkflow({ deploymentFingerprint });
  if (policy.economicsPolicyDigest !== economics.policyDigest)
    throw new Error("runtime prover funding economics policy mismatch");
  const basis = Object.freeze({
    schemaVersion: WATCHER_RUNTIME_PROVER_FUNDING_CALCULATION,
    deploymentFingerprint,
    policyDigest: policy.policyDigest,
    protocolParametersDigest: input.protocolParameters.snapshotDigest,
    economicsPolicyDigest: economics.policyDigest,
    fundingPaymentKeyHash: policy.fundingPaymentKeyHash,
    collateralFloorLovelace: economics.policy.proverCollateralFloorLovelace,
    maximumCollateralInputs: policy.maximumCollateralInputs,
    maximumFeeLovelace: policy.maximumFeeLovelace,
    maximumCollateralLovelace: policy.maximumCollateralLovelace,
    maximumSlashCollateralLovelace: policy.maximumSlashCollateralLovelace,
  });
  const calculation = Object.freeze({
    ...basis,
    reservationBasisDigest: computeDeploymentManifestJsonDigest(basis),
  });
  admittedRuntimeCalculations.add(calculation);
  return calculation;
};
