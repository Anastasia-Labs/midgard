import { computeDeploymentManifestJsonDigest } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  assertAdmittedWorkflowFundingRequirements,
  isProtocolFundedWorkflowAction,
  type WorkflowFundingRequirements,
} from "@al-ft/midgard-fault-proofs";
import { CML } from "@lucid-evolution/lucid";

import {
  assertVerifiedWatcherDeploymentIdentity,
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentReleaseEconomicsAuthority,
} from "../runtime/deployment-identity.js";
import {
  assertWatcherProtocolParameterRuntimeAuthority,
  type WatcherProtocolParameterRuntimeAuthority,
} from "./prover-funding.js";
import {
  add,
  admittedCalculations,
  assetEntries,
  capitalFlow,
  ceil,
  ceilPercentage,
  multiplyNatural,
  rational,
  sum,
  tieredReferenceScriptFee,
  WATCHER_PROVER_FUNDING_CALCULATION,
  type WatcherProverFundingCalculation,
} from "./prover-funding-calculation.capital-flow.js";

/**
 * Calculates the release-authenticated maximum-shape funding requirement.
 * The measured profile must have been bound by a fixed admitted workflow
 * factory; a digest-valid caller-authored profile is insufficient.
 */
export const calculateWatcherProverFunding = async (input: {
  readonly deploymentIdentity: VerifiedWatcherDeploymentIdentity;
  readonly protocolParameters: WatcherProtocolParameterRuntimeAuthority;
  readonly requirements: WorkflowFundingRequirements;
}): Promise<WatcherProverFundingCalculation> => {
  assertVerifiedWatcherDeploymentIdentity(input.deploymentIdentity);
  assertWatcherProtocolParameterRuntimeAuthority(input.protocolParameters);
  assertAdmittedWorkflowFundingRequirements(input.requirements);
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
  const economics = await watcherDeploymentReleaseEconomicsAuthority(
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
      if (
        controlled.role === "protocol" ||
        controlled.role === "protocol_reward"
      )
        continue;
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
      feeHeadroomLovelace: (isProtocolFundedWorkflowAction(action)
        ? 0n
        : transactionFee * attemptCount
      ).toString(),
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
    schemaVersion: WATCHER_PROVER_FUNDING_CALCULATION,
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
    calculationDigest: computeDeploymentManifestJsonDigest(calculationInput),
  });
  admittedCalculations.add(calculation);
  return calculation;
};

export const WATCHER_PROVER_FUNDING_SWEEP =
  "midgard-watcher-production-prover-funding-sweep-v1" as const;
