import {
  computeDeploymentManifestJsonDigest,
  type DeploymentManifestCardanoProtocolParameters,
  parseDeploymentManifestCardanoProtocolParameters,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";
import { CML, getAddressDetails } from "@lucid-evolution/lucid";

import type { WorkflowAdapterRunner } from "./adapters.js";
import {
  validateVerifiedFraudProofReleaseEconomicsPolicy,
  type VerifiedFraudProofReleaseEconomicsPolicy,
} from "./release-economics-policy.js";
import { isAdmittedWorkflowRunner } from "./runner-admission.js";

export type WorkflowRuntimeFundingContract = Readonly<{
  address: string;
  scriptHash: string;
  role:
    | "protocol_state"
    | "proof_thread"
    | "field_carrier"
    | "prover_bond"
    | "prover_reward"
    | "correction_lock";
}>;

export interface WorkflowRuntimeFundingPolicy {
  readonly policyVersion: "midgard-workflow-runtime-funding-policy-v1";
}

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
  parameters: DeploymentManifestCardanoProtocolParameters;
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

type PolicyValue = Readonly<{
  category: FraudProofCatalogueCategoryName;
  deploymentFingerprint: string;
  fundingPaymentKeyHash: string;
  protocolParameters: DeploymentManifestCardanoProtocolParameters;
  protocolParametersDigest: string;
  economics: VerifiedFraudProofReleaseEconomicsPolicy;
  economicsPolicyDigest: string;
  contracts: readonly WorkflowRuntimeFundingContract[];
  referenceScripts: readonly Readonly<{ outRef: string; scriptHash: string }>[];
  maximumFeeLovelace: string;
  maximumCollateralLovelace: string;
  maximumSlashCollateralLovelace: string;
  maximumCollateralInputs: string;
  policyDigest: string;
}>;
const admitted = new WeakMap<
  object,
  { runner: WorkflowAdapterRunner; value: PolicyValue }
>();

/**
 * The admitted application calls this after authenticating its deployment,
 * current local-node parameters and contract roster. A sample transaction is
 * never funding authority; every actual signed transaction is checked later.
 */
export const createWorkflowRuntimeFundingPolicy = ({
  category,
  runner,
  deploymentFingerprint,
  fundingPaymentKeyHash,
  protocolParameters,
  economics,
  contracts,
  referenceScripts,
}: {
  category: FraudProofCatalogueCategoryName;
  runner: WorkflowAdapterRunner;
  deploymentFingerprint: string;
  fundingPaymentKeyHash: string;
  protocolParameters: DeploymentManifestCardanoProtocolParameters;
  economics: VerifiedFraudProofReleaseEconomicsPolicy;
  contracts: readonly WorkflowRuntimeFundingContract[];
  referenceScripts: readonly Readonly<{ outRef: string; scriptHash: string }>[];
}): WorkflowRuntimeFundingPolicy => {
  if (!isAdmittedWorkflowRunner({ category, runner }))
    throw new Error(
      "runtime funding policy requires the fixed category runner",
    );
  if (
    !/^[0-9a-f]{64}$/u.test(deploymentFingerprint) ||
    !/^[0-9a-f]{56}$/u.test(fundingPaymentKeyHash)
  )
    throw new Error("runtime funding policy identity is malformed");
  const parameters =
    parseDeploymentManifestCardanoProtocolParameters(protocolParameters);
  const release = validateVerifiedFraudProofReleaseEconomicsPolicy(economics);
  if (release.deploymentIdentityDigest !== deploymentFingerprint)
    throw new Error("runtime funding economics belongs to another deployment");
  const roles = new Set([
    "protocol_state",
    "proof_thread",
    "field_carrier",
    "prover_bond",
    "prover_reward",
    "correction_lock",
  ]);
  const addresses = new Set<string>();
  const roster = contracts
    .map((contract) => {
      const credential = getAddressDetails(contract.address).paymentCredential;
      if (
        !roles.has(contract.role) ||
        !/^[0-9a-f]{56}$/u.test(contract.scriptHash) ||
        credential?.type !== "Script" ||
        credential.hash !== contract.scriptHash ||
        CML.Address.from_bech32(contract.address).to_bech32() !==
          contract.address ||
        addresses.has(contract.address)
      )
        throw new Error(
          "runtime funding contract identity is invalid or repeated",
        );
      addresses.add(contract.address);
      return Object.freeze({ ...contract });
    })
    .sort((left, right) => left.address.localeCompare(right.address));
  if (roster.length === 0)
    throw new Error("runtime funding contract roster is empty");
  const referenceOutRefs = new Set<string>();
  const references = referenceScripts
    .map((reference) => {
      if (
        !/^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u.test(reference.outRef) ||
        !/^[0-9a-f]{56}$/u.test(reference.scriptHash) ||
        referenceOutRefs.has(reference.outRef)
      )
        throw new Error(
          "runtime funding reference script identity is invalid or repeated",
        );
      referenceOutRefs.add(reference.outRef);
      return Object.freeze({ ...reference });
    })
    .sort((left, right) => left.outRef.localeCompare(right.outRef));
  const maximumFee = workflowRuntimeFundingMinimumFee({
    parameters,
    transactionBytes: BigInt(parameters.maxTxSize),
    memory: BigInt(parameters.maxTxExUnits.memory),
    steps: BigInt(parameters.maxTxExUnits.steps),
    referenceScriptBytes: BigInt(
      parameters.referenceScriptFee.maximumSizeBytes,
    ),
  });
  const collateral =
    (maximumFee * BigInt(parameters.collateralPercentage) + 99n) / 100n;
  const floor = BigInt(release.policy.proverCollateralFloorLovelace);
  const basis = Object.freeze({
    category,
    deploymentFingerprint,
    fundingPaymentKeyHash,
    protocolParameters: parameters,
    protocolParametersDigest: computeDeploymentManifestJsonDigest(parameters),
    economics: release,
    economicsPolicyDigest: release.policyDigest,
    contracts: Object.freeze(roster),
    referenceScripts: Object.freeze(references),
    maximumFeeLovelace: maximumFee.toString(),
    maximumCollateralLovelace: (collateral > floor
      ? collateral
      : floor
    ).toString(),
    maximumSlashCollateralLovelace: (
      (BigInt(release.policy.slashingPenaltyLovelace) *
        BigInt(parameters.collateralPercentage) +
        99n) /
      100n
    ).toString(),
    maximumCollateralInputs: parameters.maxCollateralInputs,
  });
  const value = Object.freeze({
    ...basis,
    policyDigest: computeDeploymentManifestJsonDigest(basis),
  });
  const policy: WorkflowRuntimeFundingPolicy = Object.freeze({
    policyVersion: "midgard-workflow-runtime-funding-policy-v1",
  });
  admitted.set(policy, { runner, value });
  return policy;
};

export const readWorkflowRuntimeFundingPolicy = (
  policy: WorkflowRuntimeFundingPolicy,
): PolicyValue => {
  const cell = admitted.get(policy);
  if (cell === undefined)
    throw new Error("runtime funding policy is not admitted");
  return cell.value;
};

export const assertWorkflowRuntimeFundingPolicyRunner = ({
  policy,
  runner,
  category,
}: {
  policy: WorkflowRuntimeFundingPolicy;
  runner: WorkflowAdapterRunner;
  category: FraudProofCatalogueCategoryName;
}): void => {
  const cell = admitted.get(policy);
  if (cell?.runner !== runner || cell.value.category !== category)
    throw new Error("runtime funding policy differs from its fixed runner");
};
