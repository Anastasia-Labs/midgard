import {
  computeDeploymentManifestJsonDigest,
  type DeploymentManifestCardanoProtocolParameters,
  parseDeploymentManifestCardanoProtocolParameters,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { SELECTED_DEPLOYMENT_PROFILE } from "@al-ft/midgard-core/deployment-profile";
import {
  type FraudProofCatalogueCategoryName,
  VALIDATION_CASE_FLOOR_LOVELACE,
} from "@al-ft/midgard-sdk";
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
 * The validation-trace dispute's VerifySource custody terms: the dispute
 * bond the case validator is applied with, bound by the signed deployment
 * (the manifest's `availabilityChallenge.challengerBondLovelace`), and its
 * contract addresses. The case floor is not among them: it is fixed by the
 * compiled profile's constant `VALIDATION_CASE_FLOOR_LOVELACE`
 * (`fraud_proof_output_lovelace + max_dispute_move_fee_lovelace`), not by
 * the signed deployment.
 */
export type WorkflowRuntimeValidationDisputeTerms = Readonly<{
  deploymentFingerprint: string;
  bondLovelace: bigint;
  /** The dispute-bond spending validator's address. */
  bondAddress: string;
  /** The case spending validator's address. */
  caseAddress: string;
  /** The case minting policy, which mints the case NFT and the bond marker. */
  casePolicyId: string;
  /**
   * The thread moves' contracts, present when the runner plays threads: the
   * game, timeout and award spending addresses and the receipt policy, and
   * when the runner drives the one-step resolution, the boundary's and the
   * one-step resolvers' spending addresses a thread visits between the game
   * and the award. The per-move fee cap and the receipt's lovelace are the
   * compiled profile's.
   */
  moves?: Readonly<{
    gameAddress: string;
    timeoutAddress: string;
    awardAddress: string;
    awardPolicyId: string;
    oneStep?: ValidationDisputeOneStepTerms;
  }>;
}>;

/** The holders a thread of ours visits in its one-step resolution. */
export type ValidationDisputeOneStepTerms = Readonly<{
  boundaryAddress: string;
  resolverAddresses: readonly string[];
}>;

type ValidationDisputeTermsValue = Readonly<{
  bondLovelace: string;
  caseFloorLovelace: string;
  bondAddress: string;
  caseAddress: string;
  casePolicyId: string;
  moves?: Readonly<{
    gameAddress: string;
    timeoutAddress: string;
    awardAddress: string;
    awardPolicyId: string;
    moveFeeCapLovelace: string;
    fraudProofOutputLovelace: string;
    oneStep?: ValidationDisputeOneStepTerms;
  }>;
}>;

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
  /** Present only for a validation-trace dispute policy given its terms. */
  validationDispute?: ValidationDisputeTermsValue;
  policyDigest: string;
}>;
const admitted = new WeakMap<
  object,
  { runner: WorkflowAdapterRunner; value: PolicyValue }
>();

const validationDisputeTerms = ({
  category,
  deploymentFingerprint,
  terms,
  roster,
}: {
  category: FraudProofCatalogueCategoryName;
  deploymentFingerprint: string;
  terms: WorkflowRuntimeValidationDisputeTerms;
  roster: readonly WorkflowRuntimeFundingContract[];
}): ValidationDisputeTermsValue => {
  if (category !== "validationTraceDispute")
    throw new Error(
      `runtime funding validation dispute terms are refused for category ${category}`,
    );
  if (terms.deploymentFingerprint !== deploymentFingerprint)
    throw new Error(
      "runtime funding validation dispute terms belong to another deployment",
    );
  if (typeof terms.bondLovelace !== "bigint" || terms.bondLovelace <= 0n)
    throw new Error(
      "runtime funding validation dispute bond is not a positive lovelace amount",
    );
  if (!/^[0-9a-f]{56}$/u.test(terms.casePolicyId))
    throw new Error("runtime funding validation case policy is malformed");
  const custody = (address: string, label: string) => {
    const contract = roster.find((entry) => entry.address === address);
    if (contract?.role !== "proof_thread")
      throw new Error(
        `runtime funding validation dispute ${label} address is not a rostered proof-thread contract`,
      );
  };
  custody(terms.bondAddress, "bond");
  custody(terms.caseAddress, "case");
  const moves = terms.moves;
  const addresses = [terms.bondAddress, terms.caseAddress];
  if (moves !== undefined) {
    custody(moves.gameAddress, "game");
    custody(moves.timeoutAddress, "timeout");
    custody(moves.awardAddress, "award");
    if (!/^[0-9a-f]{56}$/u.test(moves.awardPolicyId))
      throw new Error("runtime funding validation award policy is malformed");
    addresses.push(moves.gameAddress, moves.timeoutAddress, moves.awardAddress);
    const { oneStep } = moves;
    if (oneStep !== undefined) {
      if (oneStep.resolverAddresses.length === 0)
        throw new Error(
          "runtime funding validation dispute one-step terms name no resolver",
        );
      custody(oneStep.boundaryAddress, "boundary");
      for (const resolver of oneStep.resolverAddresses)
        custody(resolver, "one-step resolver");
      addresses.push(oneStep.boundaryAddress, ...oneStep.resolverAddresses);
    }
  }
  if (new Set(addresses).size !== addresses.length)
    throw new Error(
      "runtime funding validation dispute contract addresses coincide",
    );
  const { limits } = SELECTED_DEPLOYMENT_PROFILE;
  return Object.freeze({
    bondLovelace: terms.bondLovelace.toString(),
    caseFloorLovelace: VALIDATION_CASE_FLOOR_LOVELACE.toString(),
    bondAddress: terms.bondAddress,
    caseAddress: terms.caseAddress,
    casePolicyId: terms.casePolicyId,
    ...(moves === undefined
      ? {}
      : {
          moves: Object.freeze({
            gameAddress: moves.gameAddress,
            timeoutAddress: moves.timeoutAddress,
            awardAddress: moves.awardAddress,
            awardPolicyId: moves.awardPolicyId,
            moveFeeCapLovelace: limits.max_dispute_move_fee_lovelace.toString(),
            fraudProofOutputLovelace:
              limits.fraud_proof_output_lovelace.toString(),
            ...(moves.oneStep === undefined
              ? {}
              : {
                  oneStep: Object.freeze({
                    boundaryAddress: moves.oneStep.boundaryAddress,
                    resolverAddresses: Object.freeze([
                      ...moves.oneStep.resolverAddresses,
                    ]),
                  }),
                }),
          }),
        }),
  });
};

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
  validationDispute,
}: {
  category: FraudProofCatalogueCategoryName;
  runner: WorkflowAdapterRunner;
  deploymentFingerprint: string;
  fundingPaymentKeyHash: string;
  protocolParameters: DeploymentManifestCardanoProtocolParameters;
  economics: VerifiedFraudProofReleaseEconomicsPolicy;
  contracts: readonly WorkflowRuntimeFundingContract[];
  referenceScripts: readonly Readonly<{ outRef: string; scriptHash: string }>[];
  validationDispute?: WorkflowRuntimeValidationDisputeTerms;
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
  const maximumFee = workflowRuntimeFundingMaximumFee(parameters);
  const disputeTerms =
    validationDispute === undefined
      ? undefined
      : validationDisputeTerms({
          category,
          deploymentFingerprint,
          terms: validationDispute,
          roster,
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
    // Absent terms leave every other policy's digest unchanged.
    ...(disputeTerms === undefined ? {} : { validationDispute: disputeTerms }),
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
