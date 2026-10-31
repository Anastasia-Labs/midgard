import {
  DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE,
  type DeploymentManifestCardanoProtocolParameters,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import { credentialToAddress } from "@lucid-evolution/lucid";

import type { WorkflowAdapterRunner } from "../../src/workflow/adapters.js";
import {
  computeFraudProofReleaseEconomicsPolicyDigest,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
} from "../../src/workflow/release-economics-policy.js";
import { WORKFLOW_RUNNER_FACTORIES } from "../../src/workflow/runtime.js";
import {
  createWorkflowRuntimeFundingPolicy,
  type WorkflowRuntimeFundingContract,
} from "../../src/workflow/runtime-funding-policy.js";

export const RUNTIME_FUNDING_TEST_PARAMETERS: DeploymentManifestCardanoProtocolParameters =
  {
    minFeeA: "44",
    minFeeB: "155381",
    priceMemory: { numerator: "577", denominator: "10000" },
    priceSteps: { numerator: "721", denominator: "10000000" },
    coinsPerUtxoByte: "4310",
    collateralPercentage: "150",
    maxCollateralInputs: "3",
    maxTxSize: "16384",
    maxValueSize: "5000",
    maxTxExUnits: { memory: "14000000", steps: "10000000000" },
    referenceScriptFee: {
      base: { numerator: "15", denominator: "1" },
      range: "25600",
      multiplier: { numerator: "6", denominator: "5" },
      maximumSizeBytes: "204800",
    },
  };

export const runtimeFundingPolicyFixture = (input: {
  deploymentFingerprint: string;
  fundingPaymentKeyHash: string;
  contracts?: readonly WorkflowRuntimeFundingContract[];
  referenceScripts?: readonly Readonly<{
    outRef: string;
    scriptHash: string;
  }>[];
  runner?: WorkflowAdapterRunner;
}) => {
  const runner =
    input.runner ??
    WORKFLOW_RUNNER_FACTORIES.doubleSpend(async () => {
      throw new Error("funding fixture runner is never executed");
    });
  const release =
    DEPLOYMENT_MANIFEST_ECONOMICS_BY_PROFILE["bounded-acceptance-v1"];
  const policy = {
    profile: release.profile,
    requiredBondLovelace: release.requiredBondLovelace.toString(),
    slashingPenaltyLovelace: release.slashingPenaltyLovelace.toString(),
    fraudProverRewardLovelace: release.fraudProverRewardLovelace.toString(),
    inactivitySlashingPenaltyLovelace:
      release.inactivitySlashingPenaltyLovelace.toString(),
    proverCollateralFloorLovelace:
      release.proverCollateralFloorLovelace.toString(),
  };
  const constructorInput = {
    category: "doubleSpend" as const,
    runner,
    deploymentFingerprint: input.deploymentFingerprint,
    fundingPaymentKeyHash: input.fundingPaymentKeyHash,
    protocolParameters: RUNTIME_FUNDING_TEST_PARAMETERS,
    economics: {
      schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
      deploymentIdentityDigest: input.deploymentFingerprint,
      blueprintHash: "a1".repeat(32),
      policyDigest: computeFraudProofReleaseEconomicsPolicyDigest(policy),
      policy,
    },
    contracts: input.contracts ?? [
      {
        address: credentialToAddress("Preprod", {
          type: "Script",
          hash: "a5".repeat(28),
        }),
        scriptHash: "a5".repeat(28),
        role: "proof_thread" as const,
      },
    ],
    referenceScripts: input.referenceScripts ?? [],
  };
  return {
    runner,
    policy: createWorkflowRuntimeFundingPolicy(constructorInput),
    constructorInput,
  };
};
