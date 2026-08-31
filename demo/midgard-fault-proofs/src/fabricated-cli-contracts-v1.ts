import {
  FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID_V1,
  FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID_V1,
  type FabricatedDepositFaultProofContracts,
  type FabricatedWithdrawalFaultProofContracts,
} from "@al-ft/midgard-sdk";
import { type UTxO } from "@lucid-evolution/lucid";

import { resolveFaultProofClaimRegistryContractV1 } from "./claim-registry-transaction-v1.js";
import { readJsonFile } from "./json-file.js";
import {
  makeLucidForSubmit,
  requireDeploymentReferenceScript,
  resolveFaultProofDeploymentContracts,
  type SubmitProviderConfig,
} from "./runtime.js";
import { type FabricatedDepositContractsV1 } from "./submit-fabricated-deposit-step-01.js";
import { type FabricatedWithdrawalContractsV1 } from "./submit-fabricated-withdrawal-step-01.js";

/**
 * Deployment-info names of the four `fabricated-deposit` step scripts, in step
 * order. These are the same names `submit-init` registers, so a thread opened
 * by the CLI and one opened by the emulator resolve identical script hashes.
 */
const FABRICATED_DEPOSIT_STEP_DEPLOYMENT_NAMES_V1 = [
  "fraudProofFabricatedDeposit",
  "fraudProofFabricatedDepositStep02",
  "fraudProofFabricatedDepositStep03",
  "fraudProofFabricatedDepositStep04",
] as const;

const FABRICATED_WITHDRAWAL_STEP_DEPLOYMENT_NAMES_V1 = [
  "fraudProofFabricatedWithdrawal",
  "fraudProofFabricatedWithdrawalStep02",
  "fraudProofFabricatedWithdrawalStep03",
  "fraudProofFabricatedWithdrawalStep04",
] as const;

export type FabricatedCliContractsConfigV1 = SubmitProviderConfig & {
  readonly blueprintPath: string;
  readonly deploymentInfoPath: string;
};

export type FabricatedStepIndexV1 = 0 | 1 | 2 | 3;

/**
 * Both fabricated families deploy their step scripts as reference scripts
 * rather than attaching them inline, so a submission needs two things the
 * older CLI families never resolved: the focused contracts record, and the
 * published reference-script UTxO for the step being run. Both are derived
 * from the same deployment info, and the reference script is re-hashed against
 * the deployed script hash before it is used.
 */
const resolveStepReferenceScriptV1 = async ({
  config,
  deploymentInfo,
  name,
}: {
  readonly config: FabricatedCliContractsConfigV1;
  readonly deploymentInfo: Parameters<
    typeof requireDeploymentReferenceScript
  >[0]["deploymentInfo"];
  readonly name: string;
}): Promise<UTxO> => {
  const lucid = await makeLucidForSubmit(config);
  return await requireDeploymentReferenceScript({
    lucid,
    deploymentInfo,
    name,
  });
};

export const resolveFabricatedDepositCliContractsV1 = async ({
  config,
  stepIndex,
}: {
  readonly config: FabricatedCliContractsConfigV1;
  readonly stepIndex: FabricatedStepIndexV1;
}): Promise<{
  readonly contracts: FabricatedDepositContractsV1;
  readonly referenceScriptUtxo: UTxO;
}> => {
  const [blueprint, deploymentInfo] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
  ]);
  const resolved = await resolveFaultProofDeploymentContracts({
    blueprint,
    deploymentInfo,
    network: config.network,
    categoryName: "fabricatedDeposit",
    requireStateQueueMint: true,
    requireFraudProofSpend: true,
  });
  const chain = resolved.contracts as FabricatedDepositFaultProofContracts;
  if (resolved.stateQueuePolicyId === undefined) {
    throw new Error(
      "Deployment info resolved no stateQueueMint policy id for fabricated-deposit.",
    );
  }
  const claimRegistry = await resolveFaultProofClaimRegistryContractV1({
    blueprint,
    network: config.network,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    deploymentInfo,
  });
  const contracts: FabricatedDepositContractsV1 = {
    steps: chain.fabricatedDeposit.steps,
    computationThread: chain.computationThread,
    fraudProof: chain.fraudProof,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    claimRegistry,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    categoryId: FABRICATED_DEPOSIT_FRAUD_CATEGORY_ID_V1,
  };
  const referenceScriptUtxo = await resolveStepReferenceScriptV1({
    config,
    deploymentInfo: resolved.deploymentInfo,
    name: FABRICATED_DEPOSIT_STEP_DEPLOYMENT_NAMES_V1[stepIndex],
  });
  return { contracts, referenceScriptUtxo };
};

export const resolveFabricatedWithdrawalCliContractsV1 = async ({
  config,
  stepIndex,
}: {
  readonly config: FabricatedCliContractsConfigV1;
  readonly stepIndex: FabricatedStepIndexV1;
}): Promise<{
  readonly contracts: FabricatedWithdrawalContractsV1;
  readonly referenceScriptUtxo: UTxO;
}> => {
  const [blueprint, deploymentInfo] = await Promise.all([
    readJsonFile(config.blueprintPath),
    readJsonFile(config.deploymentInfoPath),
  ]);
  const resolved = await resolveFaultProofDeploymentContracts({
    blueprint,
    deploymentInfo,
    network: config.network,
    categoryName: "fabricatedWithdrawal",
    requireStateQueueMint: true,
    requireFraudProofSpend: true,
  });
  const chain = resolved.contracts as FabricatedWithdrawalFaultProofContracts;
  if (resolved.stateQueuePolicyId === undefined) {
    throw new Error(
      "Deployment info resolved no stateQueueMint policy id for fabricated-withdrawal.",
    );
  }
  const claimRegistry = await resolveFaultProofClaimRegistryContractV1({
    blueprint,
    network: config.network,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    deploymentInfo,
  });
  const contracts: FabricatedWithdrawalContractsV1 = {
    steps: chain.fabricatedWithdrawal.steps,
    computationThread: chain.computationThread,
    fraudProof: chain.fraudProof,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    claimRegistry,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    categoryId: FABRICATED_WITHDRAWAL_FRAUD_CATEGORY_ID_V1,
  };
  const referenceScriptUtxo = await resolveStepReferenceScriptV1({
    config,
    deploymentInfo: resolved.deploymentInfo,
    name: FABRICATED_WITHDRAWAL_STEP_DEPLOYMENT_NAMES_V1[stepIndex],
  });
  return { contracts, referenceScriptUtxo };
};
