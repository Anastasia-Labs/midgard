import "./runtime.fraud-proof-deployment-entries-by-category.js";

import {
  type DaHashPreimageFaultProofContracts,
  type InputNoIdxFaultProofContracts,
  type InvalidRangeFaultProofContracts,
  type InvalidSignatureFaultProofContracts,
  type NonExistentInputFaultProofContracts,
  type NoReferenceInputFaultProofContracts,
  type ReferenceInputNoIdxFaultProofContracts,
  type TransitionTraceFaultProofContracts,
  type ValidationTraceDisputeFaultProofContracts,
  type ZeroInputFaultProofContracts,
} from "@al-ft/midgard-sdk";
import {
  getAddressDetails,
  type LucidEvolution,
  type Network,
  type UTxO,
} from "@lucid-evolution/lucid";

import { parseContractDeploymentReferenceScriptAuthPolicyId } from "./inspect-contracts.js";
import { categoryLabel } from "./runtime.category-label.js";
import {
  type ResolvedDaHashPreimageDeploymentContracts,
  type ResolvedInputNoIdxDeploymentContracts,
  type ResolvedInvalidRangeDeploymentContracts,
  type ResolvedInvalidSignatureDeploymentContracts,
  type ResolvedNonExistentInputDeploymentContracts,
  type ResolvedNoReferenceInputDeploymentContracts,
  type ResolvedReferenceInputNoIdxDeploymentContracts,
  type ResolvedTransitionTraceDeploymentContracts,
  type ResolvedValidationTraceDisputeDeploymentContracts,
  type ResolvedZeroInputDeploymentContracts,
} from "./runtime.require-fault-proof-step-reference-script.js";
import { resolveFaultProofDeploymentContracts } from "./runtime.resolve-fault-proof-deployment-contracts.js";
import {
  requireDeploymentScriptHash,
  requireMatchingScriptHash,
} from "./runtime.resolve-prover-signer.js";

export const resolveNonExistentInputDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedNonExistentInputDeploymentContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "nonExistentInput",
  });
  return {
    deploymentInfo: resolved.deploymentInfo,
    nonExistentInputCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts: resolved.contracts as NonExistentInputFaultProofContracts,
  };
};

export const resolveInputNoIdxDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedInputNoIdxDeploymentContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "nonExistentInputNoIndex",
  });
  return {
    deploymentInfo: resolved.deploymentInfo,
    nonExistentInputNoIndexCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts: resolved.contracts as InputNoIdxFaultProofContracts,
  };
};

export const resolveInvalidRangeDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedInvalidRangeDeploymentContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "invalidRange",
  });
  return {
    deploymentInfo: resolved.deploymentInfo,
    invalidRangeCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts: resolved.contracts as InvalidRangeFaultProofContracts,
  };
};

export const resolveTransitionTraceDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedTransitionTraceDeploymentContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "transitionTrace",
  });
  return {
    deploymentInfo: resolved.deploymentInfo,
    transitionTraceCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts: resolved.contracts as TransitionTraceFaultProofContracts,
  };
};

export const resolveValidationTraceDisputeDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedValidationTraceDisputeDeploymentContracts> => {
  const referenceScriptAuthPolicyId =
    parseContractDeploymentReferenceScriptAuthPolicyId(
      params.deploymentInfo,
      "V1 validation-trace dispute",
    );
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "validationTraceDispute",
  });
  const contracts =
    resolved.contracts as ValidationTraceDisputeFaultProofContracts;
  const deployedCekProgramMaterialScriptHash = requireDeploymentScriptHash(
    resolved.deploymentInfo,
    "cekProgramMaterialSpend",
  );
  requireMatchingScriptHash({
    label: "cekProgramMaterialSpend script",
    deployed: deployedCekProgramMaterialScriptHash,
    derived:
      contracts.validationTraceDispute.cekProgramMaterial.spendingScriptHash,
  });
  const cekProgramMaterialAddress =
    contracts.validationTraceDispute.cekProgramMaterial.spendingScriptAddress;
  const addressCredential = getAddressDetails(
    cekProgramMaterialAddress,
  ).paymentCredential;
  if (
    addressCredential?.type !== "Script" ||
    addressCredential.hash !== deployedCekProgramMaterialScriptHash
  ) {
    throw new Error(
      `Derived CEK program-material address ${cekProgramMaterialAddress} is not locked by deployed script ${deployedCekProgramMaterialScriptHash}.`,
    );
  }
  return {
    deploymentInfo: resolved.deploymentInfo,
    referenceScriptAuthPolicyId,
    cekProgramMaterialScriptHash: deployedCekProgramMaterialScriptHash,
    cekProgramMaterialAddress,
    validationTraceDisputeCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts,
  };
};

export const resolveZeroInputDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedZeroInputDeploymentContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "zeroInput",
  });
  return {
    deploymentInfo: resolved.deploymentInfo,
    zeroInputCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts: resolved.contracts as ZeroInputFaultProofContracts,
  };
};

export const resolveDaHashPreimageDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedDaHashPreimageDeploymentContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "daHashPreimage",
  });
  return {
    deploymentInfo: resolved.deploymentInfo,
    daHashPreimageCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts: resolved.contracts as DaHashPreimageFaultProofContracts,
  };
};

export const resolveNoReferenceInputDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedNoReferenceInputDeploymentContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "noReferenceInput",
  });
  return {
    deploymentInfo: resolved.deploymentInfo,
    noReferenceInputCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts: resolved.contracts as NoReferenceInputFaultProofContracts,
  };
};

export const resolveReferenceInputNoIdxDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedReferenceInputNoIdxDeploymentContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "referenceInputNoIdx",
  });
  return {
    deploymentInfo: resolved.deploymentInfo,
    referenceInputNoIdxCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts: resolved.contracts as ReferenceInputNoIdxFaultProofContracts,
  };
};

export const resolveInvalidSignatureDeploymentContracts = async (params: {
  readonly blueprint: unknown;
  readonly deploymentInfo: unknown;
  readonly network: Network;
  readonly requireStateQueueMint?: boolean;
  readonly requireFraudProofSpend?: boolean;
}): Promise<ResolvedInvalidSignatureDeploymentContracts> => {
  const resolved = await resolveFaultProofDeploymentContracts({
    ...params,
    categoryName: "invalidSignature",
  });
  return {
    deploymentInfo: resolved.deploymentInfo,
    invalidSignatureCategory: resolved.category,
    stateQueuePolicyId: resolved.stateQueuePolicyId,
    fraudProofCataloguePolicyId: resolved.fraudProofCataloguePolicyId,
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    contracts: resolved.contracts as InvalidSignatureFaultProofContracts,
  };
};

export const faultProofCategoryLabel = categoryLabel;

export const requireSingletonUtxo = async ({
  lucid,
  address,
  unit,
  label,
}: {
  readonly lucid: LucidEvolution;
  readonly address: string;
  readonly unit: string;
  readonly label: string;
}): Promise<UTxO> => {
  const utxos = await lucid.utxosAtWithUnit(address, unit);
  const matches = utxos.filter((utxo) => (utxo.assets[unit] ?? 0n) === 1n);
  if (matches.length !== 1) {
    throw new Error(
      `Expected exactly one ${label} UTxO with unit ${unit}, found ${matches.length.toString()}.`,
    );
  }
  return matches[0]!;
};
