import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import {
  MintingPolicy,
  mintingPolicyToId,
  Network,
  type Script,
  SpendingValidator,
  validatorToAddress,
  validatorToScriptHash,
  WithdrawalValidator,
} from "@lucid-evolution/lucid";

import {
  type LegacyFaultProofFamily,
  recordedFaultProofStepContractNames,
  type RegisteredLinearFaultProofCategory,
} from "../deployable-scripts.js";
import { type NodeConfigDep } from "./config.js";
import { requireManifestString } from "./midgard-contracts.load-reference-script-auth-validator.js";

const requireManifestInteger = (
  value: unknown,
  field: string,
  sourcePath: string,
): number => {
  if (typeof value === "number" && Number.isSafeInteger(value) && value >= 0) {
    return value;
  }
  throw new Error(
    `Deployment manifest at "${sourcePath}" has invalid ${field}`,
  );
};

const isManifestScriptType = (value: string): value is Script["type"] =>
  value === "Native" ||
  value === "PlutusV1" ||
  value === "PlutusV2" ||
  value === "PlutusV3";

const requireManifestScriptType = (
  value: unknown,
  field: string,
  sourcePath: string,
): Script["type"] => {
  const scriptType = requireManifestString(value, field, sourcePath);
  if (isManifestScriptType(scriptType)) {
    return scriptType;
  }
  throw new Error(
    `Deployment manifest at "${sourcePath}" has invalid ${field}: ${scriptType}`,
  );
};

export const assertDeploymentManifestMatchesConfig = (
  manifest: DeploymentManifest,
  sourcePath: string,
  nodeConfig: Pick<
    NodeConfigDep,
    | "NETWORK"
    | "L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS"
    | "HUB_ORACLE_ONE_SHOT_TX_HASH"
    | "HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX"
    | "DEPLOYMENT_ECONOMICS_PROFILE"
    | "OPERATOR_REQUIRED_BOND_LOVELACE"
    | "OPERATOR_SLASHING_PENALTY_LOVELACE"
  >,
): void => {
  const mismatches: string[] = [];
  const manifestNetwork = requireManifestString(
    manifest.network,
    "network",
    sourcePath,
  );
  const manifestReferenceScriptAddress = requireManifestString(
    manifest.referenceScriptDeployAddress,
    "referenceScriptDeployAddress",
    sourcePath,
  );
  const manifestOneShotTxHash = requireManifestString(
    manifest.hubOracleOneShot?.txHash,
    "hubOracleOneShot.txHash",
    sourcePath,
  ).toLowerCase();
  const manifestOneShotOutputIndex = requireManifestInteger(
    manifest.hubOracleOneShot?.outputIndex,
    "hubOracleOneShot.outputIndex",
    sourcePath,
  );

  if (manifestNetwork !== nodeConfig.NETWORK) {
    mismatches.push(
      `network manifest=${manifestNetwork} config=${nodeConfig.NETWORK}`,
    );
  }
  if (
    manifestReferenceScriptAddress !==
    nodeConfig.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS
  ) {
    mismatches.push(
      `referenceScriptDeployAddress manifest=${manifestReferenceScriptAddress} config=${nodeConfig.L1_REFERENCE_SCRIPT_DEPLOY_ADDRESS}`,
    );
  }
  if (manifestOneShotTxHash !== nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH) {
    mismatches.push(
      `hubOracleOneShot.txHash manifest=${manifestOneShotTxHash} config=${nodeConfig.HUB_ORACLE_ONE_SHOT_TX_HASH}`,
    );
  }
  if (
    manifestOneShotOutputIndex !== nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX
  ) {
    mismatches.push(
      `hubOracleOneShot.outputIndex manifest=${manifestOneShotOutputIndex.toString()} config=${nodeConfig.HUB_ORACLE_ONE_SHOT_OUTPUT_INDEX.toString()}`,
    );
  }
  if (manifest.economics.profile !== nodeConfig.DEPLOYMENT_ECONOMICS_PROFILE) {
    mismatches.push(
      `economics.profile manifest=${manifest.economics.profile} config=${nodeConfig.DEPLOYMENT_ECONOMICS_PROFILE}`,
    );
  }
  if (
    BigInt(manifest.economics.requiredBondLovelace) !==
    nodeConfig.OPERATOR_REQUIRED_BOND_LOVELACE
  ) {
    mismatches.push(
      `economics.requiredBondLovelace manifest=${manifest.economics.requiredBondLovelace.toString()} config=${nodeConfig.OPERATOR_REQUIRED_BOND_LOVELACE.toString()}`,
    );
  }
  if (
    BigInt(manifest.economics.slashingPenaltyLovelace) !==
    nodeConfig.OPERATOR_SLASHING_PENALTY_LOVELACE
  ) {
    mismatches.push(
      `economics.slashingPenaltyLovelace manifest=${manifest.economics.slashingPenaltyLovelace.toString()} config=${nodeConfig.OPERATOR_SLASHING_PENALTY_LOVELACE.toString()}`,
    );
  }
  if (mismatches.length > 0) {
    throw new Error(
      `Deployment manifest at "${sourcePath}" does not match node config: ${mismatches.join(
        "; ",
      )}`,
    );
  }
};

const manifestScript = (
  manifest: DeploymentManifest,
  sourcePath: string,
  name: string,
): {
  readonly script: Script;
  readonly scriptHash: string;
  readonly cborHex: string;
} => {
  const entry = manifest.contracts?.[name];
  if (entry === undefined) {
    throw new Error(
      `Deployment manifest at "${sourcePath}" is missing contracts.${name}`,
    );
  }
  const type = requireManifestScriptType(
    entry.contract?.type,
    `contracts.${name}.contract.type`,
    sourcePath,
  );
  const cborHex = requireManifestString(
    entry.contract?.cborHex,
    `contracts.${name}.contract.cborHex`,
    sourcePath,
  );
  const scriptHash = requireManifestString(
    entry.scriptHash,
    `contracts.${name}.scriptHash`,
    sourcePath,
  ).toLowerCase();
  if (!/^[0-9a-fA-F]+$/.test(cborHex)) {
    throw new Error(
      `Deployment manifest at "${sourcePath}" has non-hex contracts.${name}.contract.cborHex`,
    );
  }
  return {
    script: {
      type,
      script: cborHex,
    },
    scriptHash,
    cborHex,
  };
};

const assertManifestScriptHash = (
  sourcePath: string,
  name: string,
  expected: string,
  actual: string,
): void => {
  if (expected !== actual) {
    throw new Error(
      `Deployment manifest at "${sourcePath}" has invalid contracts.${name}.scriptHash: expected=${expected}, derived=${actual}`,
    );
  }
};

export const mintingValidatorFromManifest = (
  manifest: DeploymentManifest,
  sourcePath: string,
  name: string,
): SDK.MintingValidator => {
  const entry = manifestScript(manifest, sourcePath, name);
  const mintingScript = entry.script as MintingPolicy;
  const policyId = mintingPolicyToId(mintingScript);
  assertManifestScriptHash(sourcePath, name, entry.scriptHash, policyId);
  return {
    mintingScriptCBOR: entry.cborHex,
    mintingScript,
    policyId,
  };
};

export const spendingValidatorFromManifest = (
  network: Network,
  manifest: DeploymentManifest,
  sourcePath: string,
  name: string,
): SDK.SpendingValidator => {
  const entry = manifestScript(manifest, sourcePath, name);
  const spendingScript = entry.script as SpendingValidator;
  const spendingScriptHash = validatorToScriptHash(spendingScript);
  assertManifestScriptHash(
    sourcePath,
    name,
    entry.scriptHash,
    spendingScriptHash,
  );
  return {
    spendingScriptCBOR: entry.cborHex,
    spendingScript,
    spendingScriptHash,
    spendingScriptAddress: validatorToAddress(network, spendingScript),
  };
};

export const withdrawalValidatorFromManifest = (
  manifest: DeploymentManifest,
  sourcePath: string,
  name: string,
): SDK.WithdrawalValidator => {
  const entry = manifestScript(manifest, sourcePath, name);
  const withdrawalScript = entry.script as WithdrawalValidator;
  const withdrawalScriptHash = validatorToScriptHash(withdrawalScript);
  assertManifestScriptHash(
    sourcePath,
    name,
    entry.scriptHash,
    withdrawalScriptHash,
  );
  return {
    withdrawalScriptCBOR: entry.cborHex,
    withdrawalScript,
    withdrawalScriptHash,
  };
};

export const authenticatedValidatorFromManifest = (
  network: Network,
  manifest: DeploymentManifest,
  sourcePath: string,
  spendName: string,
  mintName: string,
): SDK.AuthenticatedValidator => ({
  ...spendingValidatorFromManifest(network, manifest, sourcePath, spendName),
  ...mintingValidatorFromManifest(manifest, sourcePath, mintName),
});

/**
 * Every step of a fault-proof chain, restored by the manifest's own step
 * names. The step count comes from the manifest too, never from a locally
 * built bundle whose shape may differ from the deployment's.
 */
export const faultProofStepsFromManifest = (
  network: Network,
  manifest: DeploymentManifest,
  sourcePath: string,
  chain: RegisteredLinearFaultProofCategory | LegacyFaultProofFamily,
): SDK.SpendingValidator[] =>
  recordedFaultProofStepContractNames(
    chain,
    (contract) => manifest.contracts[contract] !== undefined,
  ).map((contract) =>
    spendingValidatorFromManifest(network, manifest, sourcePath, contract),
  );
