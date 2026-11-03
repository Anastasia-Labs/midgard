import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { requireManifestString } from "@al-ft/midgard-sdk";

import { resolveHousekeepingRetentionDays } from "../database/retention-policy.js";
import { type NodeConfigDep } from "./config.js";

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
  > &
    Partial<Pick<NodeConfigDep, "RETENTION_DAYS">>,
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
  // B5: a housekeeping window shorter than the manifest's declared retention
  // is a config fault, refused here at startup rather than pruned against.
  try {
    resolveHousekeepingRetentionDays({
      configured: nodeConfig.RETENTION_DAYS,
      manifestRetentionDays: manifest.da.transportProfile.retentionDays,
    });
  } catch (cause) {
    mismatches.push(
      `da.transportProfile.retentionDays: ${cause instanceof Error ? cause.message : String(cause)}`,
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
