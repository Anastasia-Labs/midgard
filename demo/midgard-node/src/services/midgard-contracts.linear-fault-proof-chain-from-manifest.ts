import {
  DEPLOYMENT_MANIFEST_FRAUD_PROOF_CONTRACT_BY_CATEGORY,
  DEPLOYMENT_MEMBERS,
  type DeploymentContract,
  type DeploymentManifest,
  parseDeploymentManifestEventHistoryBounds,
  parseDeploymentManifestEventHistoryRetentionAddress,
} from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import type { Network } from "@lucid-evolution/lucid";

import {
  mintingValidatorFromManifest,
  spendingValidatorFromManifest,
  withdrawalValidatorFromManifest,
} from "./midgard-contracts.assert-deployment-manifest-matches-config.js";

export const faultProofMemberFromManifest = (
  network: Network,
  manifest: DeploymentManifest,
  sourcePath: string,
  contract: DeploymentContract,
): SDK.FamilyMemberValidator => {
  const member = DEPLOYMENT_MEMBERS[contract];
  if (member.purpose === "spend")
    return spendingValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      contract,
    );
  if (member.purpose === "withdraw")
    return withdrawalValidatorFromManifest(manifest, sourcePath, contract);
  return mintingValidatorFromManifest(manifest, sourcePath, contract);
};

export const faultProofFamilyScriptsFromManifest = <
  Family extends Exclude<
    keyof SDK.FaultProofContractChains,
    "validationTraceDispute"
  >,
>(
  network: Network,
  manifest: DeploymentManifest,
  sourcePath: string,
  family: Family,
): Omit<SDK.FaultProofContractChains[Family], "history"> =>
  SDK.restoreFaultProofFamilyScripts(family, (contract) =>
    faultProofMemberFromManifest(network, manifest, sourcePath, contract),
  );

export const linearFaultProofChainFromManifest = <
  Family extends Exclude<
    keyof SDK.FaultProofContractChains,
    "transitionTrace" | "validationTraceDispute"
  >,
>(
  network: Network,
  manifest: DeploymentManifest,
  sourcePath: string,
  family: Family,
): SDK.FaultProofContractChains[Family] => {
  const scripts = faultProofFamilyScriptsFromManifest(
    network,
    manifest,
    sourcePath,
    family,
  );
  if (family === "fabricatedDeposit" || family === "fabricatedWithdrawal") {
    const entry =
      manifest.contracts[
        DEPLOYMENT_MANIFEST_FRAUD_PROOF_CONTRACT_BY_CATEGORY[family]
      ];
    const bounds = parseDeploymentManifestEventHistoryBounds(
      entry.eventHistoryBounds,
    );
    return {
      ...scripts,
      history: {
        inlineLimitBytes: BigInt(bounds.inlineLimitBytes),
        maxPayloadBytes: BigInt(bounds.maxPayloadBytes),
        maxPayloadNodes: BigInt(bounds.maxPayloadNodes),
        retentionAddress: parseDeploymentManifestEventHistoryRetentionAddress(
          entry.eventHistoryRetentionAddress,
        ),
      },
    } as SDK.FaultProofContractChains[Family];
  }
  return scripts as SDK.FaultProofContractChains[Family];
};
