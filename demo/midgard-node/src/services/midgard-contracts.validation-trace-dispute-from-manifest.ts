import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { parseDeploymentManifestEventHistoryRecipe } from "@al-ft/midgard-core/deployment-manifest-identity";
import * as SDK from "@al-ft/midgard-sdk";
import { Network } from "@lucid-evolution/lucid";

import {
  authenticatedValidatorFromManifest,
  spendingValidatorFromManifest,
  withdrawalValidatorFromManifest,
} from "./midgard-contracts.assert-deployment-manifest-matches-config.js";
import { faultProofMemberFromManifest } from "./midgard-contracts.linear-fault-proof-chain-from-manifest.js";

export const eventHistoryContractsFromManifest = (
  network: Network,
  manifest: DeploymentManifest,
  sourcePath: string,
): SDK.EventHistoryContractPair => {
  const load = (name: "deposit" | "withdrawal"): SDK.EventHistoryContracts => {
    const metadata = parseDeploymentManifestEventHistoryRecipe(
      manifest.contracts[`${name}Mint`]?.eventHistoryRecipe,
    );
    const kind = name === "deposit" ? "Deposit" : "Withdrawal";
    if (
      metadata.kind !== kind ||
      metadata.hubPolicyId !== manifest.contracts.hubOracleMint.scriptHash ||
      metadata.initializationNonce.txHash !==
        manifest.hubOracleOneShot.txHash ||
      metadata.initializationNonce.outputIndex !==
        manifest.hubOracleOneShot.outputIndex
    )
      throw new Error(
        `Manifest ${name} history recipe differs from its deployment`,
      );
    const list = authenticatedValidatorFromManifest(
      network,
      manifest,
      sourcePath,
      `${name}Spend`,
      `${name}Mint`,
    );
    if (list.spendingScriptHash !== list.policyId)
      throw new Error(
        `Manifest ${name} history spending and minting roles must use the same script`,
      );
    return {
      recipe: {
        kind,
        hubPolicyId: metadata.hubPolicyId,
        initializationNonce: {
          transactionId: metadata.initializationNonce.txHash,
          outputIndex: BigInt(metadata.initializationNonce.outputIndex),
        },
        protectionDurationMs: BigInt(metadata.protectionDurationMs),
        inlineLimitBytes: BigInt(metadata.bounds.inlineLimitBytes),
        maxPayloadBytes: BigInt(metadata.bounds.maxPayloadBytes),
        maxPayloadNodes: BigInt(metadata.bounds.maxPayloadNodes),
      },
      list: {
        ...list,
        ...withdrawalValidatorFromManifest(manifest, sourcePath, `${name}Mint`),
      },
      retention: spendingValidatorFromManifest(
        network,
        manifest,
        sourcePath,
        `${name}HistoryRetentionSpend`,
      ),
      retirement: withdrawalValidatorFromManifest(
        manifest,
        sourcePath,
        `${name}HistoryRetirementWithdraw`,
      ),
    };
  };
  return { deposit: load("deposit"), withdrawal: load("withdrawal") };
};

/** Restore the recorded SDK chain using purpose-checked manifest validators. */
export const validationTraceDisputeFromManifest = (
  network: Network,
  manifest: DeploymentManifest,
  sourcePath: string,
): SDK.FaultProofContractChains["validationTraceDispute"] =>
  SDK.restoreValidationTraceDisputeScripts({
    resolve: (contract) =>
      faultProofMemberFromManifest(network, manifest, sourcePath, contract),
    missingMember: (member) => {
      throw new Error(
        `Deployment manifest at "${sourcePath}" does not record validation-trace ${member}; a manifest-sourced contract bundle cannot provide it`,
      );
    },
  });
