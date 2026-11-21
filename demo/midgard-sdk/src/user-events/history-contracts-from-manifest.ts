/**
 * The contracts a user's deposit or withdrawal needs (the hub oracle and both
 * event-history lists), read from a deployment manifest's own script CBOR,
 * plus the outrefs of the two minting reference scripts. The node's full
 * contract bundle and the user CLI both build them here, so there is one copy.
 */
import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { parseDeploymentManifestEventHistoryRecipe } from "@al-ft/midgard-core/deployment-manifest-identity";
import { parseOutRefLabel } from "@al-ft/midgard-core/out-ref";
import {
  credentialToAddress,
  Network,
  type OutRef,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import { type AuthenticatedValidator } from "../common.js";
import {
  authenticatedValidatorFromManifest,
  mintingValidatorFromManifest,
  spendingValidatorFromManifest,
  withdrawalValidatorFromManifest,
} from "../deployment-manifest-validators.js";
import type * as SDK from "./history-deployment.js";
import { type UserHistoryContracts } from "./history-user.js";

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

/** The hub oracle and both event-history lists, from the manifest's scripts. */
export const userHistoryValidatorsFromManifest = (
  network: Network,
  manifest: DeploymentManifest,
  sourcePath: string,
): UserHistoryContracts => {
  const eventHistory = eventHistoryContractsFromManifest(
    network,
    manifest,
    sourcePath,
  );
  const hubOracleMint = mintingValidatorFromManifest(
    manifest,
    sourcePath,
    "hubOracleMint",
  );
  // The canonical Aiken tree ships only the one-shot hub-oracle mint policy;
  // its witness lives at that policy's script credential, so the one script
  // that can govern the address is the mint script itself.
  const hubOracle: AuthenticatedValidator = {
    spendingScriptCBOR: hubOracleMint.mintingScriptCBOR,
    spendingScript: hubOracleMint.mintingScript,
    spendingScriptHash: hubOracleMint.policyId,
    spendingScriptAddress: credentialToAddress(
      network,
      scriptHashToCredential(hubOracleMint.policyId),
    ),
    ...hubOracleMint,
  };
  return {
    hubOracle,
    eventHistory,
    deposit: eventHistory.deposit.list,
    withdrawal: eventHistory.withdrawal.list,
  };
};

/** Where the deployment published the deposit and withdrawal minting scripts. */
export type UserHistoryReferenceScripts = Readonly<{
  depositMinting: OutRef;
  withdrawalMinting: OutRef;
}>;

const mintingReferenceOutRef = (
  manifest: DeploymentManifest,
  sourcePath: string,
  role: "deposit minting" | "withdrawal minting",
  contract: "depositMint" | "withdrawalMint",
): OutRef => {
  const reference = manifest.referenceScripts?.[role];
  const scriptHash = manifest.contracts?.[contract]?.scriptHash;
  if (reference === undefined || scriptHash === undefined)
    throw new Error(
      `Deployment manifest at "${sourcePath}" does not record the ${role} reference script`,
    );
  if (reference.scriptHash.toLowerCase() !== scriptHash.toLowerCase())
    throw new Error(
      `Deployment manifest at "${sourcePath}" records a ${role} reference script whose hash is not contracts.${contract}.scriptHash`,
    );
  const { txHash, outputIndex } = parseOutRefLabel(reference.outRef);
  return { txHash, outputIndex };
};

/**
 * Everything a user's deposit or withdrawal reads from a verified deployment
 * manifest: the contracts on the manifest's own network, and the outrefs of
 * the deposit and withdrawal minting reference scripts (the caller reads
 * them by exact outref and the builder checks their script hash).
 */
export const userHistoryContractsFromDeploymentManifest = (
  manifest: DeploymentManifest,
  options: Readonly<{ sourcePath?: string }> = {},
): Readonly<{
  contracts: UserHistoryContracts;
  referenceScripts: UserHistoryReferenceScripts;
}> => {
  const sourcePath = options.sourcePath ?? `manifest ${manifest.manifestId}`;
  return {
    contracts: userHistoryValidatorsFromManifest(
      manifest.network,
      manifest,
      sourcePath,
    ),
    referenceScripts: {
      depositMinting: mintingReferenceOutRef(
        manifest,
        sourcePath,
        "deposit minting",
        "depositMint",
      ),
      withdrawalMinting: mintingReferenceOutRef(
        manifest,
        sourcePath,
        "withdrawal minting",
        "withdrawalMint",
      ),
    },
  };
};
