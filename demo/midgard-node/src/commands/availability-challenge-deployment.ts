import * as SDK from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type Network,
  type Script,
  type UTxO,
  validatorToAddress,
  validatorToRewardAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import type { DeploymentManifest } from "./contract-deployment-info.js";

/** The reference-script authentication policy of a verified manifest. */
export const manifestReferenceScriptAuthPolicy = (
  manifest: DeploymentManifest,
): string => {
  const authPolicy = manifest.contracts.referenceScriptAuthMint?.scriptHash;
  if (authPolicy === undefined)
    throw new Error("Deployment omits reference script authentication policy");
  return authPolicy;
};

/**
 * The live reference-script UTxO of manifest `role` for contract `key`: the
 * manifest's role entry must name the contract's script hash, and the UTxO it
 * points at must carry that script and the role's reference-script
 * authentication token.
 */
export const authenticatedManifestReference = async (
  lucid: LucidEvolution,
  manifest: DeploymentManifest,
  authPolicy: string,
  key: string,
  role: string,
): Promise<UTxO & { readonly scriptRef: Script }> => {
  const contract = manifest.contracts[key];
  const reference = manifest.referenceScripts[role];
  if (
    contract === undefined ||
    reference === undefined ||
    reference.scriptHash !== contract.scriptHash
  )
    throw new Error(`Deployment omits coherent availability reference ${role}`);
  const [txHash, outputIndex] = reference.outRef.split("#");
  const utxos = await lucid.utxosByOutRef([
    { txHash: txHash!, outputIndex: Number(outputIndex) },
  ]);
  const utxo = utxos[0];
  if (
    utxos.length !== 1 ||
    !utxo?.scriptRef ||
    validatorToScriptHash(utxo.scriptRef) !== contract.scriptHash ||
    utxo.assets[SDK.referenceScriptAuthUnit(authPolicy, role)] !== 1n
  ) {
    throw new Error(`Missing or unauthenticated deployment reference ${role}`);
  }
  return { ...utxo, scriptRef: utxo.scriptRef };
};

/** The DA availability parameters a verified manifest records. */
export const availabilityParametersFromManifest = (
  manifest: DeploymentManifest,
): SDK.DaAvailabilityParameters => {
  const p = manifest.availabilityChallenge;
  return SDK.daAvailabilityParameters({
    responseGeometry: SDK.availabilityResponseGeometry(p.responseGeometry),
    daBondLovelace: BigInt(p.daBondLovelace),
    daSlashPenaltyLovelace: BigInt(p.daSlashPenaltyLovelace),
    daBondMinTopUpLovelace: BigInt(p.daBondMinTopUpLovelace),
    daBondPoolFloorLovelace: BigInt(p.daBondPoolFloorLovelace),
    challengeRecordLovelace: BigInt(p.challengeRecordLovelace),
    challengerBondLovelace: BigInt(p.challengerBondLovelace),
    maxOpenFeeLovelace: BigInt(p.maxOpenFeeLovelace),
    maxPublicationFeeLovelace: BigInt(p.maxPublicationFeeLovelace),
    maxSettlementFeeLovelace: BigInt(p.maxSettlementFeeLovelace),
    maxCloseFeeLovelace: BigInt(p.maxCloseFeeLovelace),
    maxTimeoutFeeLovelace: BigInt(p.maxTimeoutFeeLovelace),
  });
};

/** A spending validator view of a reference script. */
export const spendingValidatorOf = (
  network: Network,
  value: Script,
): SDK.SpendingValidator => ({
  spendingScript: value,
  spendingScriptCBOR: value.script,
  spendingScriptHash: validatorToScriptHash(value),
  spendingScriptAddress: validatorToAddress(network, value),
});

/** A minting validator view of a reference script. */
export const mintingValidatorOf = (value: Script): SDK.MintingValidator => ({
  mintingScript: value,
  mintingScriptCBOR: value.script,
  policyId: validatorToScriptHash(value),
});

/** Build operational authority only from the verified manifest's live role outputs. */
export const availabilityDeploymentFromManifest = async (
  lucid: LucidEvolution,
  manifest: DeploymentManifest,
): Promise<SDK.DaAvailabilityDeployment> => {
  const network = lucid.config().network;
  if (network === undefined || network !== manifest.network)
    throw new Error(
      "Availability command network differs from the verified deployment",
    );
  const authPolicy = manifestReferenceScriptAuthPolicy(manifest);
  const references: Record<string, UTxO> = {};
  const script = async (key: string, role: string): Promise<Script> => {
    const utxo = await authenticatedManifestReference(
      lucid,
      manifest,
      authPolicy,
      key,
      role,
    );
    references[role] = utxo;
    return utxo.scriptRef;
  };
  const spend = async (
    key: string,
    role: string,
  ): Promise<SDK.SpendingValidator> =>
    spendingValidatorOf(network, await script(key, role));
  const mint = async (
    key: string,
    role: string,
  ): Promise<SDK.MintingValidator> =>
    mintingValidatorOf(await script(key, role));
  const withdrawal = async (
    key: string,
    role: string,
  ): Promise<SDK.WithdrawalValidator> => {
    const value = await script(key, role);
    if (
      !(await lucid.rewardAccountAt(validatorToRewardAddress(network, value)))
        .registered
    )
      throw new Error(`Unregistered availability command withdrawal ${role}`);
    return {
      withdrawalScript: value,
      withdrawalScriptCBOR: value.script,
      withdrawalScriptHash: validatorToScriptHash(value),
    };
  };
  const availabilityChallenge: SDK.AvailabilityChallengeValidator = {
    ...(await spend(
      "availabilityChallengeSpend",
      "availability-challenge spending",
    )),
    ...(await mint(
      "availabilityChallengeMint",
      "availability-challenge minting",
    )),
    yields: {
      open: await withdrawal(
        "availabilityChallengeOpenWithdraw",
        "availability-challenge open withdrawal",
      ),
      settle: await withdrawal(
        "availabilityChallengeSettleWithdraw",
        "availability-challenge settle withdrawal",
      ),
      close: await withdrawal(
        "availabilityChallengeCloseWithdraw",
        "availability-challenge close withdrawal",
      ),
      timeout: await withdrawal(
        "availabilityChallengeTimeoutWithdraw",
        "availability-challenge timeout withdrawal",
      ),
    },
  };
  const stateQueue: SDK.StateQueueValidator = {
    ...(await spend("stateQueueSpend", "state-queue spending")),
    ...(await mint("stateQueueMint", "state-queue minting")),
    yields: {
      commit: await withdrawal(
        "stateQueueCommitWithdraw",
        "state-queue commit withdrawal",
      ),
      merge: await withdrawal(
        "stateQueueMergeWithdraw",
        "state-queue merge withdrawal",
      ),
      fraudRemoval: await withdrawal(
        "stateQueueFraudRemovalWithdraw",
        "state-queue fraud-removal withdrawal",
      ),
      unattestedTimeout: await withdrawal(
        "stateQueueUnattestedTimeoutWithdraw",
        "state-queue unattested-timeout withdrawal",
      ),
      unavailableTimeout: await withdrawal(
        "stateQueueUnavailableTimeoutWithdraw",
        "state-queue unavailable-timeout withdrawal",
      ),
    },
  };
  const correctionLock = await spend(
    "correctionLockSpend",
    "correction-lock spending",
  );
  const daBondPool: SDK.AuthenticatedValidator = {
    ...(await spend("daBondPoolSpend", "da-bond-pool spending")),
    ...(await mint("daBondPoolMint", "da-bond-pool minting")),
  };
  const hubOraclePolicyId = manifest.contracts.hubOracleMint?.scriptHash;
  if (hubOraclePolicyId === undefined)
    throw new Error("Deployment omits hub oracle policy");
  return {
    contracts: {
      availabilityChallenge,
      stateQueue,
      correctionLock,
      daBondPool,
    },
    hubOraclePolicyId,
    referenceScriptAuthPolicyId: authPolicy,
    referenceScripts: references,
    hubOracleRefInput: await lucid.utxoByUnit(
      hubOraclePolicyId + SDK.HUB_ORACLE_ASSET_NAME,
    ),
    parameters: availabilityParametersFromManifest(manifest),
  };
};
