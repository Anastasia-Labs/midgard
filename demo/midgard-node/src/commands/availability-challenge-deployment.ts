import * as SDK from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type Script,
  type UTxO,
  validatorToAddress,
  validatorToRewardAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import type { DeploymentManifest } from "./contract-deployment-info.js";

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
  const authPolicy = manifest.contracts.referenceScriptAuthMint?.scriptHash;
  if (authPolicy === undefined)
    throw new Error("Deployment omits reference script authentication policy");
  const references: Record<string, UTxO> = {};
  const script = async (key: string, role: string): Promise<Script> => {
    const contract = manifest.contracts[key];
    const reference = manifest.referenceScripts[role];
    if (
      contract === undefined ||
      reference === undefined ||
      reference.scriptHash !== contract.scriptHash
    )
      throw new Error(
        `Deployment omits coherent availability reference ${role}`,
      );
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
      throw new Error(
        `Missing or unauthenticated deployment reference ${role}`,
      );
    }
    references[role] = utxo;
    return utxo.scriptRef;
  };
  const spend = async (
    key: string,
    role: string,
  ): Promise<SDK.SpendingValidator> => {
    const value = await script(key, role);
    return {
      spendingScript: value,
      spendingScriptCBOR: value.script,
      spendingScriptHash: validatorToScriptHash(value),
      spendingScriptAddress: validatorToAddress(network, value),
    };
  };
  const mint = async (
    key: string,
    role: string,
  ): Promise<SDK.MintingValidator> => {
    const value = await script(key, role);
    return {
      mintingScript: value,
      mintingScriptCBOR: value.script,
      policyId: validatorToScriptHash(value),
    };
  };
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
      bond: await withdrawal(
        "availabilityChallengeBondWithdraw",
        "availability-challenge bond withdrawal",
      ),
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
  const hubOraclePolicyId = manifest.contracts.hubOracleMint?.scriptHash;
  if (hubOraclePolicyId === undefined)
    throw new Error("Deployment omits hub oracle policy");
  const p = manifest.availabilityChallenge;
  return {
    contracts: { availabilityChallenge, stateQueue, correctionLock },
    hubOraclePolicyId,
    referenceScriptAuthPolicyId: authPolicy,
    referenceScripts: references,
    hubOracleRefInput: await lucid.utxoByUnit(
      hubOraclePolicyId + SDK.HUB_ORACLE_ASSET_NAME,
    ),
    parameters: SDK.daAvailabilityParameters({
      responseGeometry: SDK.availabilityResponseGeometry(p.responseGeometry),
      daBondLovelace: BigInt(p.daBondLovelace),
      challengerBondLovelace: BigInt(p.challengerBondLovelace),
      maxOpenFeeLovelace: BigInt(p.maxOpenFeeLovelace),
      maxPublicationFeeLovelace: BigInt(p.maxPublicationFeeLovelace),
      maxSettlementFeeLovelace: BigInt(p.maxSettlementFeeLovelace),
      maxCloseFeeLovelace: BigInt(p.maxCloseFeeLovelace),
      maxTimeoutFeeLovelace: BigInt(p.maxTimeoutFeeLovelace),
    }),
  };
};
