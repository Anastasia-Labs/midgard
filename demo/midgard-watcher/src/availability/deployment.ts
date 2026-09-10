import * as SDK from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type Script,
  type UTxO,
  validatorToAddress,
  validatorToRewardAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import {
  type VerifiedWatcherDeploymentIdentity,
  watcherDeploymentAvailabilityChallengeAuthority,
  watcherDeploymentProtocolScriptAuthority,
} from "../runtime/deployment-identity.js";

/** Resolve only immutable outrefs and scripts authenticated by the signed release. */
export const createWatcherAvailabilityDeployment = async (
  lucid: LucidEvolution,
  identity: VerifiedWatcherDeploymentIdentity,
): Promise<SDK.DaAvailabilityDeployment> => {
  const authority = watcherDeploymentProtocolScriptAuthority(identity);
  const parameters =
    watcherDeploymentAvailabilityChallengeAuthority(identity).parameters;
  const references: Record<string, UTxO> = {};
  const resolve = async (
    role: string,
    expectedHash?: string,
  ): Promise<Script> => {
    const signed = authority.referenceScripts[role];
    if (signed === undefined) throw new Error(`Signed release omitted ${role}`);
    const [txHash, outputIndex] = signed.outRef.split("#");
    const outputs = await lucid.utxosByOutRef([
      { txHash: txHash!, outputIndex: Number(outputIndex) },
    ]);
    const output = outputs[0];
    if (
      outputs.length !== 1 ||
      output?.scriptRef == null ||
      validatorToScriptHash(output.scriptRef) !== signed.scriptHash ||
      (expectedHash !== undefined && signed.scriptHash !== expectedHash) ||
      output.assets[
        SDK.referenceScriptAuthUnit(
          authority.protocolScriptHashes.referenceScriptAuthMint,
          role,
        )
      ] !== 1n
    ) {
      throw new Error(`Availability reference authority mismatch: ${role}`);
    }
    references[role] = output;
    return output.scriptRef;
  };
  const spending = async (
    role: string,
    hash: string,
  ): Promise<SDK.SpendingValidator> => {
    const script = await resolve(role, hash);
    return {
      spendingScript: script,
      spendingScriptCBOR: script.script,
      spendingScriptHash: hash,
      spendingScriptAddress: validatorToAddress(identity.network, script),
    };
  };
  const minting = async (
    role: string,
    hash: string,
  ): Promise<SDK.MintingValidator> => {
    const script = await resolve(role, hash);
    return {
      mintingScript: script,
      mintingScriptCBOR: script.script,
      policyId: hash,
    };
  };
  const withdrawal = async (
    role: string,
    hash?: string,
  ): Promise<SDK.WithdrawalValidator> => {
    const script = await resolve(role, hash);
    const account = await lucid.rewardAccountAt(
      validatorToRewardAddress(identity.network, script),
    );
    if (!account.registered)
      throw new Error(
        `Availability requires registered reward account: ${role}`,
      );
    return {
      withdrawalScript: script,
      withdrawalScriptCBOR: script.script,
      withdrawalScriptHash: validatorToScriptHash(script),
    };
  };
  const hashes = authority.protocolScriptHashes;
  const availabilityChallenge = {
    ...(await spending(
      "availability-challenge spending",
      hashes.availabilityChallengeSpend,
    )),
    ...(await minting(
      "availability-challenge minting",
      hashes.availabilityChallengeMint,
    )),
    yields: {
      bond: await withdrawal(
        "availability-challenge bond withdrawal",
        hashes.availabilityChallengeBondWithdraw,
      ),
      open: await withdrawal(
        "availability-challenge open withdrawal",
        hashes.availabilityChallengeOpenWithdraw,
      ),
      settle: await withdrawal(
        "availability-challenge settle withdrawal",
        hashes.availabilityChallengeSettleWithdraw,
      ),
      close: await withdrawal(
        "availability-challenge close withdrawal",
        hashes.availabilityChallengeCloseWithdraw,
      ),
      timeout: await withdrawal(
        "availability-challenge timeout withdrawal",
        hashes.availabilityChallengeTimeoutWithdraw,
      ),
    },
  };
  const stateQueue = {
    ...(await spending("state-queue spending", hashes.stateQueueSpend)),
    ...(await minting("state-queue minting", hashes.stateQueueMint)),
    yields: {
      commit: await withdrawal("state-queue commit withdrawal"),
      unattestedTimeout: await withdrawal(
        "state-queue unattested-timeout withdrawal",
      ),
      unavailableTimeout: await withdrawal(
        "state-queue unavailable-timeout withdrawal",
      ),
      fraudRemoval: await withdrawal("state-queue fraud-removal withdrawal"),
      merge: await withdrawal("state-queue merge withdrawal"),
    },
  };
  const correctionLock = await spending(
    "correction-lock spending",
    hashes.correctionLockSpend,
  );
  const hubOracleRefInput = await lucid.utxoByUnit(
    hashes.hubOracleMint + SDK.HUB_ORACLE_ASSET_NAME,
  );
  return {
    contracts: { availabilityChallenge, stateQueue, correctionLock },
    hubOraclePolicyId: hashes.hubOracleMint,
    referenceScriptAuthPolicyId: hashes.referenceScriptAuthMint,
    referenceScripts: Object.freeze(references),
    hubOracleRefInput,
    parameters: SDK.daAvailabilityParameters({
      responseGeometry: {
        chunk_byte_length: BigInt(parameters.responseGeometry.chunkByteLength),
        tranche_byte_length: BigInt(
          parameters.responseGeometry.trancheByteLength,
        ),
        max_tranche_count: BigInt(parameters.responseGeometry.maxTrancheCount),
      },
      daBondLovelace: BigInt(parameters.daBondLovelace),
      challengerBondLovelace: BigInt(parameters.challengerBondLovelace),
      maxOpenFeeLovelace: BigInt(parameters.maxOpenFeeLovelace),
      maxPublicationFeeLovelace: BigInt(parameters.maxPublicationFeeLovelace),
      maxSettlementFeeLovelace: BigInt(parameters.maxSettlementFeeLovelace),
      maxCloseFeeLovelace: BigInt(parameters.maxCloseFeeLovelace),
      maxTimeoutFeeLovelace: BigInt(parameters.maxTimeoutFeeLovelace),
    }),
  };
};
