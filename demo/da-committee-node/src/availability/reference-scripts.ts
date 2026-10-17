import { referenceScriptAuthUnit } from "@al-ft/midgard-sdk";
import {
  type LucidEvolution,
  type UTxO,
  validatorToRewardAddress,
  validatorToScriptHash,
} from "@lucid-evolution/lucid";

import type {
  MidgardDeploymentContract,
  MidgardNodeDeployment,
} from "../l1/deployment.js";

/** Resolve the release's exact role outrefs before permitting responder actions. */
export const availabilityResponderReferenceScripts = async (
  lucid: Pick<LucidEvolution, "utxosByOutRef" | "rewardAccountAt" | "config">,
  deployment: MidgardNodeDeployment,
): Promise<Readonly<Record<string, UTxO>>> => {
  const targets: readonly [string, MidgardDeploymentContract][] = [
    ["availability-challenge minting", deployment.availabilityChallenge.mint],
    ["availability-challenge spending", deployment.availabilityChallenge.spend],
    ...Object.entries(deployment.availabilityChallengeYields).map(
      ([arm, contract]): [string, MidgardDeploymentContract] => [
        `availability-challenge ${arm} withdrawal`,
        contract,
      ],
    ),
    ["state-queue minting", deployment.stateQueue.mint],
    ["state-queue spending", deployment.stateQueue.spend],
  ];
  const references = targets.map(([name, contract]) => {
    if (contract.refScriptOutRef === null)
      throw new Error(`Missing deployed ${name} reference outref`);
    return contract.refScriptOutRef;
  });
  const live = await lucid.utxosByOutRef(references);
  const result: Record<string, UTxO> = {};
  for (const [name, contract] of targets) {
    const utxo = live.find(
      (item) =>
        item.txHash === contract.refScriptOutRef?.txHash &&
        item.outputIndex === contract.refScriptOutRef.outputIndex,
    );
    if (
      utxo === undefined ||
      !utxo.scriptRef ||
      validatorToScriptHash(utxo.scriptRef) !== contract.scriptHash ||
      utxo.assets[
        referenceScriptAuthUnit(deployment.referenceScriptAuthPolicyId, name)
      ] !== 1n
    ) {
      throw new Error(`Missing or unauthenticated ${name} reference script`);
    }
    if (contract.purpose === "withdraw") {
      const network = lucid.config().network;
      if (network === undefined)
        throw new Error(
          "Availability responder requires a configured L1 network",
        );
      const address = validatorToRewardAddress(network, contract.script);
      if (!(await lucid.rewardAccountAt(address)).registered) {
        throw new Error(`Unregistered ${name} reward account`);
      }
    }
    result[name] = utxo;
  }
  return result;
};
