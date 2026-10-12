import type { DeploymentManifest } from "@al-ft/midgard-core/deployment-manifest-identity";
import { credentialToAddress } from "@lucid-evolution/lucid";

import { l1ProviderFailoverEnabled } from "../environment.js";
import { parseReleaseL1FinalityPolicy } from "./e2e-release-finality-policy.js";
import { assertAtOrAfter } from "./e2e-state-correction-local-authority.assert-at-or-after.js";
import { createLocalKupmiosStateCorrectionSource } from "./e2e-state-correction-local-authority.create-local-kupmios-state-correction-source.js";
import {
  aggregateOutputValues,
  assertLoopbackEndpoint,
  HEX_28,
  type LocalKupmiosStateCorrectionAuthorityConfig,
  outputValue,
  stateCorrectionValueDigest,
} from "./e2e-state-correction-local-authority.fetch-json.js";
import type { StateCorrectionIndependentAuthority } from "./e2e-state-correction-reconciliation.js";

export const createLocalKupmiosStateCorrectionAuthority = (
  config: LocalKupmiosStateCorrectionAuthorityConfig,
): StateCorrectionIndependentAuthority => {
  if (l1ProviderFailoverEnabled(config.providerFailover)) {
    throw new Error("Q57 authority forbids L1 provider failover");
  }
  assertLoopbackEndpoint(config.kupoUrl, "the local Kupo endpoint (KUPO_PORT)");
  assertLoopbackEndpoint(
    config.ogmiosUrl,
    "the local Ogmios endpoint (OGMIOS_PORT)",
  );
  if (!HEX_28.test(config.stateQueuePolicyId)) {
    throw new Error("Q57 authority state-queue policy id is invalid");
  }
  const finalityPolicy = parseReleaseL1FinalityPolicy(
    config.finalityPolicy,
    "Q57 deployment manifest l1Finality",
  );
  const source =
    config.source ?? createLocalKupmiosStateCorrectionSource(config);
  return {
    authenticateTransaction: async (input) => {
      const live = await source.observeTransaction({
        txHash: input.txHash,
        outputIndex: input.kupoOutputIndex,
        expectedIncludedAt: input.includedAt,
      });
      if (
        live.kupoIncludedAt.slot !== input.includedAt.slot ||
        live.kupoIncludedAt.blockHash !== input.includedAt.blockHash
      ) {
        throw new Error(
          `live Kupo inclusion disagrees for transaction ${input.txHash}`,
        );
      }
      if (
        live.ogmiosIncludedAt === null ||
        live.ogmiosIncludedAt.slot !== live.kupoIncludedAt.slot ||
        live.ogmiosIncludedAt.blockHash !== live.kupoIncludedAt.blockHash
      ) {
        throw new Error(
          `live Kupo/Ogmios inclusion disagreement for transaction ${input.txHash}`,
        );
      }
      assertAtOrAfter(
        live.liveTip,
        input.observedAtTip,
        `transaction ${input.txHash} live tip`,
      );
      if (
        input.observedAtTip.confirmationDepth <
          finalityPolicy.confirmationDepth ||
        live.confirmationDepth < finalityPolicy.confirmationDepth
      ) {
        throw new Error(
          `transaction ${input.txHash} has confirmation depth ${live.confirmationDepth.toString()}, below release depth ${finalityPolicy.confirmationDepth.toString()}`,
        );
      }
    },
    authenticateFinalState: async (input) => {
      if (input.manifestId !== config.manifestId) {
        throw new Error("Q57 authority manifest identity mismatch");
      }
      if (
        input.observedAt.confirmationDepth < finalityPolicy.confirmationDepth
      ) {
        throw new Error(
          `final Q57 observation depth is below release depth ${finalityPolicy.confirmationDepth.toString()}`,
        );
      }
      const [
        tip,
        queue,
        database,
        tokens,
        bondInputs,
        economicTransactions,
        payoutTransaction,
        reserveOutputs,
      ] = await Promise.all([
        source.observeTip(),
        source.observeStateQueue({
          address: config.stateQueueAddress,
          policyId: config.stateQueuePolicyId,
        }),
        source.observeDatabase(),
        Promise.all(
          input.retainedProofTokens.map(async ({ outRef }) => {
            const [txHash, outputIndexText] = outRef.split("#");
            return await source.observeOutput({
              txHash: txHash!,
              outputIndex: Number(outputIndexText),
            });
          }),
        ),
        Promise.all(
          input.economics.map(async ({ operatorBondInputOutRef }) => {
            if (operatorBondInputOutRef === null) return null;
            const [txHash, outputIndexText] =
              operatorBondInputOutRef.split("#");
            return await source.observeOutput({
              txHash: txHash!,
              outputIndex: Number(outputIndexText),
            });
          }),
        ),
        Promise.all(
          input.economics.map(
            async (economics) =>
              await source.observeEconomicTransaction({
                txHash: economics.removalTxHash,
                outputIndex: economics.kupoOutputIndex,
                includedAt: economics.includedAt,
              }),
          ),
        ),
        source.observeEconomicTransaction({
          txHash: input.withdrawalReservePayout.payoutConcludeTxHash,
          outputIndex: input.withdrawalReservePayout.kupoOutputIndex,
          includedAt: input.withdrawalReservePayout.includedAt,
        }),
        source.observeUnspentAddress({ address: config.reserveAddress }),
      ]);
      assertAtOrAfter(tip, input.observedAt, "final Q57 live tip");
      if (queue.depth !== input.stateQueueDepth) {
        throw new Error(
          `live Kupo state-queue depth mismatch: expected ${input.stateQueueDepth.toString()}, found ${queue.depth.toString()}`,
        );
      }
      if (
        database.unfinishedMutationJobs !== input.unfinishedMutationJobs ||
        database.pendingFinalizations !== input.pendingFinalizations
      ) {
        throw new Error("live node database final state disagrees");
      }
      for (const [index, expected] of input.retainedProofTokens.entries()) {
        const token = tokens[index];
        const [txHash, outputIndexText] = expected.outRef.split("#");
        if (
          token === null ||
          token === undefined ||
          token.txHash !== txHash ||
          token.outputIndex !== Number(outputIndexText) ||
          token.spent ||
          token.assets[expected.unit] !== "1"
        ) {
          throw new Error(
            `live Kupo does not retain permanent proof token ${expected.unit}@${expected.outRef}`,
          );
        }
      }
      for (const [index, expected] of input.economics.entries()) {
        const transaction = economicTransactions[index]!;
        const bondInput = bondInputs[index];
        if (transaction.feeLovelace !== expected.removalFeeLovelace) {
          throw new Error(
            `live Ogmios fee does not equal the exact removal fee for ${expected.familyId}`,
          );
        }
        const requiredBond = BigInt(
          config.economicsPolicy.requiredBondLovelace,
        );
        const fullSlash = BigInt(
          config.economicsPolicy.slashingPenaltyLovelace,
        );
        const reward = BigInt(config.economicsPolicy.fraudProverRewardLovelace);
        const inactivitySlash = BigInt(
          config.economicsPolicy.inactivitySlashingPenaltyLovelace,
        );
        const observedBond = BigInt(expected.operatorBondInputLovelace);
        const observedSlash = BigInt(expected.slashedLovelace);
        const fullTranche =
          observedBond === requiredBond && observedSlash === fullSlash;
        const partiallyInactivitySlashedTranche =
          observedBond === requiredBond - inactivitySlash &&
          observedSlash === fullSlash - inactivitySlash;
        if (!fullTranche && !partiallyInactivitySlashedTranche) {
          throw new Error(
            `live bond and slash do not match a release-bound full or partially inactivity-slashed tranche for ${expected.familyId}`,
          );
        }
        if (
          expected.slashedLovelace !== expected.removalFeeLovelace ||
          BigInt(expected.proverRewardLovelace) !== reward
        ) {
          throw new Error(
            `live slash fee and prover reward do not match release economics for ${expected.familyId}`,
          );
        }
        if (
          (expected.operatorBondInputOutRef === null) !==
            (expected.operatorBondInputLovelace === "0") ||
          (expected.proverRewardOutputOutRef === null) !==
            (expected.proverRewardLovelace === "0")
        ) {
          throw new Error(
            `Q57 economic outref/amount nullability mismatch for ${expected.familyId}`,
          );
        }
        if (
          expected.operatorBondInputOutRef === null ||
          bondInput === null ||
          bondInput === undefined ||
          !transaction.inputs.includes(expected.operatorBondInputOutRef) ||
          bondInput.lovelace !== expected.operatorBondInputLovelace ||
          bondInput.address !==
            credentialToAddress("Preprod", {
              type: "Key",
              hash: expected.operatorCredential,
            })
        ) {
          throw new Error(
            `live removal omitted or substituted the exact operator bond input for ${expected.familyId}`,
          );
        }
        if (
          !transaction.referenceInputs.includes(
            expected.referencedProofTokenOutRef,
          )
        ) {
          throw new Error(
            `live removal omitted permanent proof-token reference for ${expected.familyId}`,
          );
        }
        if (BigInt(expected.proverRewardLovelace) <= 0n) {
          throw new Error(
            `Q57 requires non-zero launch economics for ${expected.familyId}`,
          );
        }
        const proverAddress = credentialToAddress("Preprod", {
          type: "Key",
          hash: expected.proverCredential,
        });
        const rewardIndex =
          expected.proverRewardOutputOutRef === null
            ? -1
            : Number(expected.proverRewardOutputOutRef.split("#")[1]);
        if (
          expected.proverRewardOutputOutRef === null ||
          !expected.proverRewardOutputOutRef.startsWith(
            `${expected.removalTxHash}#`,
          )
        ) {
          throw new Error(
            `live removal has no exact prover reward output for ${expected.familyId}`,
          );
        }
        const rewards = transaction.outputs
          .map((output, outputIndex) => ({ output, outputIndex }))
          .filter(
            ({ output }) =>
              output.address === proverAddress &&
              output.lovelace === expected.proverRewardLovelace &&
              Object.keys(output.assets).length === 0,
          );
        if (rewards.length !== 1 || rewards[0]!.outputIndex !== rewardIndex) {
          throw new Error(
            `live removal has ${rewards.length.toString()} exact prover-reward outputs for ${expected.familyId}`,
          );
        }
      }
      const payout = input.withdrawalReservePayout;
      const payoutOutputs = payoutTransaction.outputs.filter(
        (output) =>
          output.address === payout.destination &&
          stateCorrectionValueDigest(outputValue(output)) ===
            payout.payoutValueSha256,
      );
      if (payoutOutputs.length !== 1) {
        throw new Error(
          "live payout conclusion does not contain one exact destination/value output",
        );
      }
      const reserveDigest = stateCorrectionValueDigest(
        aggregateOutputValues(reserveOutputs),
      );
      if (reserveDigest !== payout.reserveValueSha256) {
        throw new Error("live Kupo reserve value does not match Q57 evidence");
      }
    },
  };
};

export const requireManifestContract = (
  manifest: DeploymentManifest,
  name: string,
): DeploymentManifest["contracts"][string] => {
  const contract = manifest.contracts[name];
  if (contract === undefined) {
    throw new Error(`deployment manifest has no ${name} contract`);
  }
  return contract;
};
