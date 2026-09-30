import * as SDK from "@al-ft/midgard-sdk";
import { credentialToAddress } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  network,
  readBlueprint,
  realBlueprintPath,
} from "./support/emulator/blueprints.js";

// Seed an authenticated chain snapshot, then execute the real, fully applied
// mint/spend/withdrawal/lock validators. Unrelated protocols are never spent.
export const policy = (byte: string) => byte.repeat(28);

export const hubPolicy = policy("11");

export const unrelatedPolicy = policy("22");

const availabilityPolicy = policy("33");

export const referencePolicy = policy("44");

export const referenceAddress = credentialToAddress(network, {
  type: "Script",
  hash: referencePolicy,
});

const blueprint = SDK.parseFaultProofBlueprint(
  readBlueprint(realBlueprintPath),
);

const buildContracts = () =>
  Effect.runPromise(
    Effect.gen(function* () {
      const lock = yield* SDK.buildCorrectionLockValidator({
        blueprint,
        network,
        hubOraclePolicyId: hubPolicy,
        availabilityChallengePolicyId: availabilityPolicy,
      });
      const queue = yield* SDK.buildStateQueueValidator({
        blueprint,
        network,
        hubOraclePolicyId: hubPolicy,
        correctionLockScriptHash: lock.spendingScriptHash,
        activeOperatorsPolicyId: unrelatedPolicy,
        activeOperatorsAddress: referenceAddress,
        retiredOperatorsPolicyId: unrelatedPolicy,
        schedulerPolicyId: unrelatedPolicy,
        fraudProofPolicyId: unrelatedPolicy,
        settlementPolicyId: unrelatedPolicy,
        daAttestationPolicyId: unrelatedPolicy,
        availabilityChallengePolicyId: availabilityPolicy,
        referenceScriptAuthPolicyId: referencePolicy,
      });
      return {
        mint: queue.mintingScript,
        spend: queue.spendingScript,
        lock: lock.spendingScript,
        withdrawal: queue.yields.unattestedTimeout.withdrawalScript,
        stateQueuePolicyId: queue.policyId,
      };
    }),
  );

export const contractsPromise = buildContracts();

export const rent = 30_000_000n;

export const key = (hash: string) => ({ Key: { key: hash } }) as const;

export const nodeUnit = (queuePolicy: string, hash: string) =>
  queuePolicy + SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + hash;

export type SetupOptions = {
  /**
   * Start the emulator's slot grid on the whole second the headers are built
   * from and end every header 1 ms before a slot boundary, as live block
   * windows do (they end at ...999 ms). The emulator is then left near the
   * target's deadline instead of far past it.
   */
  readonly liveBlockEndTimes?: boolean;
};
