import { Address, Data, Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  AddressData,
  addressDataFromBech32,
  SpendingValidator,
  StateQueueValidator,
  WithdrawalValidator,
} from "./common.js";
import {
  applyBlueprintParams,
  type FaultProofBlueprint,
  makeMintingPolicy,
  makeSpendingValidator,
  makeWithdrawalValidator,
  tryBuild,
} from "./fraud-proof/contracts/blueprint.js";

export const CORRECTION_LOCK_SCRIPT_TITLES = {
  spend: "correction_lock.spend.spend",
} as const;

export type BuildCorrectionLockValidatorParams = {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly availabilityChallengePolicyId: string;
};

/** Apply the correction lock's trusted deployment parameters. */
export const buildCorrectionLockValidator = (
  params: BuildCorrectionLockValidatorParams,
): Effect.Effect<SpendingValidator, Error> =>
  tryBuild("Failed to build correction-lock validator", () =>
    makeSpendingValidator(
      params.network,
      applyBlueprintParams(
        params.blueprint,
        CORRECTION_LOCK_SCRIPT_TITLES.spend,
        [params.hubOraclePolicyId, params.availabilityChallengePolicyId],
      ),
    ),
  );

export const STATE_QUEUE_SCRIPT_TITLES = {
  mint: "state_queue.mint.mint",
  spend: "state_queue.spend.spend",
  commitYield: "state_queue_yields.commit.withdraw",
  unattestedTimeoutYield: "state_queue_yields.remove_unattested.withdraw",
  unavailableTimeoutYield: "state_queue_yields.remove_unavailable.withdraw",
  fraudRemovalYield: "state_queue_yields.remove_fraudulent.withdraw",
  mergeYield: "state_queue_yields.merge.withdraw",
} as const;

export type BuildStateQueueValidatorParams = {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly correctionLockScriptHash: string;
  readonly activeOperatorsPolicyId: string;
  readonly activeOperatorsAddress: Address;
  readonly retiredOperatorsPolicyId: string;
  readonly schedulerPolicyId: string;
  readonly fraudProofPolicyId: string;
  readonly settlementPolicyId: string;
  readonly daAttestationPolicyId: string;
  readonly availabilityChallengePolicyId: string;
  readonly referenceScriptAuthPolicyId: string;
};

/** Build the authenticated state queue and every associated yield from one deployment. */
export const buildStateQueueValidator = (
  params: BuildStateQueueValidatorParams,
): Effect.Effect<StateQueueValidator, Error> =>
  Effect.gen(function* () {
    const activeOperatorsAddress = yield* Effect.mapError(
      Effect.map(
        addressDataFromBech32(params.activeOperatorsAddress),
        (addressData) => Data.from(Data.to(addressData, AddressData)),
      ),
      (cause) =>
        new Error(
          `Failed to encode active-operators address for state_queue mint parameters: ${String(cause)}`,
        ),
    );
    const { blueprint, network, referenceScriptAuthPolicyId } = params;
    const mintParameters = [
      params.hubOraclePolicyId,
      params.correctionLockScriptHash,
      params.activeOperatorsPolicyId,
      activeOperatorsAddress,
      params.retiredOperatorsPolicyId,
      params.schedulerPolicyId,
      params.fraudProofPolicyId,
      params.settlementPolicyId,
      params.daAttestationPolicyId,
      params.availabilityChallengePolicyId,
      referenceScriptAuthPolicyId,
    ] as const;
    const mintingScriptCBOR = yield* tryBuild(
      "Failed to build state-queue mint validator",
      () =>
        applyBlueprintParams(
          blueprint,
          STATE_QUEUE_SCRIPT_TITLES.mint,
          mintParameters,
        ),
    );
    const minting = makeMintingPolicy(mintingScriptCBOR);
    const spendingScriptCBOR = yield* tryBuild(
      "Failed to build state-queue spend validator",
      () =>
        applyBlueprintParams(blueprint, STATE_QUEUE_SCRIPT_TITLES.spend, [
          minting.policyId,
          params.daAttestationPolicyId,
          params.availabilityChallengePolicyId,
        ]),
    );
    const buildYield = (
      title: string,
      parameters: readonly Data[],
    ): Effect.Effect<WithdrawalValidator, Error> =>
      tryBuild(`Failed to build state-queue yield "${title}"`, () =>
        makeWithdrawalValidator(
          applyBlueprintParams(blueprint, title, parameters),
        ),
      );
    return {
      ...minting,
      ...makeSpendingValidator(network, spendingScriptCBOR),
      yields: {
        commit: yield* buildYield(STATE_QUEUE_SCRIPT_TITLES.commitYield, [
          minting.policyId,
          params.hubOraclePolicyId,
          params.correctionLockScriptHash,
          params.activeOperatorsPolicyId,
          activeOperatorsAddress,
          params.schedulerPolicyId,
          params.daAttestationPolicyId,
        ]),
        unattestedTimeout: yield* buildYield(
          STATE_QUEUE_SCRIPT_TITLES.unattestedTimeoutYield,
          [
            minting.policyId,
            params.hubOraclePolicyId,
            params.correctionLockScriptHash,
          ],
        ),
        unavailableTimeout: yield* buildYield(
          STATE_QUEUE_SCRIPT_TITLES.unavailableTimeoutYield,
          [
            minting.policyId,
            params.hubOraclePolicyId,
            params.correctionLockScriptHash,
            params.availabilityChallengePolicyId,
          ],
        ),
        fraudRemoval: yield* buildYield(
          STATE_QUEUE_SCRIPT_TITLES.fraudRemovalYield,
          [
            minting.policyId,
            params.hubOraclePolicyId,
            params.correctionLockScriptHash,
            params.activeOperatorsPolicyId,
            params.retiredOperatorsPolicyId,
            params.fraudProofPolicyId,
          ],
        ),
        merge: yield* buildYield(STATE_QUEUE_SCRIPT_TITLES.mergeYield, [
          minting.policyId,
          params.hubOraclePolicyId,
          params.correctionLockScriptHash,
          params.settlementPolicyId,
          params.daAttestationPolicyId,
        ]),
      },
    };
  });
