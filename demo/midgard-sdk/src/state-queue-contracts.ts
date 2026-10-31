import { deploymentValidatorTitle as deployedTitle } from "@al-ft/midgard-core/deployment-manifest-identity";
import { type L1SlotConfig } from "@al-ft/midgard-core/l1-slot-config";
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
import { l1SlotConfigParameterData } from "./fraud-proof/contracts/cek-context.js";

export const CORRECTION_LOCK_SCRIPT_TITLES = {
  spend: deployedTitle("correctionLockSpend"),
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
  mint: deployedTitle("stateQueueMint"),
  spend: deployedTitle("stateQueueSpend"),
  commitYield: deployedTitle("stateQueueCommitWithdraw"),
  unattestedTimeoutYield: deployedTitle("stateQueueUnattestedTimeoutWithdraw"),
  unavailableTimeoutYield: deployedTitle(
    "stateQueueUnavailableTimeoutWithdraw",
  ),
  fraudRemovalYield: deployedTitle("stateQueueFraudRemovalWithdraw"),
  mergeYield: deployedTitle("stateQueueMergeWithdraw"),
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
  /**
   * The deployment's L1 slot mapping (the manifest's `l1SlotConfig`). It is
   * the commit yield's last parameter: the commit rule binds the header's
   * `block_slot` to the node's `block_time` through it.
   */
  readonly l1SlotConfig: L1SlotConfig;
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
          params.fraudProofPolicyId,
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
    const l1SlotConfig = yield* tryBuild(
      "Failed to encode the L1 slot config for the state-queue commit yield",
      () => l1SlotConfigParameterData(params.l1SlotConfig),
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
          l1SlotConfig,
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
