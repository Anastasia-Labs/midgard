import {
  Assets,
  type BuildTxWithRedeemer,
  Data,
  fromUnit,
  PolicyId,
  TxBuilder,
  UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { LinkedListError, NodeKey } from "./linked-list.js";
import {
  type EmulatorStateQueueRemoveSlashingParams,
  type FraudProverRewardPlan,
  getConfirmedStateFromStateQueueDatum,
  type StateQueueRemoveReferenceScriptUTxOs,
} from "./state-queue.emulator-state-queue-commit-block-header-params.js";
import {
  encodeActiveOperatorSpendRedeemer,
  SlashingApproach,
  type StateQueueUTxO,
} from "./state-queue.state-queue-redeemer-schema.js";
import {
  requireMintRedeemerIndex,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "./tx-context-redeemer.js";

export const findLinkStateQueueUTxO = (
  link: NodeKey,
  utxos: StateQueueUTxO[],
): Effect.Effect<StateQueueUTxO, LinkedListError> => {
  const errorMessage = `Failed to find link state queue UTxO`;
  if (link === "Empty") {
    return Effect.fail(
      new LinkedListError({
        message: errorMessage,
        cause: `Given link is "Empty"`,
      }),
    );
  }

  const foundLink = utxos.find(
    (u: StateQueueUTxO) =>
      u.datum.key !== "Empty" && u.datum.key.Key.key === link.Key.key,
  );
  if (foundLink) {
    return Effect.succeed(foundLink);
  }

  return Effect.fail(
    new LinkedListError({
      message: errorMessage,
      cause: `Link not found among given state queue UTxOs`,
    }),
  );
};

/**
 * Returns a sorted array of `StateQueueUTxO`s where the confirmed state's UTxO
 * is the head element, and the following elements are linked from their
 * previous elements.
 *
 * TODO: Make it more efficient. Currently that same list of all state queue
 *       UTxOs is traversed to find the next link UTxO multiple times. It might
 *       be better to drop link UTxOs when found so that subsequent lookups
 *       become cheaper.
 */
export const sortStateQueueUTxOs = (
  stateQueueUTxOs: StateQueueUTxO[],
): Effect.Effect<StateQueueUTxO[], LinkedListError> =>
  Effect.gen(function* () {
    const filteredForConfirmedState = yield* Effect.allSuccesses(
      stateQueueUTxOs.map((u) =>
        Effect.gen(function* () {
          const dataAndLink = yield* getConfirmedStateFromStateQueueDatum(
            u.datum,
          );
          return { ...dataAndLink, utxo: u };
        }),
      ),
    );
    if (filteredForConfirmedState.length !== 1) {
      return yield* Effect.fail(
        new LinkedListError({
          message: `Failed to sort state queue UTxOs`,
          cause: `Confirmed state (root node) not found among state queue UTxOs`,
        }),
      );
    }

    const { utxo: confirmedStateUTxO, link: linkToOldestBlock } =
      filteredForConfirmedState[0];
    const sorted: StateQueueUTxO[] = [confirmedStateUTxO];
    let link = linkToOldestBlock;
    while (link !== "Empty") {
      const linkUTxO = yield* findLinkStateQueueUTxO(link, stateQueueUTxOs);
      sorted.push(linkUTxO);
      link = linkUTxO.datum.next;
    }
    return sorted;
  });

const requireSingleNonAdaPolicyId = (
  assets: Assets,
  label: string,
): PolicyId => {
  const policyIds = new Set(
    Object.entries(assets)
      .filter(([unit, quantity]) => unit !== "lovelace" && quantity !== 0n)
      .map(([unit]) => fromUnit(unit).policyId),
  );
  if (policyIds.size !== 1) {
    throw new Error(
      `${label} expected exactly one non-ADA policy, got ${policyIds.size.toString()}`,
    );
  }
  return [...policyIds][0]!;
};

export const resolveRemoveSlashingApproach = (
  ctx: Parameters<BuildTxWithRedeemer>[0],
  slashing: EmulatorStateQueueRemoveSlashingParams,
): SlashingApproach => {
  switch (slashing.kind) {
    case "operatorAlreadySlashed":
      return {
        OperatorAlreadySlashed: {
          active_operators_element_ref_input_index: requireReferenceInputIndex(
            ctx,
            slashing.activeOperatorsElementRefInput,
            "state-queue remove active-operators slashed witness",
          ),
          retired_operators_element_ref_input_index: requireReferenceInputIndex(
            ctx,
            slashing.retiredOperatorsElementRefInput,
            "state-queue remove retired-operators slashed witness",
          ),
        },
      };
    case "slashActiveOperator":
      return {
        SlashActiveOperator: {
          active_operators_redeemer_index: requireMintRedeemerIndex(
            ctx,
            requireSingleNonAdaPolicyId(
              slashing.activeOperatorsAssetsToBurn,
              "state-queue remove active-operators burn",
            ),
            "state-queue remove active-operators burn",
          ),
          m_fraud_prover_reward_output_index:
            resolveFraudProverRewardOutputIndex(
              ctx,
              slashing.fraudProverReward,
              "state-queue remove active-operator fraud-prover reward",
            ),
        },
      };
    case "slashRetiredOperator":
      return {
        SlashRetiredOperator: {
          retired_operators_redeemer_index: requireMintRedeemerIndex(
            ctx,
            requireSingleNonAdaPolicyId(
              slashing.retiredOperatorsAssetsToBurn,
              "state-queue remove retired-operators burn",
            ),
            "state-queue remove retired-operators burn",
          ),
          m_fraud_prover_reward_output_index:
            resolveFraudProverRewardOutputIndex(
              ctx,
              slashing.fraudProverReward,
              "state-queue remove retired-operator fraud-prover reward",
            ),
        },
      };
  }
};

/**
 * Locates the reward output the on-chain guard will check, or reports `null`
 * when no reward is being routed. The predicate mirrors
 * `fraud_prover_reward_output_is_exact_v1`: the prover's enterprise address,
 * exactly the reward in lovelace, nothing else in the value, and no reference
 * script — so a builder that pays the wrong shape fails here rather than
 * on-chain. The reference-script leg never bites on the `pay.ToAddress` route
 * this module builds, and is carried anyway so the mirror is complete rather
 * than merely sufficient for the current caller.
 */
export const resolveFraudProverRewardOutputIndex = (
  ctx: Parameters<BuildTxWithRedeemer>[0],
  reward: FraudProverRewardPlan | undefined,
  label: string,
): bigint | null => {
  if (reward === undefined) return null;
  const proverOutputs = ctx.outputs.filter(
    (output) => output.address === reward.proverEnterpriseAddress,
  );
  if (proverOutputs.length !== 1) {
    throw new Error(
      `${label} must create exactly one output at the fraud prover enterprise address; found ${proverOutputs.length.toString()}`,
    );
  }
  return requireUniqueOutputIndex(
    ctx.outputs,
    (output) =>
      output.address === reward.proverEnterpriseAddress &&
      output.assets.lovelace === reward.lovelace &&
      Object.keys(output.assets).length === 1 &&
      output.datum == null &&
      output.datumHash == null &&
      (output.scriptRef ?? null) === null,
    label,
  );
};

export const removeSlashingFraudProverReward = (
  slashing: EmulatorStateQueueRemoveSlashingParams,
): FraudProverRewardPlan | undefined =>
  slashing.kind === "operatorAlreadySlashed"
    ? undefined
    : slashing.fraudProverReward;

export const removeSlashingReferenceInputs = (
  slashing: EmulatorStateQueueRemoveSlashingParams,
): readonly UTxO[] =>
  slashing.kind === "operatorAlreadySlashed"
    ? [
        slashing.activeOperatorsElementRefInput,
        slashing.retiredOperatorsElementRefInput,
      ]
    : [];

export const collectRemoveSlashingInputs = (
  tx: TxBuilder,
  slashing: EmulatorStateQueueRemoveSlashingParams,
  referenceScripts: StateQueueRemoveReferenceScriptUTxOs | undefined,
): TxBuilder => {
  if (slashing.kind === "operatorAlreadySlashed") {
    return tx;
  }

  if (slashing.kind === "slashActiveOperator") {
    let updated = tx
      .collectFrom(
        [...slashing.activeOperatorInputs],
        encodeActiveOperatorSpendRedeemer(
          slashing.activeOperatorSpendRedeemer ?? "ListStateTransition",
        ),
      )
      .mintAssets(
        slashing.activeOperatorsAssetsToBurn,
        slashing.activeOperatorsMintRedeemer,
      );
    if (referenceScripts?.activeOperatorsMint === undefined) {
      updated = updated.attach.Script(slashing.activeOperatorsMintingScript);
    }
    if (slashing.continuedActiveOperatorAnchorOutput !== undefined) {
      updated = updated.pay.ToContract(
        slashing.continuedActiveOperatorAnchorOutput.address,
        {
          kind: "inline",
          value: slashing.continuedActiveOperatorAnchorOutput.datum,
        },
        slashing.continuedActiveOperatorAnchorOutput.assets,
      );
    }
    if (slashing.schedulerSpend !== undefined) {
      updated = updated.pay
        .ToContract(
          slashing.schedulerSpend.continuedOutput.address,
          {
            kind: "inline",
            value: slashing.schedulerSpend.continuedOutput.datum,
          },
          slashing.schedulerSpend.continuedOutput.assets,
        )
        .collectFrom(
          [slashing.schedulerSpend.input],
          slashing.schedulerSpend.redeemer,
        );
      if (referenceScripts?.schedulerSpend === undefined) {
        updated = updated.attach.Script(slashing.schedulerSpend.script);
      }
    }
    if (
      slashing.activeOperatorSpendingScript !== undefined &&
      referenceScripts?.activeOperatorsSpend === undefined
    ) {
      updated = updated.attach.Script(slashing.activeOperatorSpendingScript);
    }
    return updated;
  }

  let updated = tx
    .collectFrom(
      [...slashing.retiredOperatorInputs],
      slashing.retiredOperatorSpendRedeemer ?? Data.void(),
    )
    .mintAssets(
      slashing.retiredOperatorsAssetsToBurn,
      slashing.retiredOperatorsMintRedeemer,
    );
  if (referenceScripts?.retiredOperatorsMint === undefined) {
    updated = updated.attach.Script(slashing.retiredOperatorsMintingScript);
  }
  if (slashing.continuedRetiredOperatorAnchorOutput !== undefined) {
    updated = updated.pay.ToContract(
      slashing.continuedRetiredOperatorAnchorOutput.address,
      {
        kind: "inline",
        value: slashing.continuedRetiredOperatorAnchorOutput.datum,
      },
      slashing.continuedRetiredOperatorAnchorOutput.assets,
    );
  }
  if (
    slashing.retiredOperatorSpendingScript !== undefined &&
    referenceScripts?.retiredOperatorsSpend === undefined
  ) {
    updated = updated.attach.Script(slashing.retiredOperatorSpendingScript);
  }
  return updated;
};
