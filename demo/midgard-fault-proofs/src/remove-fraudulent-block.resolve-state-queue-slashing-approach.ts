import {
  encodeLinkedListNodeView,
  type FraudProverRewardPlan,
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  resolveFraudProverRewardOutputIndex,
  RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
  RetiredOperatorMintRedeemer,
  type SlashingApproach as SlashingApproachData,
} from "@al-ft/midgard-sdk";
import {
  type BuildTxWithRedeemer,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type RemoveFraudulentBlockContracts,
  type RemoveFraudulentBlockLayout,
} from "./remove-fraudulent-block.assert-exact-fraud-slash-lovelace-conservation.js";
import {
  buildActiveSlashingInputs,
  type OperatorSlashingPlan,
} from "./remove-fraudulent-block.build-active-slashing-inputs.js";
import {
  makeLayoutRedeemer,
  makeRetiredOperatorsMintRedeemerFromPlan,
  type SlashingTxPlan,
} from "./remove-fraudulent-block.derive-operator-slashing-layout-from-redeemer-context.js";
import {
  type OperatorSlashingLayoutContext,
  type RemoveFraudulentBlockSlashing,
} from "./remove-fraudulent-block.remove-fraudulent-block-explicit-category.js";
import { outRefLabel } from "./runtime.js";

const buildRetiredSlashingInputs = ({
  plan,
  operator,
  contracts,
  hubOracleUtxo,
  fraudProverReward,
}: {
  readonly plan: Extract<
    OperatorSlashingPlan,
    { readonly approach: "SlashRetiredOperator" }
  >;
  readonly operator: string;
  readonly contracts: RemoveFraudulentBlockContracts;
  readonly hubOracleUtxo: UTxO;
  readonly fraudProverReward?: FraudProverRewardPlan;
}): SlashingTxPlan => ({
  approach: "SlashRetiredOperator",
  removedOperatorNodeOutRef: outRefLabel(plan.removalPlan.node.utxo),
  registeredOperatorsElementOutRef: null,
  buildSlashing: () => {
    const retiredLayoutContext: OperatorSlashingLayoutContext = {
      operatorDirectoryAnchor: plan.removalPlan.anchor.utxo,
      operatorDirectoryNode: plan.removalPlan.node.utxo,
      operatorDirectoryAnchorUnit: plan.anchorUnit,
      slashedOperatorDirectory: "retired",
      hubOracle: hubOracleUtxo,
      contracts,
    };
    return {
      kind: "slashRetiredOperator",
      ...(fraudProverReward === undefined ? {} : { fraudProverReward }),
      retiredOperatorsAssetsToBurn: {
        [toUnit(
          contracts.retiredOperatorsPolicyId,
          RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX + operator,
        )]: -1n,
      },
      retiredOperatorsMintRedeemer: makeLayoutRedeemer({
        layoutContext: retiredLayoutContext,
        encode: makeRetiredOperatorsMintRedeemerFromPlan({ operator }),
        schema: RetiredOperatorMintRedeemer,
        bindOwnPurpose: (ctx) =>
          requireOwnMintPurpose(
            ctx,
            contracts.retiredOperatorsPolicyId,
            "remove-fraudulent-block retired-operators slash",
          ),
      }),
      retiredOperatorsMintingScript: contracts.retiredOperatorsMintingScript,
      retiredOperatorInputs: [
        plan.removalPlan.anchor.utxo,
        plan.removalPlan.node.utxo,
      ],
      retiredOperatorSpendingScript: contracts.retiredOperatorsSpendingScript,
      continuedRetiredOperatorAnchorOutput: {
        address: contracts.retiredOperatorsAddress,
        datum: encodeLinkedListNodeView({
          ...plan.removalPlan.anchor.view,
          next: plan.removalPlan.node.view.next,
        }),
        assets: plan.removalPlan.anchor.utxo.assets,
      },
    };
  },
  additionalRefInputs: [hubOracleUtxo],
});

const buildAlreadySlashedInputs = ({
  plan,
}: {
  readonly plan: Extract<
    OperatorSlashingPlan,
    { readonly approach: "OperatorAlreadySlashed" }
  >;
}): SlashingTxPlan => ({
  approach: "OperatorAlreadySlashed",
  removedOperatorNodeOutRef: null,
  registeredOperatorsElementOutRef: null,
  buildSlashing: () => ({
    kind: "operatorAlreadySlashed",
    activeOperatorsElementRefInput: plan.activeWitness.utxo,
    retiredOperatorsElementRefInput: plan.retiredWitness.utxo,
  }),
  additionalRefInputs: [],
});

export const buildSlashingInputs = ({
  plan,
  operator,
  contracts,
  hubOracleUtxo,
  registeredOperatorsElementUtxo,
  fraudProverReward,
}: {
  readonly plan: OperatorSlashingPlan;
  readonly operator: string;
  readonly contracts: RemoveFraudulentBlockContracts;
  readonly hubOracleUtxo: UTxO;
  readonly registeredOperatorsElementUtxo?: UTxO;
  /**
   * D3 reward routing. Only the bond-consuming approaches can carry it;
   * `OperatorAlreadySlashed` consumes no bond and pays no reward, which is the
   * D4 exclusivity ruling expressed in the redeemer's own shape.
   */
  readonly fraudProverReward?: FraudProverRewardPlan;
}): SlashingTxPlan => {
  switch (plan.approach) {
    case "SlashActiveOperator":
      return buildActiveSlashingInputs({
        plan,
        operator,
        contracts,
        hubOracleUtxo,
        registeredOperatorsElementUtxo,
        ...(fraudProverReward === undefined ? {} : { fraudProverReward }),
      });
    case "SlashRetiredOperator":
      return buildRetiredSlashingInputs({
        plan,
        operator,
        contracts,
        hubOracleUtxo,
        ...(fraudProverReward === undefined ? {} : { fraudProverReward }),
      });
    case "OperatorAlreadySlashed":
      return buildAlreadySlashedInputs({ plan });
  }
};

export const resolveStateQueueSlashingApproach = ({
  ctx,
  slashing,
  contracts,
}: {
  readonly ctx: Parameters<BuildTxWithRedeemer>[0];
  readonly slashing: RemoveFraudulentBlockSlashing;
  readonly contracts: RemoveFraudulentBlockContracts;
}): {
  readonly slashingApproach: SlashingApproachData;
  readonly layout: Partial<RemoveFraudulentBlockLayout>;
} => {
  switch (slashing.kind) {
    case "slashActiveOperator": {
      const activeOperatorsRedeemerTxInfoIndex = requireMintRedeemerIndex(
        ctx,
        contracts.activeOperatorsPolicyId,
        "remove-fraudulent-block active-operators slash",
      );
      return {
        slashingApproach: {
          SlashActiveOperator: {
            active_operators_redeemer_index: activeOperatorsRedeemerTxInfoIndex,
            m_fraud_prover_reward_output_index:
              resolveFraudProverRewardOutputIndex(
                ctx,
                slashing.fraudProverReward,
                "remove-fraudulent-block active-operator fraud-prover reward",
              ),
          },
        },
        layout: { activeOperatorsRedeemerTxInfoIndex },
      };
    }
    case "slashRetiredOperator": {
      const retiredOperatorsRedeemerTxInfoIndex = requireMintRedeemerIndex(
        ctx,
        contracts.retiredOperatorsPolicyId,
        "remove-fraudulent-block retired-operators slash",
      );
      return {
        slashingApproach: {
          SlashRetiredOperator: {
            retired_operators_redeemer_index:
              retiredOperatorsRedeemerTxInfoIndex,
            m_fraud_prover_reward_output_index:
              resolveFraudProverRewardOutputIndex(
                ctx,
                slashing.fraudProverReward,
                "remove-fraudulent-block retired-operator fraud-prover reward",
              ),
          },
        },
        layout: { retiredOperatorsRedeemerTxInfoIndex },
      };
    }
    case "operatorAlreadySlashed": {
      const activeOperatorsElementRefInputIndex = requireReferenceInputIndex(
        ctx,
        slashing.activeOperatorsElementRefInput,
        "remove-fraudulent-block active-operators non-membership witness",
      );
      const retiredOperatorsElementRefInputIndex = requireReferenceInputIndex(
        ctx,
        slashing.retiredOperatorsElementRefInput,
        "remove-fraudulent-block retired-operators non-membership witness",
      );
      return {
        slashingApproach: {
          OperatorAlreadySlashed: {
            active_operators_element_ref_input_index:
              activeOperatorsElementRefInputIndex,
            retired_operators_element_ref_input_index:
              retiredOperatorsElementRefInputIndex,
          },
        },
        layout: {
          activeOperatorsElementRefInputIndex,
          retiredOperatorsElementRefInputIndex,
        },
      };
    }
  }
};
