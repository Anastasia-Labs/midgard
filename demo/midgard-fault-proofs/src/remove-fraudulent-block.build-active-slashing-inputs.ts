import {
  ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
  ACTIVE_OPERATORS_ROOT_ASSET_NAME,
  ActiveOperatorMintRedeemer,
  encodeLinkedListNodeView,
  type FraudProverRewardPlan,
  requireOwnMintPurpose,
  requireOwnSpendPurpose,
  RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
  RETIRED_OPERATORS_ROOT_ASSET_NAME,
  SCHEDULER_ASSET_NAME,
  SchedulerDatum,
  SchedulerSpendRedeemer,
} from "@al-ft/midgard-sdk";
import {
  Data,
  type LucidEvolution,
  toUnit,
  type UTxO,
} from "@lucid-evolution/lucid";

import { type RemoveFraudulentBlockContracts } from "./remove-fraudulent-block.assert-exact-fraud-slash-lovelace-conservation.js";
import {
  makeActiveOperatorsMintRedeemerFromPlan,
  makeLayoutRedeemer,
  makeSchedulerSpendRedeemerFromPlan,
  requireLayoutUtxo,
  type SlashingTxPlan,
} from "./remove-fraudulent-block.derive-operator-slashing-layout-from-redeemer-context.js";
import {
  activeOperatorUnit,
  nodeKeyValue,
} from "./remove-fraudulent-block.load-state-queue-topology.js";
import {
  type OperatorListEntry,
  type OperatorListRemovalPlan,
  type OperatorSlashingLayoutContext,
  type SchedulerRemovalPlan,
} from "./remove-fraudulent-block.remove-fraudulent-block-explicit-category.js";
import {
  loadOperatorList,
  resolveNonMembershipWitness,
  resolveOperatorRemovalPlan,
  resolveSchedulerRemovalPlan,
} from "./remove-fraudulent-block.resolve-registered-operator-removal-witness.js";
import { outRefLabel } from "./runtime.js";

export type OperatorSlashingPlan =
  | {
      readonly approach: "SlashActiveOperator";
      readonly removalPlan: OperatorListRemovalPlan;
      readonly schedulerUtxo: UTxO;
      readonly schedulerPlan: SchedulerRemovalPlan;
      readonly anchorKey: string | null;
      readonly anchorUnit: string;
      readonly activeOperatorsLastNode?: UTxO;
    }
  | {
      readonly approach: "SlashRetiredOperator";
      readonly removalPlan: OperatorListRemovalPlan;
      readonly anchorUnit: string;
    }
  | {
      readonly approach: "OperatorAlreadySlashed";
      readonly activeWitness: OperatorListEntry;
      readonly retiredWitness: OperatorListEntry;
    };

export const resolveOperatorSlashingPlan = async ({
  lucid,
  contracts,
  operator,
  schedulerUtxo,
  activeOperatorsRootUnit,
  retiredOperatorsRootUnit,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: RemoveFraudulentBlockContracts;
  readonly operator: string;
  readonly schedulerUtxo: UTxO;
  readonly activeOperatorsRootUnit: string;
  readonly retiredOperatorsRootUnit: string;
}): Promise<OperatorSlashingPlan> => {
  const activeEntries = await loadOperatorList({
    lucid,
    address: contracts.activeOperatorsAddress,
    policyId: contracts.activeOperatorsPolicyId,
    rootAssetName: ACTIVE_OPERATORS_ROOT_ASSET_NAME,
    nodeAssetNamePrefix: ACTIVE_OPERATOR_NODE_ASSET_NAME_PREFIX,
    label: "active-operators",
  });
  const activePlan = resolveOperatorRemovalPlan({
    entries: activeEntries,
    operator,
    label: "active-operators",
  });
  if (activePlan !== undefined) {
    const schedulerPlan = resolveSchedulerRemovalPlan({
      schedulerUtxo,
      operator,
      removalPlan: activePlan,
    });
    const anchorKey = nodeKeyValue(activePlan.anchor.view.key);
    const anchorUnit =
      anchorKey === null
        ? activeOperatorsRootUnit
        : activeOperatorUnit(contracts.activeOperatorsPolicyId, anchorKey);

    return {
      approach: "SlashActiveOperator",
      removalPlan: activePlan,
      schedulerUtxo,
      schedulerPlan,
      anchorKey,
      anchorUnit,
      activeOperatorsLastNode:
        schedulerPlan.kind === "rewind" &&
        schedulerPlan.newOperator !== undefined
          ? activePlan.lastNodeAfterRemoval?.utxo
          : undefined,
    };
  }

  const retiredEntries = await loadOperatorList({
    lucid,
    address: contracts.retiredOperatorsAddress,
    policyId: contracts.retiredOperatorsPolicyId,
    rootAssetName: RETIRED_OPERATORS_ROOT_ASSET_NAME,
    nodeAssetNamePrefix: RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX,
    label: "retired-operators",
  });
  const retiredPlan = resolveOperatorRemovalPlan({
    entries: retiredEntries,
    operator,
    label: "retired-operators",
  });
  if (retiredPlan !== undefined) {
    const anchorKey = nodeKeyValue(retiredPlan.anchor.view.key);
    const anchorUnit =
      anchorKey === null
        ? retiredOperatorsRootUnit
        : toUnit(
            contracts.retiredOperatorsPolicyId,
            RETIRED_OPERATOR_NODE_ASSET_NAME_PREFIX + anchorKey,
          );
    return {
      approach: "SlashRetiredOperator",
      removalPlan: retiredPlan,
      anchorUnit,
    };
  }

  return {
    approach: "OperatorAlreadySlashed",
    activeWitness: resolveNonMembershipWitness({
      entries: activeEntries,
      operator,
      label: "active-operators",
    }),
    retiredWitness: resolveNonMembershipWitness({
      entries: retiredEntries,
      operator,
      label: "retired-operators",
    }),
  };
};

export const buildActiveSlashingInputs = ({
  plan,
  operator,
  contracts,
  hubOracleUtxo,
  registeredOperatorsElementUtxo,
  fraudProverReward,
}: {
  readonly plan: Extract<
    OperatorSlashingPlan,
    { readonly approach: "SlashActiveOperator" }
  >;
  readonly operator: string;
  readonly contracts: RemoveFraudulentBlockContracts;
  readonly hubOracleUtxo: UTxO;
  readonly registeredOperatorsElementUtxo?: UTxO;
  readonly fraudProverReward?: FraudProverRewardPlan;
}): SlashingTxPlan => {
  const schedulerUnit = toUnit(
    contracts.schedulerPolicyId,
    SCHEDULER_ASSET_NAME,
  );
  const registeredOperatorsElement =
    plan.schedulerPlan.kind === "rewind"
      ? requireLayoutUtxo(
          registeredOperatorsElementUtxo,
          "registered-operators terminal element",
        )
      : undefined;
  return {
    approach: "SlashActiveOperator",
    removedOperatorNodeOutRef: outRefLabel(plan.removalPlan.node.utxo),
    registeredOperatorsElementOutRef:
      registeredOperatorsElement === undefined
        ? null
        : outRefLabel(registeredOperatorsElement),
    buildSlashing: (schedulerStartTime) => {
      const activeLayoutContext: OperatorSlashingLayoutContext = {
        operatorDirectoryAnchor: plan.removalPlan.anchor.utxo,
        operatorDirectoryNode: plan.removalPlan.node.utxo,
        scheduler: plan.schedulerUtxo,
        schedulerPlan: plan.schedulerPlan,
        hubOracle: hubOracleUtxo,
        activeOperatorsLastNode: plan.activeOperatorsLastNode,
        operatorDirectoryAnchorUnit: plan.anchorUnit,
        slashedOperatorDirectory: "active",
        schedulerUnit,
        ...(registeredOperatorsElement === undefined
          ? {}
          : { registeredOperatorsElement }),
        contracts,
      };
      const schedulerSpend =
        plan.schedulerPlan.kind === "inactive"
          ? undefined
          : {
              input: plan.schedulerUtxo,
              redeemer: makeLayoutRedeemer({
                layoutContext: activeLayoutContext,
                encode: makeSchedulerSpendRedeemerFromPlan({
                  schedulerPlan: plan.schedulerPlan,
                }),
                schema: SchedulerSpendRedeemer,
                bindOwnPurpose: (ctx) =>
                  requireOwnSpendPurpose(
                    ctx,
                    plan.schedulerUtxo,
                    "remove-fraudulent-block scheduler",
                  ),
              }),
              script: contracts.schedulerSpendingScript,
              continuedOutput: {
                address: contracts.schedulerAddress,
                datum:
                  plan.schedulerPlan.newOperator === undefined
                    ? Data.to("NoActiveOperators", SchedulerDatum)
                    : Data.to(
                        {
                          ActiveOperator: {
                            operator: plan.schedulerPlan.newOperator,
                            start_time: schedulerStartTime,
                          },
                        },
                        SchedulerDatum,
                      ),
                assets: plan.schedulerUtxo.assets,
              },
            };
      return {
        kind: "slashActiveOperator",
        ...(fraudProverReward === undefined ? {} : { fraudProverReward }),
        activeOperatorsAssetsToBurn: {
          [activeOperatorUnit(contracts.activeOperatorsPolicyId, operator)]:
            -1n,
        },
        activeOperatorsMintRedeemer: makeLayoutRedeemer({
          layoutContext: activeLayoutContext,
          encode: makeActiveOperatorsMintRedeemerFromPlan({
            operator,
            schedulerPlan: plan.schedulerPlan,
            activeOperatorAnchorKey: plan.anchorKey,
          }),
          schema: ActiveOperatorMintRedeemer,
          bindOwnPurpose: (ctx) =>
            requireOwnMintPurpose(
              ctx,
              contracts.activeOperatorsPolicyId,
              "remove-fraudulent-block active-operators slash",
            ),
        }),
        activeOperatorsMintingScript: contracts.activeOperatorsMintingScript,
        activeOperatorInputs: [
          plan.removalPlan.anchor.utxo,
          plan.removalPlan.node.utxo,
        ],
        activeOperatorSpendingScript: contracts.activeOperatorsSpendingScript,
        activeOperatorSpendRedeemer: "ListStateTransition",
        continuedActiveOperatorAnchorOutput: {
          address: contracts.activeOperatorsAddress,
          datum: encodeLinkedListNodeView({
            ...plan.removalPlan.anchor.view,
            next: plan.removalPlan.node.view.next,
          }),
          assets: plan.removalPlan.anchor.utxo.assets,
        },
        ...(schedulerSpend === undefined ? {} : { schedulerSpend }),
      };
    },
    additionalRefInputs: [
      hubOracleUtxo,
      ...(plan.schedulerPlan.kind === "inactive" ? [plan.schedulerUtxo] : []),
      ...(registeredOperatorsElement === undefined
        ? []
        : [registeredOperatorsElement]),
      ...(plan.activeOperatorsLastNode === undefined
        ? []
        : [plan.activeOperatorsLastNode]),
    ],
  };
};
