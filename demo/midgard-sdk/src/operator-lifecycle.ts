import {
  Data as LucidData,
  type Assets,
  type LucidEvolution,
  type RedeemerBuilder,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import * as SDK from "@/operator-lifecycle/primitives.js";
import {
  type ActivateRedeemerLayout,
  type NodeWithDatum,
  type ReferenceScriptPublication,
  type RegisterRedeemerLayout,
} from "@/operator-lifecycle/layout.js";

export * from "@/operator-lifecycle/layout.js";

const ACTIVATION_SELECTED_REGISTERED_INPUTS_COUNT = 2;

const ACTIVE_OPERATOR_LIST_STATE_TRANSITION_REDEEMER = LucidData.to(
  "ListStateTransition",
  SDK.ActiveOperatorSpendRedeemer,
);

const encodeActiveOperatorDatumValue = (
  bondUnlockTime: bigint | null,
): unknown =>
  SDK.castActiveOperatorDatumToData({
    bond_unlock_time: bondUnlockTime,
    inactivity_strikes: 0n,
  });

const encodeLinkedListNodeView = (nodeView: SDK.LinkedListNodeView): string =>
  SDK.encodeLinkedListNodeView(nodeView);

export const encodeRegisteredOperatorDatumValue = (
  operatorKeyHash: string,
): unknown =>
  SDK.castRegisteredOperatorDatumToData({
    operator: operatorKeyHash,
    bond_unlock_time: null,
  });

export const registeredActivateRedeemer = ({
  operatorKeyHash,
  layout,
}: {
  readonly operatorKeyHash: string;
  readonly layout: ActivateRedeemerLayout;
}): string =>
  LucidData.to(
    {
      ActivateOperator: {
        activating_operator: operatorKeyHash,
        anchor_element_input_index:
          layout.registeredOperatorsAnchorNodeInputIndex,
        removed_node_input_index:
          layout.registeredOperatorsRemovedNodeInputIndex,
        anchor_element_output_index:
          layout.registeredOperatorsAnchorNodeOutputIndex,
        hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
        retired_operators_element_ref_input_index:
          layout.retiredOperatorRefInputIndex,
        active_operators_redeemer_index: layout.activeOperatorsRedeemerIndex,
      },
    },
    SDK.RegisteredOperatorMintRedeemer,
  );

const registeredActivateRedeemerBuilder = ({
  operatorKeyHash,
  layout,
  registeredNode,
  registeredAnchor,
}: {
  readonly operatorKeyHash: string;
  readonly layout: ActivateRedeemerLayout;
  readonly registeredNode: UTxO;
  readonly registeredAnchor: UTxO;
}): RedeemerBuilder => ({
  kind: "selected",
  inputs: [registeredNode, registeredAnchor],
  makeRedeemer: (inputIndices) => {
    if (inputIndices.length !== ACTIVATION_SELECTED_REGISTERED_INPUTS_COUNT) {
      throw new Error(
        `Activation redeemer builder expected ${ACTIVATION_SELECTED_REGISTERED_INPUTS_COUNT.toString()} registered inputs, got ${inputIndices.length.toString()}`,
      );
    }
    return registeredActivateRedeemer({
      operatorKeyHash,
      layout: {
        ...layout,
        registeredOperatorsRemovedNodeInputIndex: inputIndices[0]!,
        registeredOperatorsAnchorNodeInputIndex: inputIndices[1]!,
      },
    });
  },
});

export type RegisterOperatorTxConfig = {
  readonly lucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
  readonly operatorKeyHash: string;
  readonly registeredOperatorScriptRefs: readonly ReferenceScriptPublication[];
  readonly hubOracleRefInput: UTxO;
  readonly activeNotMemberWitness: NodeWithDatum;
  readonly retiredNotMemberWitness: NodeWithDatum;
  readonly registeredRootNode: NodeWithDatum;
  readonly registerMintAssets: Assets;
  readonly prependedNodeDatum: SDK.LinkedListNodeView;
  readonly prependedNodeAssets: Assets;
  readonly updatedRegisteredRootDatum: SDK.LinkedListNodeView;
  readonly registerValidTo: bigint;
};

export const buildRegisterOperatorTx = (
  config: RegisterOperatorTxConfig,
  layout: RegisterRedeemerLayout,
): TxBuilder => {
  const registerRedeemer = LucidData.to(
    {
      RegisterOperator: {
        registering_operator: config.operatorKeyHash,
        root_input_index: layout.rootInputIndex,
        root_output_index: layout.anchorNodeOutputIndex,
        registered_node_output_index: layout.prependedNodeOutputIndex,
        hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
        active_operators_element_ref_input_index:
          layout.activeOperatorRefInputIndex,
        operator_origin: {
          NewOperator: {
            retired_operators_element_ref_input_index:
              layout.retiredOperatorRefInputIndex,
          },
        },
      },
    },
    SDK.RegisteredOperatorMintRedeemer,
  );
  return config.lucid
    .newTx()
    .collectFrom([config.registeredRootNode.utxo], LucidData.void())
    .readFrom([
      ...config.registeredOperatorScriptRefs.map(({ utxo }) => utxo),
      config.hubOracleRefInput,
      config.activeNotMemberWitness.utxo,
      config.retiredNotMemberWitness.utxo,
    ])
    .mintAssets(config.registerMintAssets, registerRedeemer)
    .pay.ToContract(
      config.contracts.registeredOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView(config.prependedNodeDatum),
      },
      config.prependedNodeAssets,
    )
    .pay.ToContract(
      config.contracts.registeredOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView(config.updatedRegisteredRootDatum),
      },
      config.registeredRootNode.utxo.assets,
    )
    .addSignerKey(config.operatorKeyHash)
    .validTo(Number(config.registerValidTo));
};

export type ActivateOperatorTxConfig = {
  readonly lucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
  readonly operatorKeyHash: string;
  readonly registeredOperatorScriptRefs: readonly ReferenceScriptPublication[];
  readonly activeOperatorScriptRefs: readonly ReferenceScriptPublication[];
  readonly hubOracleRefInput: UTxO;
  readonly retiredNotMemberWitness: NodeWithDatum;
  readonly registeredNode: NodeWithDatum;
  readonly registeredAnchor: NodeWithDatum;
  readonly activeAppendAnchor: NodeWithDatum;
  readonly activationFundingInputs: readonly UTxO[];
  readonly validFrom: bigint;
  readonly validTo?: bigint;
  readonly registeredNodeUnit: string;
  readonly activeNodeUnit: string;
  readonly transferredOperatorAssets: Assets;
  readonly updatedRegisteredAnchorDatum: SDK.LinkedListNodeView;
  readonly resolveRegisteredRedeemerWithBuilder?: boolean;
};

export const buildActivateOperatorTx = (
  config: ActivateOperatorTxConfig,
  layout: ActivateRedeemerLayout,
): TxBuilder => {
  const activatedNodeDatum: SDK.LinkedListNodeView = {
    key: { Key: { key: config.operatorKeyHash } },
    next: config.activeAppendAnchor.datum.next,
    data: encodeActiveOperatorDatumValue(
      null,
    ) as SDK.LinkedListNodeView["data"],
  };
  const updatedActiveAnchorDatum: SDK.LinkedListNodeView = {
    ...config.activeAppendAnchor.datum,
    next: { Key: { key: config.operatorKeyHash } },
  };
  const registeredRedeemer =
    config.resolveRegisteredRedeemerWithBuilder === true
      ? registeredActivateRedeemerBuilder({
          operatorKeyHash: config.operatorKeyHash,
          layout,
          registeredNode: config.registeredNode.utxo,
          registeredAnchor: config.registeredAnchor.utxo,
        })
      : registeredActivateRedeemer({
          operatorKeyHash: config.operatorKeyHash,
          layout,
        });
  const activeRedeemer = LucidData.to(
    {
      ActivateOperator: {
        new_active_operator_key: config.operatorKeyHash,
        new_active_operator_bond_unlock_time: null,
        active_operator_anchor_element_input_index:
          layout.activeOperatorsAnchorNodeInputIndex,
        active_operator_anchor_element_output_index:
          layout.activeOperatorsAnchorNodeOutputIndex,
        active_operator_inserted_node_output_index:
          layout.activeOperatorsInsertedNodeOutputIndex,
        registered_operators_redeemer_index:
          layout.registeredOperatorsRedeemerIndex,
      },
    },
    SDK.ActiveOperatorMintRedeemer,
  );

  let tx = config.lucid
    .newTx()
    .validFrom(Number(config.validFrom))
    .collectFrom([...config.activationFundingInputs])
    .collectFrom(
      [config.registeredNode.utxo, config.registeredAnchor.utxo],
      LucidData.void(),
    )
    .collectFrom(
      [config.activeAppendAnchor.utxo],
      ACTIVE_OPERATOR_LIST_STATE_TRANSITION_REDEEMER,
    )
    .readFrom([
      ...config.registeredOperatorScriptRefs.map(({ utxo }) => utxo),
      ...config.activeOperatorScriptRefs.map(({ utxo }) => utxo),
      config.hubOracleRefInput,
      config.retiredNotMemberWitness.utxo,
    ])
    .mintAssets({ [config.registeredNodeUnit]: -1n }, registeredRedeemer)
    .mintAssets({ [config.activeNodeUnit]: 1n }, activeRedeemer);
  if (config.validTo !== undefined) {
    tx = tx.validTo(Number(config.validTo));
  }

  return tx.pay
    .ToContract(
      config.contracts.activeOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView(activatedNodeDatum),
      },
      config.transferredOperatorAssets,
    )
    .pay.ToContract(
      config.contracts.activeOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView(updatedActiveAnchorDatum),
      },
      config.activeAppendAnchor.utxo.assets,
    )
    .pay.ToContract(
      config.contracts.registeredOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView(config.updatedRegisteredAnchorDatum),
      },
      config.registeredAnchor.utxo.assets,
    )
    .addSignerKey(config.operatorKeyHash);
};

export type DeregisterRegisteredOperatorLayout = {
  readonly removedNodeInputIndex: bigint;
  readonly anchorNodeInputIndex: bigint;
};

export type DeregisterRegisteredOperatorTxConfig = {
  readonly lucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
  readonly operatorKeyHash: string;
  readonly registeredOperatorScriptRefs: readonly ReferenceScriptPublication[];
  readonly registeredNode: NodeWithDatum;
  readonly registeredAnchor: NodeWithDatum;
  readonly registeredNodeUnit: string;
  readonly updatedRegisteredAnchorDatum: SDK.LinkedListNodeView;
};

export const buildDeregisterRegisteredOperatorTx = (
  config: DeregisterRegisteredOperatorTxConfig,
  layout: DeregisterRegisteredOperatorLayout,
): TxBuilder => {
  const redeemer = LucidData.to(
    {
      DeregisterOperator: {
        deregistering_operator: config.operatorKeyHash,
        removed_node_input_index: layout.removedNodeInputIndex,
        anchor_element_input_index: layout.anchorNodeInputIndex,
        anchor_element_output_index: 0n,
      },
    },
    SDK.RegisteredOperatorMintRedeemer,
  );
  return config.lucid
    .newTx()
    .collectFrom(
      [config.registeredNode.utxo, config.registeredAnchor.utxo],
      LucidData.void(),
    )
    .readFrom(config.registeredOperatorScriptRefs.map(({ utxo }) => utxo))
    .mintAssets({ [config.registeredNodeUnit]: -1n }, redeemer)
    .pay.ToContract(
      config.contracts.registeredOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: encodeLinkedListNodeView(config.updatedRegisteredAnchorDatum),
      },
      config.registeredAnchor.utxo.assets,
    )
    .addSignerKey(config.operatorKeyHash);
};
