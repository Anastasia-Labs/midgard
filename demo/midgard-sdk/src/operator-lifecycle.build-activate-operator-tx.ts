import {
  type BuildTxWithRedeemer,
  Data as LucidData,
  type LucidEvolution,
  type TxBuilder,
} from "@lucid-evolution/lucid";

import { outputReferenceFromUTxO } from "./common.js";
import {
  type ActivateOperatorTxConfig,
  ACTIVE_OPERATOR_LIST_STATE_TRANSITION_REDEEMER,
  encodeActiveOperatorDatumValue,
  encodeLinkedListNodeView,
  registeredActivateRedeemer,
} from "./operator-lifecycle.build-register-operator-tx.js";
import {
  type ActivateRedeemerLayout,
  type NodeWithDatum,
  type ReferenceScriptPublication,
} from "./operator-lifecycle/layout.js";
import {
  outputMatchesElement as outputMatches,
  requirePolicyNftUnit,
} from "./operator-lifecycle/output-selectors.js";
import * as SDK from "./operator-lifecycle/primitives.js";
import {
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "./tx-context-redeemer.js";

const deriveActivateLayoutFromContext = ({
  config,
  ctx,
  activatedNodeDatumCbor,
  updatedActiveAnchorDatumCbor,
  updatedRegisteredAnchorDatumCbor,
}: {
  readonly config: ActivateOperatorTxConfig;
  readonly ctx: Parameters<BuildTxWithRedeemer>[0];
  readonly activatedNodeDatumCbor: string;
  readonly updatedActiveAnchorDatumCbor: string;
  readonly updatedRegisteredAnchorDatumCbor: string;
}): ActivateRedeemerLayout => ({
  hubOracleRefInputIndex: requireReferenceInputIndex(
    ctx,
    config.hubOracleRefInput,
    "operator activation hub oracle",
  ),
  retiredOperatorRefInputIndex: requireReferenceInputIndex(
    ctx,
    config.retiredNotMemberWitness.utxo,
    "operator activation retired witness",
  ),
  registeredOperatorsRedeemerIndex: requireMintRedeemerIndex(
    ctx,
    config.contracts.registeredOperators.policyId,
    "operator activation registered mint",
  ),
  activeOperatorsRedeemerIndex: requireMintRedeemerIndex(
    ctx,
    config.contracts.activeOperators.policyId,
    "operator activation active mint",
  ),
  registeredOperatorsAnchorNodeInputOutRef: outputReferenceFromUTxO(
    config.registeredAnchor.utxo,
  ),
  registeredOperatorsAnchorNodeOutputIndex: requireUniqueOutputIndex(
    ctx.outputs,
    (output) =>
      outputMatches({
        output,
        address: config.contracts.registeredOperators.spendingScriptAddress,
        datum: updatedRegisteredAnchorDatumCbor,
        unit: requirePolicyNftUnit(
          config.registeredAnchor.utxo.assets,
          config.contracts.registeredOperators.policyId,
          "operator activation registered anchor assets",
        ),
      }),
    "operator activation updated registered anchor",
  ),
  activeOperatorsInsertedNodeOutputIndex: requireUniqueOutputIndex(
    ctx.outputs,
    (output) =>
      outputMatches({
        output,
        address: config.contracts.activeOperators.spendingScriptAddress,
        datum: activatedNodeDatumCbor,
        unit: config.activeNodeUnit,
      }),
    "operator activation inserted active node",
  ),
  activeOperatorsAnchorNodeOutputIndex: requireUniqueOutputIndex(
    ctx.outputs,
    (output) =>
      outputMatches({
        output,
        address: config.contracts.activeOperators.spendingScriptAddress,
        datum: updatedActiveAnchorDatumCbor,
        unit: requirePolicyNftUnit(
          config.activeInsertionAnchor.utxo.assets,
          config.contracts.activeOperators.policyId,
          "operator activation active anchor assets",
        ),
      }),
    "operator activation updated active anchor",
  ),
});

export const buildActivateOperatorTx = (
  config: ActivateOperatorTxConfig,
): TxBuilder => {
  const activatedNodeDatum: SDK.LinkedListNodeView = {
    key: { Key: { key: config.operatorKeyHash } },
    next: config.activeInsertionAnchor.datum.next,
    data: encodeActiveOperatorDatumValue(
      null,
    ) as SDK.LinkedListNodeView["data"],
  };
  const updatedActiveAnchorDatum: SDK.LinkedListNodeView = {
    ...config.activeInsertionAnchor.datum,
    next: { Key: { key: config.operatorKeyHash } },
  };
  const activatedNodeDatumCbor = encodeLinkedListNodeView(activatedNodeDatum);
  const updatedActiveAnchorDatumCbor = encodeLinkedListNodeView(
    updatedActiveAnchorDatum,
  );
  const updatedRegisteredAnchorDatumCbor = encodeLinkedListNodeView(
    config.updatedRegisteredAnchorDatum,
  );
  const layoutFromContext = (
    ctx: Parameters<BuildTxWithRedeemer>[0],
  ): ActivateRedeemerLayout => {
    if (config.layout !== undefined) {
      return config.layout;
    }
    const layout = deriveActivateLayoutFromContext({
      config,
      ctx,
      activatedNodeDatumCbor,
      updatedActiveAnchorDatumCbor,
      updatedRegisteredAnchorDatumCbor,
    });
    config.onLayout?.(layout);
    return layout;
  };
  const encodeActiveRedeemer = (layout: ActivateRedeemerLayout): string =>
    LucidData.to(
      {
        ActivateOperator: {
          new_active_operator_key: config.operatorKeyHash,
          active_operator_anchor_element_output_index:
            layout.activeOperatorsAnchorNodeOutputIndex,
          active_operator_inserted_node_output_index:
            layout.activeOperatorsInsertedNodeOutputIndex,
          registered_operators_redeemer_index:
            layout.registeredOperatorsRedeemerIndex,
          active_operators_set_was_empty:
            config.activeInsertionAnchor.datum.key === "Empty" &&
            config.activeInsertionAnchor.datum.next === "Empty",
        },
      },
      SDK.ActiveOperatorMintRedeemer,
    );
  const registeredRedeemer =
    config.layout === undefined
      ? (((ctx) => {
          requireOwnMintPurpose(
            ctx,
            config.contracts.registeredOperators.policyId,
            "operator activation registered mint",
          );
          return registeredActivateRedeemer({
            operatorKeyHash: config.operatorKeyHash,
            layout: layoutFromContext(ctx),
          });
        }) satisfies BuildTxWithRedeemer)
      : registeredActivateRedeemer({
          operatorKeyHash: config.operatorKeyHash,
          layout: config.layout,
        });
  const activeRedeemer =
    config.layout === undefined
      ? (((ctx) => {
          requireOwnMintPurpose(
            ctx,
            config.contracts.activeOperators.policyId,
            "operator activation active mint",
          );
          return encodeActiveRedeemer(layoutFromContext(ctx));
        }) satisfies BuildTxWithRedeemer)
      : encodeActiveRedeemer(config.layout);
  if (config.layout !== undefined) {
    config.onLayout?.(config.layout);
  }

  let tx = config.lucid
    .newTx()
    .validFrom(Number(config.validFrom))
    .collectFrom([...config.activationFundingInputs])
    .collectFrom(
      [config.registeredNode.utxo, config.registeredAnchor.utxo],
      LucidData.void(),
    )
    .collectFrom(
      [config.activeInsertionAnchor.utxo],
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

  tx = tx.pay
    .ToContract(
      config.contracts.activeOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: activatedNodeDatumCbor,
      },
      config.transferredOperatorAssets,
    )
    .pay.ToContract(
      config.contracts.activeOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: updatedActiveAnchorDatumCbor,
      },
      config.activeInsertionAnchor.utxo.assets,
    )
    .pay.ToContract(
      config.contracts.registeredOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: updatedRegisteredAnchorDatumCbor,
      },
      config.registeredAnchor.utxo.assets,
    );
  return config.requireOperatorSignature === false
    ? tx
    : tx.addSignerKey(config.operatorKeyHash);
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
): TxBuilder => {
  const updatedRegisteredAnchorDatumCbor = encodeLinkedListNodeView(
    config.updatedRegisteredAnchorDatum,
  );
  const registeredAnchorNodeUnit = requirePolicyNftUnit(
    config.registeredAnchor.utxo.assets,
    config.contracts.registeredOperators.policyId,
    "deregister registered anchor",
  );
  const redeemer = ((ctx) => {
    requireOwnMintPurpose(
      ctx,
      config.contracts.registeredOperators.policyId,
      "deregister registered operator",
    );
    return LucidData.to(
      {
        DeregisterOperator: {
          deregistering_operator: config.operatorKeyHash,
          anchor_element_input_outref: outputReferenceFromUTxO(
            config.registeredAnchor.utxo,
          ),
          anchor_element_output_index: requireUniqueOutputIndex(
            ctx.outputs,
            (output) =>
              outputMatches({
                output,
                address:
                  config.contracts.registeredOperators.spendingScriptAddress,
                datum: updatedRegisteredAnchorDatumCbor,
                unit: registeredAnchorNodeUnit,
              }),
            "deregister registered anchor",
          ),
        },
      },
      SDK.RegisteredOperatorMintRedeemer,
    );
  }) satisfies BuildTxWithRedeemer;
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
        value: updatedRegisteredAnchorDatumCbor,
      },
      config.registeredAnchor.utxo.assets,
    )
    .addSignerKey(config.operatorKeyHash);
};
