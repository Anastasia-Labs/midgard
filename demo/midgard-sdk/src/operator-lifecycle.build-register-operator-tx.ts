import {
  type Assets,
  type BuildTxWithRedeemer,
  Data as LucidData,
  type LucidEvolution,
  type TxBuilder,
  type UTxO,
} from "@lucid-evolution/lucid";

import {
  type ActivateRedeemerLayout,
  type NodeWithDatum,
  type ReferenceScriptPublication,
  type RegisterRedeemerLayout,
} from "./operator-lifecycle/layout.js";
import {
  outputMatchesElement as outputMatches,
  requirePolicyNftUnit,
} from "./operator-lifecycle/output-selectors.js";
import * as SDK from "./operator-lifecycle/primitives.js";
import {
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "./tx-context-redeemer.js";

export const ACTIVE_OPERATOR_LIST_STATE_TRANSITION_REDEEMER = LucidData.to(
  "ListStateTransition",
  SDK.ActiveOperatorSpendRedeemer,
);

export const encodeActiveOperatorDatumValue = (
  bondUnlockTime: bigint | null,
): unknown =>
  SDK.castActiveOperatorDatumToData({
    bond_unlock_time: bondUnlockTime,
    inactivity_strikes: 0n,
  });

export const encodeLinkedListNodeView = (
  nodeView: SDK.LinkedListNodeView,
): string => SDK.encodeLinkedListNodeView(nodeView);

export const encodeRegisteredOperatorDatumValue = (
  operatorKeyHash: string,
): unknown =>
  SDK.castRegisteredOperatorDatumToData({
    operator: operatorKeyHash,
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
        anchor_element_input_outref:
          layout.registeredOperatorsAnchorNodeInputOutRef,
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

export type RegisterOperatorTxConfig = {
  readonly lucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
  readonly operatorKeyHash: string;
  readonly registeredOperatorScriptRefs: readonly ReferenceScriptPublication[];
  readonly hubOracleRefInput: UTxO;
  readonly activeNotMemberWitness: NodeWithDatum;
  readonly retiredNotMemberWitness: NodeWithDatum;
  readonly registeredRootNode: NodeWithDatum;
  readonly registerFundingInputs: readonly UTxO[];
  readonly registerMintAssets: Assets;
  readonly prependedNodeDatum: SDK.LinkedListNodeView;
  readonly prependedNodeAssets: Assets;
  readonly updatedRegisteredRootDatum: SDK.LinkedListNodeView;
  readonly registerValidTo: bigint;
  readonly layout?: RegisterRedeemerLayout;
  readonly onLayout?: (layout: RegisterRedeemerLayout) => void;
};

const deriveRegisterLayoutFromContext = ({
  config,
  ctx,
  prependedNodeDatumCbor,
  updatedRegisteredRootDatumCbor,
}: {
  readonly config: RegisterOperatorTxConfig;
  readonly ctx: Parameters<BuildTxWithRedeemer>[0];
  readonly prependedNodeDatumCbor: string;
  readonly updatedRegisteredRootDatumCbor: string;
}): RegisterRedeemerLayout => ({
  hubOracleRefInputIndex: requireReferenceInputIndex(
    ctx,
    config.hubOracleRefInput,
    "registered-operator register hub oracle",
  ),
  activeOperatorRefInputIndex: requireReferenceInputIndex(
    ctx,
    config.activeNotMemberWitness.utxo,
    "registered-operator register active witness",
  ),
  retiredOperatorRefInputIndex: requireReferenceInputIndex(
    ctx,
    config.retiredNotMemberWitness.utxo,
    "registered-operator register retired witness",
  ),
  prependedNodeOutputIndex: requireUniqueOutputIndex(
    ctx.outputs,
    (output) =>
      outputMatches({
        output,
        address: config.contracts.registeredOperators.spendingScriptAddress,
        datum: prependedNodeDatumCbor,
        unit: requirePolicyNftUnit(
          config.prependedNodeAssets,
          config.contracts.registeredOperators.policyId,
          "registered-operator register prepended node assets",
        ),
      }),
    "registered-operator register prepended node",
  ),
  anchorNodeOutputIndex: requireUniqueOutputIndex(
    ctx.outputs,
    (output) =>
      outputMatches({
        output,
        address: config.contracts.registeredOperators.spendingScriptAddress,
        datum: updatedRegisteredRootDatumCbor,
        unit: requirePolicyNftUnit(
          config.registeredRootNode.utxo.assets,
          config.contracts.registeredOperators.policyId,
          "registered-operator register root assets",
        ),
      }),
    "registered-operator register updated root",
  ),
});

export const buildRegisterOperatorTx = (
  config: RegisterOperatorTxConfig,
): TxBuilder => {
  const prependedNodeDatumCbor = encodeLinkedListNodeView(
    config.prependedNodeDatum,
  );
  const updatedRegisteredRootDatumCbor = encodeLinkedListNodeView(
    config.updatedRegisteredRootDatum,
  );
  const encodeRegisterRedeemer = (layout: RegisterRedeemerLayout): string =>
    LucidData.to(
      {
        RegisterOperator: {
          registering_operator: config.operatorKeyHash,
          root_output_index: layout.anchorNodeOutputIndex,
          registered_node_output_index: layout.prependedNodeOutputIndex,
          hub_oracle_ref_input_index: layout.hubOracleRefInputIndex,
          active_operators_element_ref_input_index:
            layout.activeOperatorRefInputIndex,
          retired_operators_element_ref_input_index:
            layout.retiredOperatorRefInputIndex,
        },
      },
      SDK.RegisteredOperatorMintRedeemer,
    );
  const registerRedeemer =
    config.layout === undefined
      ? (((ctx) => {
          requireOwnMintPurpose(
            ctx,
            config.contracts.registeredOperators.policyId,
            "registered-operator register mint",
          );
          const layout = deriveRegisterLayoutFromContext({
            config,
            ctx,
            prependedNodeDatumCbor,
            updatedRegisteredRootDatumCbor,
          });
          config.onLayout?.(layout);
          return encodeRegisterRedeemer(layout);
        }) satisfies BuildTxWithRedeemer)
      : encodeRegisterRedeemer(config.layout);
  if (config.layout !== undefined) {
    config.onLayout?.(config.layout);
  }
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
        value: prependedNodeDatumCbor,
      },
      config.prependedNodeAssets,
    )
    .pay.ToContract(
      config.contracts.registeredOperators.spendingScriptAddress,
      {
        kind: "inline",
        value: updatedRegisteredRootDatumCbor,
      },
      config.registeredRootNode.utxo.assets,
    )
    .addSignerKey(config.operatorKeyHash)
    .validTo(Number(config.registerValidTo))
    .collectFrom([...config.registerFundingInputs]);
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
  readonly activeInsertionAnchor: NodeWithDatum;
  readonly activationFundingInputs: readonly UTxO[];
  readonly validFrom: bigint;
  readonly validTo?: bigint;
  readonly registeredNodeUnit: string;
  readonly activeNodeUnit: string;
  readonly transferredOperatorAssets: Assets;
  readonly updatedRegisteredAnchorDatum: SDK.LinkedListNodeView;
  /**
   * Activation is permissionless on-chain: neither operator-directory
   * validator checks a signature for `ActivateOperator`. The operator's own
   * node still requires its key by default so an operator-run activation
   * cannot be replayed by a stranger's wallet snapshot; a third party that
   * activates an eligible registration on the operator's behalf sets this to
   * `false` and only pays the fee.
   */
  readonly requireOperatorSignature?: boolean;
  readonly layout?: ActivateRedeemerLayout;
  readonly onLayout?: (layout: ActivateRedeemerLayout) => void;
};
