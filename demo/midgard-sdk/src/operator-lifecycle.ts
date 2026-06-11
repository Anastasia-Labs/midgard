import {
  Data as LucidData,
  type BuildTxWithRedeemer,
  type Assets,
  type LucidEvolution,
  type TxBuilder,
  type TxOutput,
  type UTxO,
} from "@lucid-evolution/lucid";
import { canonicalPlutusDataCbor } from "@al-ft/midgard-core/plutus-data-cbor";

import * as SDK from "@/operator-lifecycle/primitives.js";
import { outputReferenceFromUTxO } from "@/common.js";
import {
  type ActivateRedeemerLayout,
  type NodeWithDatum,
  type ReferenceScriptPublication,
  type RegisterRedeemerLayout,
} from "@/operator-lifecycle/layout.js";
import {
  requireMintRedeemerIndex,
  requireOwnMintPurpose,
  requireReferenceInputIndex,
  requireUniqueOutputIndex,
} from "@/tx-context-redeemer.js";

export * from "@/operator-lifecycle/layout.js";

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

const outputDatumCborMatches = (
  output: Pick<TxOutput, "datum">,
  datumCbor: string,
): boolean =>
  output.datum != null &&
  canonicalPlutusDataCbor(output.datum) === canonicalPlutusDataCbor(datumCbor);

const outputMatches = ({
  output,
  address,
  datum,
  unit,
}: {
  readonly output: TxOutput;
  readonly address: string;
  readonly datum: string;
  readonly unit: string;
}): boolean =>
  output.address === address &&
  outputDatumCborMatches(output, datum) &&
  (output.assets[unit] ?? 0n) === 1n;

const requirePolicyNftUnit = (
  assets: Assets,
  policyId: string,
  label: string,
): string => {
  const units = Object.entries(assets)
    .filter(
      ([unit, quantity]) =>
        unit !== "lovelace" && unit.startsWith(policyId) && quantity === 1n,
    )
    .map(([unit]) => unit);
  if (units.length !== 1) {
    throw new Error(
      `${label} expected exactly one ${policyId} NFT unit, got ${units.length.toString()}`,
    );
  }
  return units[0]!;
};

export const encodeRegisteredOperatorDatumValue = (
  operatorKeyHash: string,
): unknown =>
  SDK.castRegisteredOperatorDatumToData({
    operator: operatorKeyHash,
    bond_unlock_time: null,
  });

const registeredActivateRedeemer = ({
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
  readonly layout?: ActivateRedeemerLayout;
  readonly onLayout?: (layout: ActivateRedeemerLayout) => void;
};

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
          config.activeAppendAnchor.utxo.assets,
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
    next: config.activeAppendAnchor.datum.next,
    data: encodeActiveOperatorDatumValue(
      null,
    ) as SDK.LinkedListNodeView["data"],
  };
  const updatedActiveAnchorDatum: SDK.LinkedListNodeView = {
    ...config.activeAppendAnchor.datum,
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
          new_active_operator_bond_unlock_time: null,
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
      config.activeAppendAnchor.utxo.assets,
    )
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
