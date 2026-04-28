/**
 * Register/activate layout derivation for operator lifecycle transactions.
 * This module is the canonical bridge between balanced draft transactions and
 * the ledger-ordered witness, redeemer, and policy-output layout they imply.
 */
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  Data as LucidData,
  type UTxO,
  coreToTxOutput,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";
import {
  compareOutRefs,
  resolveReferenceInputIndexFromSet,
} from "@/tx-context.js";
import {
  getRedeemerPointersInContextOrder,
  getTxInfoRedeemerIndexes,
} from "@/cml-redeemers.js";

export type ReferenceScriptPublication = {
  readonly name: string;
  readonly utxo: UTxO;
};

export type NodeWithDatum = {
  readonly utxo: UTxO;
  readonly datum: SDK.LinkedListNodeView;
  readonly assetName: string;
};

export type RegisterRedeemerLayout = {
  readonly rootInputIndex: bigint;
  readonly hubOracleRefInputIndex: bigint;
  readonly activeOperatorRefInputIndex: bigint;
  readonly prependedNodeOutputIndex: bigint;
  readonly anchorNodeOutputIndex: bigint;
  readonly operatorOrigin:
    | {
        readonly tag: "NewOperator";
        readonly retiredOperatorRefInputIndex: bigint;
      }
    | {
        readonly tag: "ReturningOperator";
        readonly retiredOperatorsRedeemerIndex: bigint;
        readonly registeredOperatorsRedeemerIndex: bigint;
        readonly retiredOperatorRemovedNodeInputIndex: bigint;
        readonly retiredOperatorAnchorNodeInputIndex: bigint;
        readonly retiredOperatorAnchorNodeOutputIndex: bigint;
      };
};

type PartialRegisterOperatorOriginLayout =
  | {
      readonly tag: "NewOperator";
      readonly retiredOperatorRefInputIndex: bigint | undefined;
    }
  | {
      readonly tag: "ReturningOperator";
      readonly retiredOperatorsRedeemerIndex: bigint;
      readonly registeredOperatorsRedeemerIndex: bigint;
      readonly retiredOperatorRemovedNodeInputIndex: bigint | undefined;
      readonly retiredOperatorAnchorNodeInputIndex: bigint | undefined;
      readonly retiredOperatorAnchorNodeOutputIndex: bigint | undefined;
    };

export type ActivateRedeemerLayout = {
  readonly hubOracleRefInputIndex: bigint;
  readonly retiredOperatorRefInputIndex: bigint;
  readonly registeredOperatorsRedeemerIndex: bigint;
  readonly activeOperatorsRedeemerIndex: bigint;
  readonly registeredOperatorsRemovedNodeInputIndex: bigint;
  readonly registeredOperatorsAnchorNodeInputIndex: bigint;
  readonly registeredOperatorsAnchorNodeOutputIndex: bigint;
  readonly activeOperatorsAnchorNodeInputIndex: bigint;
  readonly activeOperatorsInsertedNodeOutputIndex: bigint;
  readonly activeOperatorsAnchorNodeOutputIndex: bigint;
};

export type RetireRedeemerLayout = {
  readonly hubOracleRefInputIndex: bigint;
  readonly schedulerRefInputIndex: bigint;
  readonly activeOperatorsRedeemerIndex: bigint;
  readonly retiredOperatorsRedeemerIndex: bigint;
  readonly activeOperatorsRemovedNodeInputIndex: bigint;
  readonly activeOperatorsAnchorNodeInputIndex: bigint;
  readonly activeOperatorsAnchorNodeOutputIndex: bigint;
  readonly retiredOperatorsAnchorNodeInputIndex: bigint;
  readonly retiredOperatorsInsertedNodeOutputIndex: bigint;
  readonly retiredOperatorsAnchorNodeOutputIndex: bigint;
};

/**
 * Minimal outref shape used for canonical ordering calculations.
 */
type OrderedOutRef = {
  readonly txHash: string;
  readonly outputIndex: number;
};

/**
 * Lexicographically compares two hex strings by byte value.
 */
const compareHex = (left: string, right: string): number =>
  Buffer.from(left, "hex").compare(Buffer.from(right, "hex"));

/**
 * Finds the canonical index of a target outref inside an unordered collection.
 */
const resolveOrderedOutRefIndex = (
  target: OrderedOutRef,
  ordered: readonly OrderedOutRef[],
): bigint | undefined => {
  const position = [...ordered]
    .sort(compareOutRefs)
    .findIndex(
      (candidate) =>
        candidate.txHash === target.txHash &&
        candidate.outputIndex === target.outputIndex,
    );
  return position >= 0 ? BigInt(position) : undefined;
};

/**
 * Returns whether an asset unit belongs to the given policy and has positive
 * quantity.
 */
const isPolicyAsset = (unit: string, quantity: bigint, policyId: string) =>
  unit !== "lovelace" && quantity > 0n && unit.startsWith(policyId);

const stringifyDatum = (value: unknown): string =>
  JSON.stringify(value, (_key, nested) =>
    typeof nested === "bigint" ? nested.toString() : nested,
  );

const decodeCanonicalNodeDatumFromOutput = (
  output: ReturnType<typeof coreToTxOutput>,
  policyId: string,
): SDK.LinkedListNodeView | undefined => {
  if (output.datum === undefined || output.datum === null) {
    return undefined;
  }
  const assetName = getAssetNameByPolicy(output.assets, policyId);
  if (assetName === null) {
    return undefined;
  }
  try {
    const linkedListDatum = LucidData.from(
      output.datum,
      SDK.LinkedListDatum,
    );
    return SDK.linkedListDatumToNodeView(linkedListDatum, assetName);
  } catch {
    return undefined;
  }
};

/**
 * Resolves the tx-info redeemer index for a mint policy when multiple mint
 * policies are present.
 */
const resolveMintRedeemerTxInfoIndex = ({
  targetPolicyId,
  policyIds,
  spendRedeemerCount,
}: {
  readonly targetPolicyId: string;
  readonly policyIds: readonly string[];
  readonly spendRedeemerCount: number;
}): bigint => {
  const sortedPolicyIds = [...policyIds].sort(compareHex);
  const targetPolicyContextIndex = sortedPolicyIds.indexOf(targetPolicyId);
  if (targetPolicyContextIndex < 0) {
    throw new Error(
      `Failed to resolve mint redeemer index for policy ${targetPolicyId}`,
    );
  }
  return BigInt(spendRedeemerCount + targetPolicyContextIndex);
};

/**
 * Resolves the canonical reference-input index of a specific UTxO.
 */
export const findReferenceInputIndex = (
  tx: CML.Transaction,
  target: UTxO,
): bigint | undefined => {
  const referenceInputs = tx.body().reference_inputs();
  if (referenceInputs === undefined) {
    return undefined;
  }
  const orderedReferenceInputs: OrderedOutRef[] = Array.from(
    { length: referenceInputs.len() },
    (_, index) => {
      const input = referenceInputs.get(index);
      return {
        txHash: input.transaction_id().to_hex(),
        outputIndex: Number(input.index()),
      };
    },
  );
  return resolveOrderedOutRefIndex(
    {
      txHash: target.txHash,
      outputIndex: Number(target.outputIndex),
    },
    orderedReferenceInputs,
  );
};

/**
 * Resolves the canonical input index of a specific spent UTxO.
 */
export const findInputIndex = (
  tx: CML.Transaction,
  target: UTxO,
): bigint | undefined => {
  const inputs = tx.body().inputs();
  const orderedInputs: OrderedOutRef[] = Array.from(
    { length: inputs.len() },
    (_, index) => {
      const input = inputs.get(index);
      return {
        txHash: input.transaction_id().to_hex(),
        outputIndex: Number(input.index()),
      };
    },
  );
  return resolveOrderedOutRefIndex(
    {
      txHash: target.txHash,
      outputIndex: Number(target.outputIndex),
    },
    orderedInputs,
  );
};

/**
 * Finds the global authored output position of a specific node token.
 */
export const findNodeOutputIndexByUnit = (
  tx: CML.Transaction,
  policyId: string,
  address: string,
  unit: string,
): bigint | undefined => {
  const outputs = tx.body().outputs();
  for (let index = 0; index < outputs.len(); index += 1) {
    const output = coreToTxOutput(outputs.get(index));
    if (
      output.address === address &&
      (output.assets[unit] ?? 0n) === 1n &&
      Object.entries(output.assets).some(([assetUnit, quantity]) =>
        isPolicyAsset(assetUnit, quantity, policyId),
      )
    ) {
      return BigInt(index);
    }
  }
  return undefined;
};

/**
 * Compares two register-layout derivations for exact equality.
 */
export const registerLayoutsEqual = (
  left: RegisterRedeemerLayout,
  right: RegisterRedeemerLayout,
): boolean =>
  left.rootInputIndex === right.rootInputIndex &&
  left.hubOracleRefInputIndex === right.hubOracleRefInputIndex &&
  left.activeOperatorRefInputIndex === right.activeOperatorRefInputIndex &&
  left.prependedNodeOutputIndex === right.prependedNodeOutputIndex &&
  left.anchorNodeOutputIndex === right.anchorNodeOutputIndex &&
  registerOriginLayoutsEqual(left.operatorOrigin, right.operatorOrigin);

const registerOriginLayoutsEqual = (
  left: RegisterRedeemerLayout["operatorOrigin"],
  right: RegisterRedeemerLayout["operatorOrigin"],
): boolean => {
  if (left.tag !== right.tag) {
    return false;
  }
  if (left.tag === "NewOperator") {
    return (
      right.tag === "NewOperator" &&
      left.retiredOperatorRefInputIndex === right.retiredOperatorRefInputIndex
    );
  }
  return (
    right.tag === "ReturningOperator" &&
    left.retiredOperatorsRedeemerIndex ===
      right.retiredOperatorsRedeemerIndex &&
    left.registeredOperatorsRedeemerIndex ===
      right.registeredOperatorsRedeemerIndex &&
    left.retiredOperatorRemovedNodeInputIndex ===
      right.retiredOperatorRemovedNodeInputIndex &&
    left.retiredOperatorAnchorNodeInputIndex ===
      right.retiredOperatorAnchorNodeInputIndex &&
    left.retiredOperatorAnchorNodeOutputIndex ===
      right.retiredOperatorAnchorNodeOutputIndex
  );
};

/**
 * Formats a register-layout derivation for logs.
 */
export const registerLayoutToLogString = (
  layout: RegisterRedeemerLayout,
): string =>
  [
    `root_in=${layout.rootInputIndex.toString()}`,
    `hub_ref=${layout.hubOracleRefInputIndex.toString()}`,
    `active_ref=${layout.activeOperatorRefInputIndex.toString()}`,
    `prepended_out=${layout.prependedNodeOutputIndex.toString()}`,
    `anchor_out=${layout.anchorNodeOutputIndex.toString()}`,
    registerOriginLayoutToLogString(layout.operatorOrigin),
  ].join(",");

const registerOriginLayoutToLogString = (
  origin: RegisterRedeemerLayout["operatorOrigin"],
): string =>
  origin.tag === "NewOperator"
    ? `origin=NewOperator,retired_ref=${origin.retiredOperatorRefInputIndex.toString()}`
    : [
        "origin=ReturningOperator",
        `retired_redeemer=${origin.retiredOperatorsRedeemerIndex.toString()}`,
        `registered_redeemer=${origin.registeredOperatorsRedeemerIndex.toString()}`,
        `retired_removed_in=${origin.retiredOperatorRemovedNodeInputIndex.toString()}`,
        `retired_anchor_in=${origin.retiredOperatorAnchorNodeInputIndex.toString()}`,
        `retired_anchor_out=${origin.retiredOperatorAnchorNodeOutputIndex.toString()}`,
      ].join(",");

/**
 * Compares two activate-layout derivations for exact equality.
 */
export const activateLayoutsEqual = (
  left: ActivateRedeemerLayout,
  right: ActivateRedeemerLayout,
): boolean =>
  left.hubOracleRefInputIndex === right.hubOracleRefInputIndex &&
  left.retiredOperatorRefInputIndex === right.retiredOperatorRefInputIndex &&
  left.registeredOperatorsRedeemerIndex ===
    right.registeredOperatorsRedeemerIndex &&
  left.activeOperatorsRedeemerIndex === right.activeOperatorsRedeemerIndex &&
  left.registeredOperatorsRemovedNodeInputIndex ===
    right.registeredOperatorsRemovedNodeInputIndex &&
  left.registeredOperatorsAnchorNodeInputIndex ===
    right.registeredOperatorsAnchorNodeInputIndex &&
  left.registeredOperatorsAnchorNodeOutputIndex ===
    right.registeredOperatorsAnchorNodeOutputIndex &&
  left.activeOperatorsAnchorNodeInputIndex ===
    right.activeOperatorsAnchorNodeInputIndex &&
  left.activeOperatorsInsertedNodeOutputIndex ===
    right.activeOperatorsInsertedNodeOutputIndex &&
  left.activeOperatorsAnchorNodeOutputIndex ===
    right.activeOperatorsAnchorNodeOutputIndex;

/**
 * Formats an activate-layout derivation for logs.
 */
export const activateLayoutToLogString = (
  layout: ActivateRedeemerLayout,
): string =>
  [
    `hub_ref=${layout.hubOracleRefInputIndex.toString()}`,
    `retired_ref=${layout.retiredOperatorRefInputIndex.toString()}`,
    `registered_redeemer=${layout.registeredOperatorsRedeemerIndex.toString()}`,
    `active_redeemer=${layout.activeOperatorsRedeemerIndex.toString()}`,
    `registered_removed_in=${layout.registeredOperatorsRemovedNodeInputIndex.toString()}`,
    `registered_anchor_in=${layout.registeredOperatorsAnchorNodeInputIndex.toString()}`,
    `registered_anchor_out=${layout.registeredOperatorsAnchorNodeOutputIndex.toString()}`,
    `active_anchor_in=${layout.activeOperatorsAnchorNodeInputIndex.toString()}`,
    `active_inserted_out=${layout.activeOperatorsInsertedNodeOutputIndex.toString()}`,
    `active_anchor_out=${layout.activeOperatorsAnchorNodeOutputIndex.toString()}`,
  ].join(",");

/**
 * Compares two retire-layout derivations for exact equality.
 */
export const retireLayoutsEqual = (
  left: RetireRedeemerLayout,
  right: RetireRedeemerLayout,
): boolean =>
  left.hubOracleRefInputIndex === right.hubOracleRefInputIndex &&
  left.schedulerRefInputIndex === right.schedulerRefInputIndex &&
  left.activeOperatorsRedeemerIndex === right.activeOperatorsRedeemerIndex &&
  left.retiredOperatorsRedeemerIndex === right.retiredOperatorsRedeemerIndex &&
  left.activeOperatorsRemovedNodeInputIndex ===
    right.activeOperatorsRemovedNodeInputIndex &&
  left.activeOperatorsAnchorNodeInputIndex ===
    right.activeOperatorsAnchorNodeInputIndex &&
  left.activeOperatorsAnchorNodeOutputIndex ===
    right.activeOperatorsAnchorNodeOutputIndex &&
  left.retiredOperatorsAnchorNodeInputIndex ===
    right.retiredOperatorsAnchorNodeInputIndex &&
  left.retiredOperatorsInsertedNodeOutputIndex ===
    right.retiredOperatorsInsertedNodeOutputIndex &&
  left.retiredOperatorsAnchorNodeOutputIndex ===
    right.retiredOperatorsAnchorNodeOutputIndex;

/**
 * Formats a retire-layout derivation for logs.
 */
export const retireLayoutToLogString = (
  layout: RetireRedeemerLayout,
): string =>
  [
    `hub_ref=${layout.hubOracleRefInputIndex.toString()}`,
    `scheduler_ref=${layout.schedulerRefInputIndex.toString()}`,
    `active_redeemer=${layout.activeOperatorsRedeemerIndex.toString()}`,
    `retired_redeemer=${layout.retiredOperatorsRedeemerIndex.toString()}`,
    `active_removed_in=${layout.activeOperatorsRemovedNodeInputIndex.toString()}`,
    `active_anchor_in=${layout.activeOperatorsAnchorNodeInputIndex.toString()}`,
    `active_anchor_out=${layout.activeOperatorsAnchorNodeOutputIndex.toString()}`,
    `retired_anchor_in=${layout.retiredOperatorsAnchorNodeInputIndex.toString()}`,
    `retired_inserted_out=${layout.retiredOperatorsInsertedNodeOutputIndex.toString()}`,
    `retired_anchor_out=${layout.retiredOperatorsAnchorNodeOutputIndex.toString()}`,
  ].join(",");

/**
 * Derives the expected register redeemer layout before transaction balancing.
 */
export const resolveInitialRegisterRedeemerLayout = ({
  registeredOperatorScriptRefs,
  retiredOperatorScriptRefs,
  hubOracleRefInput,
  activeNotMemberWitness,
  registeredRootNode,
  fundingInputs,
  operatorOrigin,
}: {
  readonly registeredOperatorScriptRefs: readonly ReferenceScriptPublication[];
  readonly retiredOperatorScriptRefs?: readonly ReferenceScriptPublication[];
  readonly hubOracleRefInput: UTxO;
  readonly activeNotMemberWitness: NodeWithDatum;
  readonly registeredRootNode: NodeWithDatum;
  readonly fundingInputs: readonly UTxO[];
  readonly operatorOrigin:
    | {
        readonly tag: "NewOperator";
        readonly retiredNotMemberWitness: NodeWithDatum;
      }
    | {
        readonly tag: "ReturningOperator";
        readonly retiredOperatorNode: NodeWithDatum;
        readonly retiredOperatorAnchor: NodeWithDatum;
        readonly contracts: SDK.MidgardValidators;
      };
}): RegisterRedeemerLayout => {
  const referenceInputs = [
    ...registeredOperatorScriptRefs.map(({ utxo }) => utxo),
    ...(operatorOrigin.tag === "ReturningOperator"
      ? (retiredOperatorScriptRefs ?? []).map(({ utxo }) => utxo)
      : []),
    hubOracleRefInput,
    activeNotMemberWitness.utxo,
    ...(operatorOrigin.tag === "NewOperator"
      ? [operatorOrigin.retiredNotMemberWitness.utxo]
      : []),
  ] as const;
  const transactionInputs = [
    registeredRootNode.utxo,
    ...(operatorOrigin.tag === "ReturningOperator"
      ? [
          operatorOrigin.retiredOperatorNode.utxo,
          operatorOrigin.retiredOperatorAnchor.utxo,
        ]
      : []),
    ...fundingInputs,
  ] as const;
  const rootInputIndex = resolveOrderedOutRefIndex(
    registeredRootNode.utxo,
    transactionInputs,
  );
  if (rootInputIndex === undefined) {
    throw new Error(
      `Failed to resolve initial registered root input index for ${registeredRootNode.utxo.txHash}#${registeredRootNode.utxo.outputIndex.toString()}`,
    );
  }
  const originLayout =
    operatorOrigin.tag === "NewOperator"
      ? {
          tag: "NewOperator" as const,
          retiredOperatorRefInputIndex: resolveReferenceInputIndexFromSet(
            operatorOrigin.retiredNotMemberWitness.utxo,
            referenceInputs,
          ),
        }
      : resolveInitialReturningOperatorOriginLayout({
          operatorOrigin,
          transactionInputs,
        });
  return {
    rootInputIndex,
    hubOracleRefInputIndex: resolveReferenceInputIndexFromSet(
      hubOracleRefInput,
      referenceInputs,
    ),
    activeOperatorRefInputIndex: resolveReferenceInputIndexFromSet(
      activeNotMemberWitness.utxo,
      referenceInputs,
    ),
    // The register tx only emits the prepended node and the updated anchor in
    // authored order under the registered-operators policy. Returning operators
    // emit the updated retired anchor after those two outputs.
    prependedNodeOutputIndex: 0n,
    anchorNodeOutputIndex: 1n,
    operatorOrigin: originLayout,
  };
};

const resolveInitialReturningOperatorOriginLayout = ({
  operatorOrigin,
  transactionInputs,
}: {
  readonly operatorOrigin: Extract<
    Parameters<
      typeof resolveInitialRegisterRedeemerLayout
    >[0]["operatorOrigin"],
    { readonly tag: "ReturningOperator" }
  >;
  readonly transactionInputs: readonly OrderedOutRef[];
}): Extract<
  RegisterRedeemerLayout["operatorOrigin"],
  { readonly tag: "ReturningOperator" }
> => {
  const retiredOperatorRemovedNodeInputIndex = resolveOrderedOutRefIndex(
    operatorOrigin.retiredOperatorNode.utxo,
    transactionInputs,
  );
  const retiredOperatorAnchorNodeInputIndex = resolveOrderedOutRefIndex(
    operatorOrigin.retiredOperatorAnchor.utxo,
    transactionInputs,
  );
  if (
    retiredOperatorRemovedNodeInputIndex === undefined ||
    retiredOperatorAnchorNodeInputIndex === undefined
  ) {
    throw new Error("Failed to resolve initial retired operator input indexes");
  }
  const returningRegisterScriptSpendCount = 3;
  return {
    tag: "ReturningOperator",
    retiredOperatorsRedeemerIndex: resolveMintRedeemerTxInfoIndex({
      targetPolicyId: operatorOrigin.contracts.retiredOperators.policyId,
      policyIds: [
        operatorOrigin.contracts.registeredOperators.policyId,
        operatorOrigin.contracts.retiredOperators.policyId,
      ],
      spendRedeemerCount: returningRegisterScriptSpendCount,
    }),
    registeredOperatorsRedeemerIndex: resolveMintRedeemerTxInfoIndex({
      targetPolicyId: operatorOrigin.contracts.registeredOperators.policyId,
      policyIds: [
        operatorOrigin.contracts.registeredOperators.policyId,
        operatorOrigin.contracts.retiredOperators.policyId,
      ],
      spendRedeemerCount: returningRegisterScriptSpendCount,
    }),
    retiredOperatorRemovedNodeInputIndex,
    retiredOperatorAnchorNodeInputIndex,
    retiredOperatorAnchorNodeOutputIndex: 2n,
  };
};

/**
 * Derives the expected activate redeemer layout before transaction balancing.
 */
export const resolveInitialActivateRedeemerLayout = ({
  registeredOperatorScriptRefs,
  activeOperatorScriptRefs,
  hubOracleRefInput,
  retiredNotMemberWitnessForActivate,
  registeredNode,
  registeredAnchor,
  activeAppendAnchor,
  contracts,
  fundingInputs,
}: {
  readonly registeredOperatorScriptRefs: readonly ReferenceScriptPublication[];
  readonly activeOperatorScriptRefs: readonly ReferenceScriptPublication[];
  readonly hubOracleRefInput: UTxO;
  readonly retiredNotMemberWitnessForActivate: NodeWithDatum;
  readonly registeredNode: NodeWithDatum;
  readonly registeredAnchor: NodeWithDatum;
  readonly activeAppendAnchor: NodeWithDatum;
  readonly contracts: SDK.MidgardValidators;
  readonly fundingInputs: readonly UTxO[];
}): ActivateRedeemerLayout => {
  const referenceInputs = [
    ...registeredOperatorScriptRefs.map(({ utxo }) => utxo),
    ...activeOperatorScriptRefs.map(({ utxo }) => utxo),
    hubOracleRefInput,
    retiredNotMemberWitnessForActivate.utxo,
  ] as const;
  const activationInputs = [
    registeredNode.utxo,
    registeredAnchor.utxo,
    activeAppendAnchor.utxo,
    ...fundingInputs,
  ] as const;
  const registeredOperatorsRemovedNodeInputIndex = resolveOrderedOutRefIndex(
    registeredNode.utxo,
    activationInputs,
  );
  const registeredOperatorsAnchorNodeInputIndex = resolveOrderedOutRefIndex(
    registeredAnchor.utxo,
    activationInputs,
  );
  const activeOperatorsAnchorNodeInputIndex = resolveOrderedOutRefIndex(
    activeAppendAnchor.utxo,
    activationInputs,
  );
  if (
    registeredOperatorsRemovedNodeInputIndex === undefined ||
    registeredOperatorsAnchorNodeInputIndex === undefined ||
    activeOperatorsAnchorNodeInputIndex === undefined
  ) {
    throw new Error("Failed to resolve initial activation input indexes");
  }
  const activationScriptSpendCount = 3;
  return {
    hubOracleRefInputIndex: resolveReferenceInputIndexFromSet(
      hubOracleRefInput,
      referenceInputs,
    ),
    retiredOperatorRefInputIndex: resolveReferenceInputIndexFromSet(
      retiredNotMemberWitnessForActivate.utxo,
      referenceInputs,
    ),
    registeredOperatorsRedeemerIndex: resolveMintRedeemerTxInfoIndex({
      targetPolicyId: contracts.registeredOperators.policyId,
      policyIds: [
        contracts.registeredOperators.policyId,
        contracts.activeOperators.policyId,
      ],
      spendRedeemerCount: activationScriptSpendCount,
    }),
    activeOperatorsRedeemerIndex: resolveMintRedeemerTxInfoIndex({
      targetPolicyId: contracts.activeOperators.policyId,
      policyIds: [
        contracts.registeredOperators.policyId,
        contracts.activeOperators.policyId,
      ],
      spendRedeemerCount: activationScriptSpendCount,
    }),
    registeredOperatorsRemovedNodeInputIndex,
    registeredOperatorsAnchorNodeInputIndex,
    activeOperatorsAnchorNodeInputIndex,
    // The activation tx emits the active inserted node, active anchor, and
    // registered anchor in authored order.
    registeredOperatorsAnchorNodeOutputIndex: 2n,
    activeOperatorsInsertedNodeOutputIndex: 0n,
    activeOperatorsAnchorNodeOutputIndex: 1n,
  };
};

/**
 * Derives the expected retire redeemer layout before transaction balancing.
 */
export const resolveInitialRetireRedeemerLayout = ({
  activeOperatorScriptRefs,
  retiredOperatorScriptRefs,
  hubOracleRefInput,
  schedulerRefInput,
  activeOperatorNode,
  activeOperatorAnchor,
  retiredAppendAnchor,
  contracts,
  fundingInputs,
}: {
  readonly activeOperatorScriptRefs: readonly ReferenceScriptPublication[];
  readonly retiredOperatorScriptRefs: readonly ReferenceScriptPublication[];
  readonly hubOracleRefInput: UTxO;
  readonly schedulerRefInput: UTxO;
  readonly activeOperatorNode: NodeWithDatum;
  readonly activeOperatorAnchor: NodeWithDatum;
  readonly retiredAppendAnchor: NodeWithDatum;
  readonly contracts: SDK.MidgardValidators;
  readonly fundingInputs: readonly UTxO[];
}): RetireRedeemerLayout => {
  const referenceInputs = [
    ...activeOperatorScriptRefs.map(({ utxo }) => utxo),
    ...retiredOperatorScriptRefs.map(({ utxo }) => utxo),
    hubOracleRefInput,
    schedulerRefInput,
  ] as const;
  const retireInputs = [
    activeOperatorNode.utxo,
    activeOperatorAnchor.utxo,
    retiredAppendAnchor.utxo,
    ...fundingInputs,
  ] as const;
  const activeOperatorsRemovedNodeInputIndex = resolveOrderedOutRefIndex(
    activeOperatorNode.utxo,
    retireInputs,
  );
  const activeOperatorsAnchorNodeInputIndex = resolveOrderedOutRefIndex(
    activeOperatorAnchor.utxo,
    retireInputs,
  );
  const retiredOperatorsAnchorNodeInputIndex = resolveOrderedOutRefIndex(
    retiredAppendAnchor.utxo,
    retireInputs,
  );
  if (
    activeOperatorsRemovedNodeInputIndex === undefined ||
    activeOperatorsAnchorNodeInputIndex === undefined ||
    retiredOperatorsAnchorNodeInputIndex === undefined
  ) {
    throw new Error("Failed to resolve initial retire input indexes");
  }
  return {
    hubOracleRefInputIndex: resolveReferenceInputIndexFromSet(
      hubOracleRefInput,
      referenceInputs,
    ),
    schedulerRefInputIndex: resolveReferenceInputIndexFromSet(
      schedulerRefInput,
      referenceInputs,
    ),
    activeOperatorsRedeemerIndex: resolveMintRedeemerTxInfoIndex({
      targetPolicyId: contracts.activeOperators.policyId,
      policyIds: [
        contracts.activeOperators.policyId,
        contracts.retiredOperators.policyId,
      ],
      spendRedeemerCount: 3,
    }),
    retiredOperatorsRedeemerIndex: resolveMintRedeemerTxInfoIndex({
      targetPolicyId: contracts.retiredOperators.policyId,
      policyIds: [
        contracts.activeOperators.policyId,
        contracts.retiredOperators.policyId,
      ],
      spendRedeemerCount: 3,
    }),
    activeOperatorsRemovedNodeInputIndex,
    activeOperatorsAnchorNodeInputIndex,
    activeOperatorsAnchorNodeOutputIndex: 2n,
    retiredOperatorsAnchorNodeInputIndex,
    retiredOperatorsInsertedNodeOutputIndex: 0n,
    retiredOperatorsAnchorNodeOutputIndex: 1n,
  };
};

/**
 * Describes the datum at a global authored output index for diagnostics.
 */
export const describePolicyOutputDatumAtIndex = (
  tx: CML.Transaction,
  policyId: string,
  outputIndex: bigint,
): string => {
  const index = Number(outputIndex);
  const outputs = tx.body().outputs();
  if (!Number.isSafeInteger(index) || index < 0 || index >= outputs.len()) {
    return `<missing:${outputIndex.toString()}>`;
  }
  const output = coreToTxOutput(outputs.get(index));
  const hasPolicyAsset = Object.entries(output.assets).some(
    ([assetUnit, quantity]) => isPolicyAsset(assetUnit, quantity, policyId),
  );
  if (!hasPolicyAsset) {
    return `<wrong-policy:${outputIndex.toString()}>`;
  }
  if (output.datum === undefined) {
    return "<no-datum>";
  }
  try {
    const nodeDatum = decodeCanonicalNodeDatumFromOutput(output, policyId);
    return `cbor=${output.datum},decoded=${stringifyDatum(nodeDatum)}`;
  } catch (cause) {
    return `<datum-decode-error:${String(cause)},cbor=${output.datum}>`;
  }
};

/**
 * Decodes a node datum from a global authored output index when present.
 */
export const getNodeDatumAtPolicyOutputIndex = (
  tx: CML.Transaction,
  policyId: string,
  outputIndex: bigint,
): SDK.LinkedListNodeView | undefined => {
  const index = Number(outputIndex);
  const outputs = tx.body().outputs();
  if (!Number.isSafeInteger(index) || index < 0 || index >= outputs.len()) {
    return undefined;
  }
  const output = coreToTxOutput(outputs.get(index));
  const hasPolicyAsset = Object.entries(output.assets).some(
    ([assetUnit, quantity]) => isPolicyAsset(assetUnit, quantity, policyId),
  );
  if (!hasPolicyAsset) {
    return undefined;
  }
  if (output.datum === undefined) {
    return undefined;
  }
  return decodeCanonicalNodeDatumFromOutput(output, policyId);
};

/**
 * Resolves the tx-info redeemer index of the registered-operators mint action
 * from a balanced draft transaction.
 */
const resolveMintRedeemerIndexForPolicy = (
  draftTx: CML.Transaction,
  contracts: SDK.MidgardValidators,
  targetPolicyId: string,
  mintPolicyIds: readonly string[] = [
    contracts.registeredOperators.policyId,
    contracts.activeOperators.policyId,
  ],
): Effect.Effect<number, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const pointers = getRedeemerPointersInContextOrder(draftTx);
    const txInfoRedeemerIndexes = getTxInfoRedeemerIndexes(pointers);
    const sortedMintPolicyIds = [...mintPolicyIds].sort(compareHex);
    const targetMintContextIndex = BigInt(
      sortedMintPolicyIds.indexOf(targetPolicyId),
    );
    const targetMintRedeemerContextIndex = pointers.findIndex(
      (pointer) =>
        pointer.tag === CML.RedeemerTag.Mint &&
        pointer.index === targetMintContextIndex,
    );
    if (targetMintRedeemerContextIndex < 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to locate mint redeemer index in balanced draft tx",
          cause: JSON.stringify(
            pointers.map((pointer) => ({
              tag: pointer.tag,
              index: pointer.index.toString(),
            })),
          ),
        }),
      );
    }
    const targetMintRedeemerTxInfoIndex =
      txInfoRedeemerIndexes[targetMintRedeemerContextIndex] ?? -1;
    if (targetMintRedeemerTxInfoIndex < 0) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to map mint redeemer from context order to tx-info order",
          cause: JSON.stringify({
            targetPolicyId,
            targetMintRedeemerContextIndex,
            txInfoRedeemerIndexes,
            pointers: pointers.map((pointer, contextIndex) => ({
              contextIndex,
              tag: pointer.tag,
              index: pointer.index.toString(),
            })),
          }),
        }),
      );
    }
    return targetMintRedeemerTxInfoIndex;
  });

/**
 * Derives the final activation redeemer layout from a balanced draft
 * transaction.
 */
export const deriveActivateRedeemerLayout = (
  tx: CML.Transaction,
  params: {
    readonly hubOracleRefInput: UTxO;
    readonly retiredNotMemberWitnessForActivate: NodeWithDatum;
    readonly operatorKeyHash: string;
    readonly registeredNode: NodeWithDatum;
    readonly registeredAnchor: NodeWithDatum;
    readonly activeAppendAnchor: NodeWithDatum;
    readonly registeredOperatorsPolicyId: string;
    readonly registeredOperatorsAddress: string;
    readonly registeredAnchorNodeUnit: string;
    readonly activeOperatorsPolicyId: string;
    readonly activeOperatorsAddress: string;
    readonly activeNodeUnit: string;
    readonly activeAnchorNodeUnit: string;
    readonly contracts: SDK.MidgardValidators;
  },
): Effect.Effect<ActivateRedeemerLayout, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const hubOracleRefInputIndex = findReferenceInputIndex(
      tx,
      params.hubOracleRefInput,
    );
    const retiredOperatorRefInputIndex = findReferenceInputIndex(
      tx,
      params.retiredNotMemberWitnessForActivate.utxo,
    );
    const registeredOperatorsRedeemerIndex = yield* resolveMintRedeemerIndexForPolicy(
      tx,
      params.contracts,
      params.contracts.registeredOperators.policyId,
    );
    const activeOperatorsRedeemerIndex = yield* resolveMintRedeemerIndexForPolicy(
      tx,
      params.contracts,
      params.contracts.activeOperators.policyId,
    );
    const registeredNodeInputPosition = findInputIndex(
      tx,
      params.registeredNode.utxo,
    );
    const registeredAnchorInputPosition = findInputIndex(
      tx,
      params.registeredAnchor.utxo,
    );
    const activeOperatorsAnchorInputPosition = findInputIndex(
      tx,
      params.activeAppendAnchor.utxo,
    );
    const registeredOperatorsAnchorNodeOutputIndex = findNodeOutputIndexByUnit(
      tx,
      params.registeredOperatorsPolicyId,
      params.registeredOperatorsAddress,
      params.registeredAnchorNodeUnit,
    );
    const activeOperatorsInsertedNodeOutputIndex = findNodeOutputIndexByUnit(
      tx,
      params.activeOperatorsPolicyId,
      params.activeOperatorsAddress,
      params.activeNodeUnit,
    );
    const activeOperatorsAnchorNodeOutputIndex = findNodeOutputIndexByUnit(
      tx,
      params.activeOperatorsPolicyId,
      params.activeOperatorsAddress,
      params.activeAnchorNodeUnit,
    );
    if (
      hubOracleRefInputIndex === undefined ||
      retiredOperatorRefInputIndex === undefined ||
      registeredNodeInputPosition === undefined ||
      registeredAnchorInputPosition === undefined ||
      activeOperatorsAnchorInputPosition === undefined ||
      registeredNodeInputPosition === registeredAnchorInputPosition ||
      registeredNodeInputPosition === activeOperatorsAnchorInputPosition ||
      registeredAnchorInputPosition === activeOperatorsAnchorInputPosition ||
      registeredOperatorsAnchorNodeOutputIndex === undefined ||
      activeOperatorsInsertedNodeOutputIndex === undefined ||
      activeOperatorsAnchorNodeOutputIndex === undefined
    ) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to derive activate redeemer layout from balanced draft transaction",
          cause: JSON.stringify({
            hubOracleRefInputIndex:
              hubOracleRefInputIndex?.toString() ?? "missing",
            retiredOperatorRefInputIndex:
              retiredOperatorRefInputIndex?.toString() ?? "missing",
            registeredOperatorsRedeemerIndex:
              registeredOperatorsRedeemerIndex.toString(),
            registeredNodeInputPosition:
              registeredNodeInputPosition?.toString() ?? "missing",
            registeredAnchorInputPosition:
              registeredAnchorInputPosition?.toString() ?? "missing",
            activeOperatorsAnchorInputPosition:
              activeOperatorsAnchorInputPosition?.toString() ?? "missing",
            registeredOperatorsAnchorNodeOutputIndex:
              registeredOperatorsAnchorNodeOutputIndex?.toString() ?? "missing",
            activeOperatorsInsertedNodeOutputIndex:
              activeOperatorsInsertedNodeOutputIndex?.toString() ?? "missing",
            activeOperatorsAnchorNodeOutputIndex:
              activeOperatorsAnchorNodeOutputIndex?.toString() ?? "missing",
          }),
        }),
      );
    }
    if (params.registeredNode.datum.key === "Empty") {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Registered node key is unexpectedly Empty during activation",
          cause: JSON.stringify({
            registeredNodeOutRef: `${params.registeredNode.utxo.txHash}#${params.registeredNode.utxo.outputIndex.toString()}`,
          }),
        }),
      );
    }
    const operatorKeyHash = params.operatorKeyHash;
    const insertedOutputDatum = getNodeDatumAtPolicyOutputIndex(
      tx,
      params.activeOperatorsPolicyId,
      activeOperatorsInsertedNodeOutputIndex,
    );
    const anchorOutputDatum = getNodeDatumAtPolicyOutputIndex(
      tx,
      params.activeOperatorsPolicyId,
      activeOperatorsAnchorNodeOutputIndex,
    );
    if (insertedOutputDatum === undefined || anchorOutputDatum === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to decode active policy output datum(s) while deriving activation layout",
          cause: JSON.stringify({
            insertedIndex: activeOperatorsInsertedNodeOutputIndex.toString(),
            anchorIndex: activeOperatorsAnchorNodeOutputIndex.toString(),
            insertedDatum: describePolicyOutputDatumAtIndex(
              tx,
              params.activeOperatorsPolicyId,
              activeOperatorsInsertedNodeOutputIndex,
            ),
            anchorDatum: describePolicyOutputDatumAtIndex(
              tx,
              params.activeOperatorsPolicyId,
              activeOperatorsAnchorNodeOutputIndex,
            ),
          }),
        }),
      );
    }
    const insertedMatchesOperator = nodeKeyEquals(
      insertedOutputDatum,
      operatorKeyHash,
    );
    const anchorMatchesOperator = nodeKeyEquals(
      anchorOutputDatum,
      operatorKeyHash,
    );
    let resolvedInsertedNodeOutputIndex = activeOperatorsInsertedNodeOutputIndex;
    let resolvedAnchorNodeOutputIndex = activeOperatorsAnchorNodeOutputIndex;
    if (!insertedMatchesOperator && anchorMatchesOperator) {
      resolvedInsertedNodeOutputIndex = activeOperatorsAnchorNodeOutputIndex;
      resolvedAnchorNodeOutputIndex = activeOperatorsInsertedNodeOutputIndex;
      yield* Effect.logWarning(
        [
          "Detected swapped active output indexes while deriving activation layout;",
          " correcting inserted/anchor indexes from policy output datums.",
          `operator=${operatorKeyHash}`,
          `inserted_out=${activeOperatorsInsertedNodeOutputIndex.toString()}`,
          `anchor_out=${activeOperatorsAnchorNodeOutputIndex.toString()}`,
          `corrected_inserted_out=${resolvedInsertedNodeOutputIndex.toString()}`,
          `corrected_anchor_out=${resolvedAnchorNodeOutputIndex.toString()}`,
        ].join(" "),
      );
    } else if (!insertedMatchesOperator) {
      yield* Effect.logWarning(
        [
          "Activation output derivation failed: inserted output does not carry operator key.",
          `operator=${operatorKeyHash}`,
          `inserted_out=${activeOperatorsInsertedNodeOutputIndex.toString()}`,
          `anchor_out=${activeOperatorsAnchorNodeOutputIndex.toString()}`,
          `inserted_datum=${describePolicyOutputDatumAtIndex(
            tx,
            params.activeOperatorsPolicyId,
            activeOperatorsInsertedNodeOutputIndex,
          )}`,
          `anchor_datum=${describePolicyOutputDatumAtIndex(
            tx,
            params.activeOperatorsPolicyId,
            activeOperatorsAnchorNodeOutputIndex,
          )}`,
        ].join(" "),
      );
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Derived inserted active output index does not point to the operator node",
          cause: JSON.stringify({
            operatorKeyHash,
            insertedIndex: activeOperatorsInsertedNodeOutputIndex.toString(),
            anchorIndex: activeOperatorsAnchorNodeOutputIndex.toString(),
            insertedDatum: describePolicyOutputDatumAtIndex(
              tx,
              params.activeOperatorsPolicyId,
              activeOperatorsInsertedNodeOutputIndex,
            ),
            anchorDatum: describePolicyOutputDatumAtIndex(
              tx,
              params.activeOperatorsPolicyId,
              activeOperatorsAnchorNodeOutputIndex,
            ),
          }),
        }),
      );
    }
    return {
      hubOracleRefInputIndex,
      retiredOperatorRefInputIndex,
      registeredOperatorsRedeemerIndex: BigInt(registeredOperatorsRedeemerIndex),
      activeOperatorsRedeemerIndex: BigInt(activeOperatorsRedeemerIndex),
      registeredOperatorsRemovedNodeInputIndex: registeredNodeInputPosition,
      registeredOperatorsAnchorNodeInputIndex: registeredAnchorInputPosition,
      registeredOperatorsAnchorNodeOutputIndex,
      activeOperatorsAnchorNodeInputIndex: activeOperatorsAnchorInputPosition,
      activeOperatorsInsertedNodeOutputIndex: resolvedInsertedNodeOutputIndex,
      activeOperatorsAnchorNodeOutputIndex: resolvedAnchorNodeOutputIndex,
    };
  });

/**
 * Derives the final retire redeemer layout from a balanced draft transaction.
 */
export const deriveRetireRedeemerLayout = (
  tx: CML.Transaction,
  params: {
    readonly hubOracleRefInput: UTxO;
    readonly schedulerRefInput: UTxO;
    readonly activeOperatorNode: NodeWithDatum;
    readonly activeOperatorAnchor: NodeWithDatum;
    readonly retiredAppendAnchor: NodeWithDatum;
    readonly activeOperatorsPolicyId: string;
    readonly activeOperatorsAddress: string;
    readonly activeNodeUnit: string;
    readonly activeAnchorNodeUnit: string;
    readonly retiredOperatorsPolicyId: string;
    readonly retiredOperatorsAddress: string;
    readonly retiredNodeUnit: string;
    readonly retiredAnchorNodeUnit: string;
    readonly contracts: SDK.MidgardValidators;
  },
): Effect.Effect<RetireRedeemerLayout, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const hubOracleRefInputIndex = findReferenceInputIndex(
      tx,
      params.hubOracleRefInput,
    );
    const schedulerRefInputIndex = findReferenceInputIndex(
      tx,
      params.schedulerRefInput,
    );
    const activeOperatorsRedeemerIndex = yield* resolveMintRedeemerIndexForPolicy(
      tx,
      params.contracts,
      params.contracts.activeOperators.policyId,
      [
        params.contracts.activeOperators.policyId,
        params.contracts.retiredOperators.policyId,
      ],
    );
    const retiredOperatorsRedeemerIndex = yield* resolveMintRedeemerIndexForPolicy(
      tx,
      params.contracts,
      params.contracts.retiredOperators.policyId,
      [
        params.contracts.activeOperators.policyId,
        params.contracts.retiredOperators.policyId,
      ],
    );
    const activeOperatorsRemovedNodeInputIndex = findInputIndex(
      tx,
      params.activeOperatorNode.utxo,
    );
    const activeOperatorsAnchorNodeInputIndex = findInputIndex(
      tx,
      params.activeOperatorAnchor.utxo,
    );
    const retiredOperatorsAnchorNodeInputIndex = findInputIndex(
      tx,
      params.retiredAppendAnchor.utxo,
    );
    const activeOperatorsAnchorNodeOutputIndex = findNodeOutputIndexByUnit(
      tx,
      params.activeOperatorsPolicyId,
      params.activeOperatorsAddress,
      params.activeAnchorNodeUnit,
    );
    const retiredOperatorsInsertedNodeOutputIndex = findNodeOutputIndexByUnit(
      tx,
      params.retiredOperatorsPolicyId,
      params.retiredOperatorsAddress,
      params.retiredNodeUnit,
    );
    const retiredOperatorsAnchorNodeOutputIndex = findNodeOutputIndexByUnit(
      tx,
      params.retiredOperatorsPolicyId,
      params.retiredOperatorsAddress,
      params.retiredAnchorNodeUnit,
    );
    if (
      hubOracleRefInputIndex === undefined ||
      schedulerRefInputIndex === undefined ||
      activeOperatorsRemovedNodeInputIndex === undefined ||
      activeOperatorsAnchorNodeInputIndex === undefined ||
      retiredOperatorsAnchorNodeInputIndex === undefined ||
      activeOperatorsRemovedNodeInputIndex ===
        activeOperatorsAnchorNodeInputIndex ||
      activeOperatorsRemovedNodeInputIndex ===
        retiredOperatorsAnchorNodeInputIndex ||
      activeOperatorsAnchorNodeInputIndex ===
        retiredOperatorsAnchorNodeInputIndex ||
      activeOperatorsAnchorNodeOutputIndex === undefined ||
      retiredOperatorsInsertedNodeOutputIndex === undefined ||
      retiredOperatorsAnchorNodeOutputIndex === undefined
    ) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to derive retire redeemer layout from balanced draft transaction",
          cause: JSON.stringify({
            hubOracleRefInputIndex:
              hubOracleRefInputIndex?.toString() ?? "missing",
            schedulerRefInputIndex:
              schedulerRefInputIndex?.toString() ?? "missing",
            activeOperatorsRedeemerIndex:
              activeOperatorsRedeemerIndex.toString(),
            retiredOperatorsRedeemerIndex:
              retiredOperatorsRedeemerIndex.toString(),
            activeOperatorsRemovedNodeInputIndex:
              activeOperatorsRemovedNodeInputIndex?.toString() ?? "missing",
            activeOperatorsAnchorNodeInputIndex:
              activeOperatorsAnchorNodeInputIndex?.toString() ?? "missing",
            retiredOperatorsAnchorNodeInputIndex:
              retiredOperatorsAnchorNodeInputIndex?.toString() ?? "missing",
            activeOperatorsAnchorNodeOutputIndex:
              activeOperatorsAnchorNodeOutputIndex?.toString() ?? "missing",
            retiredOperatorsInsertedNodeOutputIndex:
              retiredOperatorsInsertedNodeOutputIndex?.toString() ?? "missing",
            retiredOperatorsAnchorNodeOutputIndex:
              retiredOperatorsAnchorNodeOutputIndex?.toString() ?? "missing",
          }),
        }),
      );
    }
    const insertedOutputDatum = getNodeDatumAtPolicyOutputIndex(
      tx,
      params.retiredOperatorsPolicyId,
      retiredOperatorsInsertedNodeOutputIndex,
    );
    if (
      insertedOutputDatum === undefined ||
      params.activeOperatorNode.datum.key === "Empty" ||
      !nodeKeyEquals(
        insertedOutputDatum,
        params.activeOperatorNode.datum.key.Key.key,
      )
    ) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Derived inserted retired output index does not point to the retiring operator node",
          cause: JSON.stringify({
            insertedIndex: retiredOperatorsInsertedNodeOutputIndex.toString(),
            insertedDatum: describePolicyOutputDatumAtIndex(
              tx,
              params.retiredOperatorsPolicyId,
              retiredOperatorsInsertedNodeOutputIndex,
            ),
          }),
        }),
      );
    }
    return {
      hubOracleRefInputIndex,
      schedulerRefInputIndex,
      activeOperatorsRedeemerIndex: BigInt(activeOperatorsRedeemerIndex),
      retiredOperatorsRedeemerIndex: BigInt(retiredOperatorsRedeemerIndex),
      activeOperatorsRemovedNodeInputIndex,
      activeOperatorsAnchorNodeInputIndex,
      activeOperatorsAnchorNodeOutputIndex,
      retiredOperatorsAnchorNodeInputIndex,
      retiredOperatorsInsertedNodeOutputIndex,
      retiredOperatorsAnchorNodeOutputIndex,
    };
  });

/**
 * Derives the final register redeemer layout from a balanced draft
 * transaction.
 */
export const deriveRegisterRedeemerLayout = (
  tx: CML.Transaction,
  params: {
    readonly hubOracleRefInput: UTxO;
    readonly activeNotMemberWitness: NodeWithDatum;
    readonly registeredRootNode: NodeWithDatum;
    readonly registeredOperatorsPolicyId: string;
    readonly registeredOperatorsAddress: string;
    readonly registeredNodeUnit: string;
    readonly registeredRootNodeUnit: string;
    readonly operatorOrigin:
      | {
          readonly tag: "NewOperator";
          readonly retiredNotMemberWitness: NodeWithDatum;
        }
      | {
          readonly tag: "ReturningOperator";
          readonly retiredOperatorNode: NodeWithDatum;
          readonly retiredOperatorAnchor: NodeWithDatum;
          readonly retiredOperatorsPolicyId: string;
          readonly retiredOperatorsAddress: string;
          readonly retiredAnchorNodeUnit: string;
          readonly contracts: SDK.MidgardValidators;
        };
  },
): Effect.Effect<RegisterRedeemerLayout, SDK.StateQueueError> =>
  Effect.gen(function* () {
    const hubOracleRefInputIndex = findReferenceInputIndex(
      tx,
      params.hubOracleRefInput,
    );
    const activeOperatorRefInputIndex = findReferenceInputIndex(
      tx,
      params.activeNotMemberWitness.utxo,
    );
    const rootInputIndex = findInputIndex(tx, params.registeredRootNode.utxo);
    const prependedNodeOutputIndex = findNodeOutputIndexByUnit(
      tx,
      params.registeredOperatorsPolicyId,
      params.registeredOperatorsAddress,
      params.registeredNodeUnit,
    );
    const anchorNodeOutputIndex = findNodeOutputIndexByUnit(
      tx,
      params.registeredOperatorsPolicyId,
      params.registeredOperatorsAddress,
      params.registeredRootNodeUnit,
    );
    const originLayout =
      params.operatorOrigin.tag === "NewOperator"
        ? {
            tag: "NewOperator" as const,
            retiredOperatorRefInputIndex: findReferenceInputIndex(
              tx,
              params.operatorOrigin.retiredNotMemberWitness.utxo,
            ),
          }
        : {
            tag: "ReturningOperator" as const,
            retiredOperatorsRedeemerIndex: BigInt(
              yield* resolveMintRedeemerIndexForPolicy(
                tx,
                params.operatorOrigin.contracts,
                params.operatorOrigin.contracts.retiredOperators.policyId,
                [
                  params.operatorOrigin.contracts.registeredOperators.policyId,
                  params.operatorOrigin.contracts.retiredOperators.policyId,
                ],
              ),
            ),
            registeredOperatorsRedeemerIndex: BigInt(
              yield* resolveMintRedeemerIndexForPolicy(
                tx,
                params.operatorOrigin.contracts,
                params.operatorOrigin.contracts.registeredOperators.policyId,
                [
                  params.operatorOrigin.contracts.registeredOperators.policyId,
                  params.operatorOrigin.contracts.retiredOperators.policyId,
                ],
              ),
            ),
            retiredOperatorRemovedNodeInputIndex: findInputIndex(
              tx,
              params.operatorOrigin.retiredOperatorNode.utxo,
            ),
            retiredOperatorAnchorNodeInputIndex: findInputIndex(
              tx,
              params.operatorOrigin.retiredOperatorAnchor.utxo,
            ),
            retiredOperatorAnchorNodeOutputIndex: findNodeOutputIndexByUnit(
              tx,
              params.operatorOrigin.retiredOperatorsPolicyId,
              params.operatorOrigin.retiredOperatorsAddress,
              params.operatorOrigin.retiredAnchorNodeUnit,
            ),
          };

    if (
      hubOracleRefInputIndex === undefined ||
      activeOperatorRefInputIndex === undefined ||
      rootInputIndex === undefined ||
      prependedNodeOutputIndex === undefined ||
      anchorNodeOutputIndex === undefined ||
      originLayoutHasMissingIndex(originLayout)
    ) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Failed to derive register redeemer layout from balanced draft transaction",
          cause: JSON.stringify({
            hubOracleRefInputIndex:
              hubOracleRefInputIndex?.toString() ?? "missing",
            activeOperatorRefInputIndex:
              activeOperatorRefInputIndex?.toString() ?? "missing",
            rootInputIndex: rootInputIndex?.toString() ?? "missing",
            prependedNodeOutputIndex:
              prependedNodeOutputIndex?.toString() ?? "missing",
            anchorNodeOutputIndex: anchorNodeOutputIndex?.toString() ?? "missing",
            operatorOrigin:
              registerOriginLayoutToLogStringWithMissingIndexes(originLayout),
          }),
        }),
      );
    }

    return {
      rootInputIndex,
      hubOracleRefInputIndex,
      activeOperatorRefInputIndex,
      prependedNodeOutputIndex,
      anchorNodeOutputIndex,
      operatorOrigin: completeRegisterOriginLayout(originLayout),
    };
  });

const originLayoutHasMissingIndex = (
  origin: PartialRegisterOperatorOriginLayout,
): boolean =>
  origin.tag === "NewOperator"
    ? origin.retiredOperatorRefInputIndex === undefined
    : origin.retiredOperatorRemovedNodeInputIndex === undefined ||
      origin.retiredOperatorAnchorNodeInputIndex === undefined ||
      origin.retiredOperatorAnchorNodeOutputIndex === undefined;

const registerOriginLayoutToLogStringWithMissingIndexes = (
  origin: PartialRegisterOperatorOriginLayout,
): string =>
  origin.tag === "NewOperator"
    ? `origin=NewOperator,retired_ref=${origin.retiredOperatorRefInputIndex?.toString() ?? "missing"}`
    : [
        "origin=ReturningOperator",
        `retired_redeemer=${origin.retiredOperatorsRedeemerIndex.toString()}`,
        `registered_redeemer=${origin.registeredOperatorsRedeemerIndex.toString()}`,
        `retired_removed_in=${origin.retiredOperatorRemovedNodeInputIndex?.toString() ?? "missing"}`,
        `retired_anchor_in=${origin.retiredOperatorAnchorNodeInputIndex?.toString() ?? "missing"}`,
        `retired_anchor_out=${origin.retiredOperatorAnchorNodeOutputIndex?.toString() ?? "missing"}`,
      ].join(",");

const completeRegisterOriginLayout = (
  origin: PartialRegisterOperatorOriginLayout,
): RegisterRedeemerLayout["operatorOrigin"] => {
  if (origin.tag === "NewOperator") {
    if (origin.retiredOperatorRefInputIndex === undefined) {
      throw new Error("NewOperator origin is missing retired ref input index");
    }
    return {
      tag: "NewOperator",
      retiredOperatorRefInputIndex: origin.retiredOperatorRefInputIndex,
    };
  }
  if (
    origin.retiredOperatorRemovedNodeInputIndex === undefined ||
    origin.retiredOperatorAnchorNodeInputIndex === undefined ||
    origin.retiredOperatorAnchorNodeOutputIndex === undefined
  ) {
    throw new Error("ReturningOperator origin is missing retired layout indexes");
  }
  return {
    tag: "ReturningOperator",
    retiredOperatorsRedeemerIndex: origin.retiredOperatorsRedeemerIndex,
    registeredOperatorsRedeemerIndex: origin.registeredOperatorsRedeemerIndex,
    retiredOperatorRemovedNodeInputIndex:
      origin.retiredOperatorRemovedNodeInputIndex,
    retiredOperatorAnchorNodeInputIndex: origin.retiredOperatorAnchorNodeInputIndex,
    retiredOperatorAnchorNodeOutputIndex:
      origin.retiredOperatorAnchorNodeOutputIndex,
  };
};

/**
 * Returns whether a node carries the provided key hash as its own key.
 */
export const nodeKeyEquals = (
  node: SDK.LinkedListNodeView,
  keyHash: string,
): boolean => node.key !== "Empty" && node.key.Key.key === keyHash;

/**
 * Returns whether a node points to the provided key hash via its `next` link.
 */
export const linkPointsTo = (
  node: SDK.LinkedListNodeView,
  keyHash: string,
): boolean => node.next !== "Empty" && node.next.Key.key === keyHash;

/**
 * Returns whether a node is the correct ordered "not member" witness for the
 * provided key hash.
 */
export const orderedNotMemberWitness = (
  node: SDK.LinkedListNodeView,
  keyHash: string,
): boolean => {
  const lowerBoundSatisfied =
    node.key === "Empty" || compareHex(node.key.Key.key, keyHash) < 0;
  const upperBoundSatisfied =
    node.next === "Empty" || compareHex(keyHash, node.next.Key.key) < 0;
  return lowerBoundSatisfied && upperBoundSatisfied;
};

/**
 * Returns whether a node is the correct append-anchor witness for the active
 * operators list.
 */
export const activeAppendAnchorWitness = (
  node: SDK.LinkedListNodeView,
  keyHash: string,
): boolean =>
  node.next === "Empty" &&
  (node.key === "Empty" || compareHex(node.key.Key.key, keyHash) < 0);

/**
 * Extracts the unique asset name minted under a given policy from an asset map.
 */
export const getAssetNameByPolicy = (
  assets: Readonly<Record<string, bigint>>,
  policyId: string,
): string | null => {
  const entries = Object.entries(assets).filter(
    ([unit, quantity]) => isPolicyAsset(unit, quantity, policyId),
  );
  if (entries.length !== 1) {
    return null;
  }
  return entries[0][0].slice(56);
};
