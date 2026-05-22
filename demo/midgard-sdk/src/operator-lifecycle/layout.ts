/**
 * Register/activate layout derivation for operator lifecycle transactions.
 * This module is the canonical bridge between balanced draft transactions and
 * the ledger-ordered witness, redeemer, and policy-output layout they imply.
 */
import { compareHex } from "@al-ft/midgard-core/hex";
import * as SDK from "@/operator-lifecycle/primitives.js";
import {
  CML,
  coreToTxOutput,
  Data as LucidData,
  type UTxO,
} from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  collectSortedInputOutRefs,
  findOutRefIndex,
  resolveOutRefIndexFromSet,
} from "@/tx-out-ref-order.js";

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
  readonly retiredOperatorRefInputIndex: bigint;
  readonly prependedNodeOutputIndex: bigint;
  readonly anchorNodeOutputIndex: bigint;
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

/**
 * Lexicographically compares two hex strings by byte value.
 */
const compareHash28 = (left: string, right: string): number =>
  compareHex(left, right, { byteLength: 28 });

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
  if (output.datum == null) {
    return undefined;
  }
  const assetName = getAssetNameByPolicy(output.assets, policyId);
  if (assetName === null) {
    return undefined;
  }
  try {
    const linkedListDatum = LucidData.from(output.datum, SDK.LinkedListDatum);
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
}): bigint =>
  SDK.resolveMintPolicyTxInfoRedeemerIndexFromPolicySet({
    policyIds,
    targetPolicyId,
    precedingSpendRedeemerCount: spendRedeemerCount,
  });

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
  const index = findOutRefIndex(
    collectSortedInputOutRefs(referenceInputs),
    target,
  );
  return index === undefined ? undefined : BigInt(index);
};

/**
 * Resolves the canonical input index of a specific spent UTxO.
 */
export const findInputIndex = (
  tx: CML.Transaction,
  target: UTxO,
): bigint | undefined => {
  const index = findOutRefIndex(
    collectSortedInputOutRefs(tx.body().inputs()),
    target,
  );
  return index === undefined ? undefined : BigInt(index);
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
const layoutsEqual = <L extends object>(left: L, right: L): boolean =>
  Object.entries(left).every(([key, value]) => right[key as keyof L] === value);

export const registerLayoutsEqual = layoutsEqual<RegisterRedeemerLayout>;

/**
 * Formats a register-layout derivation for logs.
 */
export const registerLayoutToLogString = (
  layout: RegisterRedeemerLayout,
): string =>
  `root_in=${layout.rootInputIndex.toString()},hub_ref=${layout.hubOracleRefInputIndex.toString()},active_ref=${layout.activeOperatorRefInputIndex.toString()},retired_ref=${layout.retiredOperatorRefInputIndex.toString()},prepended_out=${layout.prependedNodeOutputIndex.toString()},anchor_out=${layout.anchorNodeOutputIndex.toString()}`;

/**
 * Compares two activate-layout derivations for exact equality.
 */
export const activateLayoutsEqual = layoutsEqual<ActivateRedeemerLayout>;

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
 * Derives the expected register redeemer layout before transaction balancing.
 */
export const resolveInitialRegisterRedeemerLayout = ({
  registeredOperatorScriptRefs,
  hubOracleRefInput,
  activeNotMemberWitness,
  retiredNotMemberWitness,
  registeredRootNode,
  fundingInputs,
}: {
  readonly registeredOperatorScriptRefs: readonly ReferenceScriptPublication[];
  readonly hubOracleRefInput: UTxO;
  readonly activeNotMemberWitness: NodeWithDatum;
  readonly retiredNotMemberWitness: NodeWithDatum;
  readonly registeredRootNode: NodeWithDatum;
  readonly fundingInputs: readonly UTxO[];
}): RegisterRedeemerLayout => {
  const referenceInputs = [
    ...registeredOperatorScriptRefs.map(({ utxo }) => utxo),
    hubOracleRefInput,
    activeNotMemberWitness.utxo,
    retiredNotMemberWitness.utxo,
  ] as const;
  const rootInputIndex = resolveOutRefIndexFromSet(registeredRootNode.utxo, [
    registeredRootNode.utxo,
    ...fundingInputs,
  ]);
  return {
    rootInputIndex,
    hubOracleRefInputIndex: resolveOutRefIndexFromSet(
      hubOracleRefInput,
      referenceInputs,
    ),
    activeOperatorRefInputIndex: resolveOutRefIndexFromSet(
      activeNotMemberWitness.utxo,
      referenceInputs,
    ),
    retiredOperatorRefInputIndex: resolveOutRefIndexFromSet(
      retiredNotMemberWitness.utxo,
      referenceInputs,
    ),
    // The register tx only emits the prepended node and the updated anchor in
    // authored order under the registered-operators policy.
    prependedNodeOutputIndex: 0n,
    anchorNodeOutputIndex: 1n,
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
  const registeredOperatorsRemovedNodeInputIndex = resolveOutRefIndexFromSet(
    registeredNode.utxo,
    activationInputs,
  );
  const registeredOperatorsAnchorNodeInputIndex = resolveOutRefIndexFromSet(
    registeredAnchor.utxo,
    activationInputs,
  );
  const activeOperatorsAnchorNodeInputIndex = resolveOutRefIndexFromSet(
    activeAppendAnchor.utxo,
    activationInputs,
  );
  const activationScriptSpendCount = 3;
  return {
    hubOracleRefInputIndex: resolveOutRefIndexFromSet(
      hubOracleRefInput,
      referenceInputs,
    ),
    retiredOperatorRefInputIndex: resolveOutRefIndexFromSet(
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
): Effect.Effect<bigint, SDK.StateQueueError> =>
  Effect.try({
    try: () =>
      SDK.resolveMintPolicyRedeemerTxInfoIndex({
        tx: draftTx,
        policyIds: [
          contracts.registeredOperators.policyId,
          contracts.activeOperators.policyId,
        ],
        targetPolicyId,
      }),
    catch: (cause) =>
      new SDK.StateQueueError({
        message: "Failed to resolve mint redeemer index in balanced draft tx",
        cause,
      }),
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
    const registeredOperatorsRedeemerIndex =
      yield* resolveMintRedeemerIndexForPolicy(
        tx,
        params.contracts,
        params.contracts.registeredOperators.policyId,
      );
    const activeOperatorsRedeemerIndex =
      yield* resolveMintRedeemerIndexForPolicy(
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
          message:
            "Registered node key is unexpectedly Empty during activation",
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
    let resolvedInsertedNodeOutputIndex =
      activeOperatorsInsertedNodeOutputIndex;
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
      registeredOperatorsRedeemerIndex,
      activeOperatorsRedeemerIndex,
      registeredOperatorsRemovedNodeInputIndex: registeredNodeInputPosition,
      registeredOperatorsAnchorNodeInputIndex: registeredAnchorInputPosition,
      registeredOperatorsAnchorNodeOutputIndex,
      activeOperatorsAnchorNodeInputIndex: activeOperatorsAnchorInputPosition,
      activeOperatorsInsertedNodeOutputIndex: resolvedInsertedNodeOutputIndex,
      activeOperatorsAnchorNodeOutputIndex: resolvedAnchorNodeOutputIndex,
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
    readonly retiredNotMemberWitness: NodeWithDatum;
    readonly registeredRootNode: NodeWithDatum;
    readonly registeredOperatorsPolicyId: string;
    readonly registeredOperatorsAddress: string;
    readonly registeredNodeUnit: string;
    readonly registeredRootNodeUnit: string;
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
    const retiredOperatorRefInputIndex = findReferenceInputIndex(
      tx,
      params.retiredNotMemberWitness.utxo,
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

    if (
      hubOracleRefInputIndex === undefined ||
      activeOperatorRefInputIndex === undefined ||
      retiredOperatorRefInputIndex === undefined ||
      rootInputIndex === undefined ||
      prependedNodeOutputIndex === undefined ||
      anchorNodeOutputIndex === undefined
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
            retiredOperatorRefInputIndex:
              retiredOperatorRefInputIndex?.toString() ?? "missing",
            rootInputIndex: rootInputIndex?.toString() ?? "missing",
            prependedNodeOutputIndex:
              prependedNodeOutputIndex?.toString() ?? "missing",
            anchorNodeOutputIndex:
              anchorNodeOutputIndex?.toString() ?? "missing",
          }),
        }),
      );
    }

    return {
      rootInputIndex,
      hubOracleRefInputIndex,
      activeOperatorRefInputIndex,
      retiredOperatorRefInputIndex,
      prependedNodeOutputIndex,
      anchorNodeOutputIndex,
    };
  });

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
    node.key === "Empty" || compareHash28(node.key.Key.key, keyHash) < 0;
  const upperBoundSatisfied =
    node.next === "Empty" || compareHash28(keyHash, node.next.Key.key) < 0;
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
  (node.key === "Empty" || compareHash28(node.key.Key.key, keyHash) < 0);

/**
 * Extracts the unique asset name minted under a given policy from an asset map.
 */
export const getAssetNameByPolicy = (
  assets: Readonly<Record<string, bigint>>,
  policyId: string,
): string | null => {
  const entries = Object.entries(assets).filter(([unit, quantity]) =>
    isPolicyAsset(unit, quantity, policyId),
  );
  if (entries.length !== 1) {
    return null;
  }
  return entries[0][0].slice(56);
};
