import {
  getLinkedListNodeViewFromUTxO,
  REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX,
  REGISTERED_OPERATORS_ROOT_ASSET_NAME,
} from "@al-ft/midgard-sdk";
import { type LucidEvolution, toUnit, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  compareHexByteStrings,
  decodeSchedulerDatum,
  hasOperatorListToken,
  nextKeyValue,
  nodeKeyValue,
} from "./remove-fraudulent-block.load-state-queue-topology.js";
import {
  type OperatorListEntry,
  type OperatorListRemovalPlan,
  type SchedulerRemovalPlan,
} from "./remove-fraudulent-block.remove-fraudulent-block-explicit-category.js";
import { outRefLabel } from "./runtime.js";

export const loadOperatorList = async ({
  lucid,
  address,
  policyId,
  rootAssetName,
  nodeAssetNamePrefix,
  label,
}: {
  readonly lucid: LucidEvolution;
  readonly address: string;
  readonly policyId: string;
  readonly rootAssetName: string;
  readonly nodeAssetNamePrefix: string;
  readonly label: string;
}): Promise<readonly OperatorListEntry[]> => {
  const utxos = await lucid.utxosAt(address);
  const entries = await Promise.all(
    utxos
      .filter((utxo) =>
        hasOperatorListToken({
          utxo,
          policyId,
          rootAssetName,
          nodeAssetNamePrefix,
        }),
      )
      .map(async (utxo) => ({
        utxo,
        view: await Effect.runPromise(getLinkedListNodeViewFromUTxO(utxo)),
      })),
  );
  const roots = entries.filter((entry) => entry.view.key === "Empty");
  if (roots.length !== 1) {
    throw new Error(
      `Expected exactly one ${label} root UTxO, found ${roots.length.toString()}.`,
    );
  }
  return entries;
};

export class RegisteredOperatorActivationRequiredError extends Error {
  constructor(
    readonly registeredOperatorOutRef: string,
    readonly activationTime: bigint,
  ) {
    super(
      `Scheduler rewind requires activation of registered operator ${registeredOperatorOutRef} first (activation time ${activationTime.toString()}).`,
    );
    this.name = "RegisteredOperatorActivationRequiredError";
  }
}

/** The scheduler requires the final registered element, whose activation is
 * still after the complete removal interval, or the empty registered root. */
export const resolveRegisteredOperatorRemovalWitness = async ({
  utxos,
  address,
  policyId,
  inclusiveValidityUpperBound,
}: {
  readonly utxos: readonly UTxO[];
  readonly address: string;
  readonly policyId: string;
  readonly inclusiveValidityUpperBound: bigint;
}): Promise<UTxO> => {
  const entries = await Promise.all(
    utxos
      .filter(
        (utxo) =>
          utxo.address === address &&
          hasOperatorListToken({
            utxo,
            policyId,
            rootAssetName: REGISTERED_OPERATORS_ROOT_ASSET_NAME,
            nodeAssetNamePrefix: REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX,
          }),
      )
      .map(async (utxo) => ({
        utxo,
        view: await Effect.runPromise(getLinkedListNodeViewFromUTxO(utxo)),
      })),
  );
  const roots = entries.filter((entry) => entry.view.key === "Empty");
  if (
    roots.length !== 1 ||
    roots[0]!.utxo.assets[
      toUnit(policyId, REGISTERED_OPERATORS_ROOT_ASSET_NAME)
    ] !== 1n
  ) {
    throw new Error("Registered operators require exactly one authentic root.");
  }
  const nodes = new Map<string, OperatorListEntry>();
  for (const entry of entries) {
    const key = nodeKeyValue(entry.view.key);
    if (key === null) continue;
    if (
      !/^(?:[0-9a-f]{2})+$/u.test(key) ||
      entry.utxo.assets[
        toUnit(policyId, REGISTERED_OPERATOR_NODE_ASSET_NAME_PREFIX + key)
      ] !== 1n ||
      nodes.has(key)
    ) {
      throw new Error(
        "Registered operators contain an invalid or duplicate node identity.",
      );
    }
    nodes.set(key, entry);
  }
  let terminal = roots[0]!;
  let previousActivation: bigint | undefined;
  const visited = new Set<string>();
  for (
    let key = nextKeyValue(terminal.view);
    key !== null;
    key = nextKeyValue(terminal.view)
  ) {
    const node = nodes.get(key);
    if (node === undefined || visited.has(key)) {
      throw new Error("Registered operators contain a missing node or cycle.");
    }
    const activation = BigInt(`0x${key}`);
    if (previousActivation !== undefined && activation >= previousActivation) {
      throw new Error(
        "Registered operators are not ordered by descending activation time.",
      );
    }
    visited.add(key);
    previousActivation = activation;
    terminal = node;
  }
  if (visited.size !== nodes.size) {
    throw new Error("Registered operators contain unreachable nodes.");
  }
  if (
    previousActivation !== undefined &&
    previousActivation <= inclusiveValidityUpperBound
  ) {
    throw new RegisteredOperatorActivationRequiredError(
      outRefLabel(terminal.utxo),
      previousActivation,
    );
  }
  return terminal.utxo;
};

export const resolveOperatorRemovalPlan = ({
  entries,
  operator,
  label,
}: {
  readonly entries: readonly OperatorListEntry[];
  readonly operator: string;
  readonly label: string;
}): OperatorListRemovalPlan | undefined => {
  const root = entries.find((entry) => entry.view.key === "Empty");
  if (root === undefined) {
    throw new Error(`${label} list is missing its root.`);
  }
  const nodesByKey = new Map<string, OperatorListEntry>();
  for (const entry of entries) {
    const key = nodeKeyValue(entry.view.key);
    if (key === null) {
      continue;
    }
    if (nodesByKey.has(key)) {
      throw new Error(`${label} list contains duplicate key ${key}.`);
    }
    nodesByKey.set(key, entry);
  }

  let anchor: OperatorListEntry = root;
  let currentKey = nextKeyValue(root.view);
  let removed: OperatorListEntry | undefined;
  let lastNode: OperatorListEntry | undefined;
  const visited = new Set<string>();
  while (currentKey !== null) {
    if (visited.has(currentKey)) {
      throw new Error(`${label} list contains a cycle at key ${currentKey}.`);
    }
    visited.add(currentKey);
    const current = nodesByKey.get(currentKey);
    if (current === undefined) {
      throw new Error(`${label} list points to missing node ${currentKey}.`);
    }
    if (currentKey === operator) {
      removed = current;
    }
    const nextKey = nextKeyValue(current.view);
    if (nextKey === null) {
      lastNode = current;
    }
    if (removed === undefined) {
      anchor = current;
    }
    currentKey = nextKey;
  }

  if (visited.size !== nodesByKey.size) {
    const unreachable = [...nodesByKey.keys()].filter(
      (key) => !visited.has(key),
    );
    throw new Error(
      `${label} list contains unreachable node(s): ${unreachable.join(", ")}.`,
    );
  }

  if (removed === undefined) {
    return undefined;
  }
  const lastNodeAfterRemoval =
    lastNode === undefined || nodeKeyValue(lastNode.view.key) === operator
      ? undefined
      : lastNode;
  return { anchor, node: removed, lastNodeAfterRemoval };
};

export const resolveNonMembershipWitness = ({
  entries,
  operator,
  label,
}: {
  readonly entries: readonly OperatorListEntry[];
  readonly operator: string;
  readonly label: string;
}): OperatorListEntry => {
  const root = entries.find((entry) => entry.view.key === "Empty");
  if (root === undefined) {
    throw new Error(`${label} list is missing its root.`);
  }
  let witness = root;
  let currentKey = nextKeyValue(root.view);
  const nodesByKey = new Map<string, OperatorListEntry>();
  for (const entry of entries) {
    const key = nodeKeyValue(entry.view.key);
    if (key !== null) {
      nodesByKey.set(key, entry);
    }
  }
  const visited = new Set<string>();
  while (currentKey !== null) {
    if (visited.has(currentKey)) {
      throw new Error(`${label} list contains a cycle at key ${currentKey}.`);
    }
    visited.add(currentKey);
    const current = nodesByKey.get(currentKey);
    if (current === undefined) {
      throw new Error(`${label} list points to missing node ${currentKey}.`);
    }
    const comparison = compareHexByteStrings(operator, currentKey);
    if (comparison === 0) {
      throw new Error(`${label} list still contains operator ${operator}.`);
    }
    if (comparison < 0) {
      return witness;
    }
    witness = current;
    currentKey = nextKeyValue(current.view);
  }
  return witness;
};

export const resolveSchedulerRemovalPlan = ({
  schedulerUtxo,
  operator,
  removalPlan,
}: {
  readonly schedulerUtxo: UTxO;
  readonly operator: string;
  readonly removalPlan: OperatorListRemovalPlan;
}): SchedulerRemovalPlan => {
  const schedulerDatum = decodeSchedulerDatum(schedulerUtxo);
  if (
    schedulerDatum === "NoActiveOperators" ||
    schedulerDatum.ActiveOperator.operator !== operator
  ) {
    return { kind: "inactive" };
  }
  const anchorKey = nodeKeyValue(removalPlan.anchor.view.key);
  const removedNodeIsLast = nextKeyValue(removalPlan.node.view) === null;
  if (anchorKey !== null) {
    return { kind: "goToAnchor", newOperator: anchorKey, removedNodeIsLast };
  }
  return {
    kind: "rewind",
    newOperator:
      removalPlan.lastNodeAfterRemoval === undefined
        ? undefined
        : (nodeKeyValue(removalPlan.lastNodeAfterRemoval.view.key) ??
          undefined),
    removedNodeIsLast,
  };
};
