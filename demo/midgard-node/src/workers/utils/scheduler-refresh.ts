import type * as SDK from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

export type NodeUtxoWithDatum = {
  readonly utxo: UTxO;
  readonly datum: SDK.NodeDatum;
};

export type SchedulerRefreshWitnessSelection =
  | {
      readonly kind: "Advance";
      readonly activeNode: NodeUtxoWithDatum;
    }
  | {
      readonly kind: "Rewind";
      readonly activeNode: NodeUtxoWithDatum;
      readonly activeRootNode: NodeUtxoWithDatum;
      readonly registeredWitnessNode: NodeUtxoWithDatum;
    };

const nodeKeyBytes = (key: SDK.NodeDatum["key"]): string | undefined =>
  key === "Empty" ? undefined : key.Key.key;

const linkKeyBytes = (datum: SDK.NodeDatum): string | undefined =>
  datum.next === "Empty" ? undefined : datum.next.Key.key;

const findRootNode = (
  nodes: readonly NodeUtxoWithDatum[],
  label: string,
): NodeUtxoWithDatum => {
  const rootNode = nodes.find((node) => node.datum.key === "Empty");
  if (rootNode === undefined) {
    throw new Error(`${label} root node is missing`);
  }
  return rootNode;
};

const findMemberNode = (
  nodes: readonly NodeUtxoWithDatum[],
  key: string,
  label: string,
): NodeUtxoWithDatum => {
  const node = nodes.find((candidate) => nodeKeyBytes(candidate.datum.key) === key);
  if (node === undefined) {
    throw new Error(`${label} node for key ${key} was not found`);
  }
  return node;
};

const findLastMemberNode = (
  nodes: readonly NodeUtxoWithDatum[],
  label: string,
): NodeUtxoWithDatum | undefined =>
  nodes.find(
    (candidate) =>
      candidate.datum.key !== "Empty" && candidate.datum.next === "Empty",
  );

export const resolveSchedulerRefreshWitnessSelection = ({
  currentOperator,
  targetOperator,
  activeNodes,
  registeredNodes,
  allowGenesisRewind,
}: {
  readonly currentOperator: string;
  readonly targetOperator: string;
  readonly activeNodes: readonly NodeUtxoWithDatum[];
  readonly registeredNodes: readonly NodeUtxoWithDatum[];
  readonly allowGenesisRewind: boolean;
}): SchedulerRefreshWitnessSelection => {
  const targetNode = findMemberNode(activeNodes, targetOperator, "Active-operators");
  if (linkKeyBytes(targetNode.datum) === currentOperator) {
    return {
      kind: "Advance",
      activeNode: targetNode,
    };
  }

  const activeRootNode = findRootNode(activeNodes, "Active-operators");
  const currentOperatorIsActiveHead = linkKeyBytes(activeRootNode.datum) === currentOperator;
  const targetNodeIsActiveTail = targetNode.datum.next === "Empty";
  if (!targetNodeIsActiveTail) {
    throw new Error(
      `Operator ${targetOperator} is not the next scheduled operator for current scheduler operator ${currentOperator}`,
    );
  }

  if (!allowGenesisRewind && !currentOperatorIsActiveHead) {
    throw new Error(
      `Operator ${targetOperator} cannot rewind scheduler from current operator ${currentOperator}`,
    );
  }

  const registeredWitnessNode =
    findLastMemberNode(registeredNodes, "Registered-operators") ??
    findRootNode(registeredNodes, "Registered-operators");

  return {
    kind: "Rewind",
    activeNode: targetNode,
    activeRootNode,
    registeredWitnessNode,
  };
};
