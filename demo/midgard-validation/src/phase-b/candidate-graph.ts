/** Candidate dependencies and conflicts, including rejection propagation. Graph operations do not read or mutate the UTxO state. */
import { LedgerColumns } from "../ledger.js";
import { PhaseAValidatedTx, RejectCodes, RejectedTx } from "../types.js";
import { reject } from "./rejection.js";

export type CandidateNode = {
  readonly index: number;
  readonly candidate: PhaseAValidatedTx;
  readonly spentOutRefs: Set<string>;
  readonly referenceOutRefs: Set<string>;
  readonly parents: Set<number>;
  readonly children: Set<number>;
};

export type CandidateStatus = "pending" | "accepted" | "rejected";

export type CandidateDecision = {
  readonly index: number;
  readonly accepted: boolean;
  readonly rejection?: RejectedTx;
};

type ConflictNode = {
  readonly spentOutRefs: ReadonlySet<string>;
  readonly referenceOutRefs: ReadonlySet<string>;
};

/**
 * Builds transitive conflict components in near-linear time. Ref/ref overlap is
 * intentionally not a conflict; spend/spend and spend/ref overlap are.
 */
export const buildConflictComponents = <T extends ConflictNode>(
  readyNodes: readonly T[],
): T[][] => {
  const parent = readyNodes.map((_, index) => index);
  const rank = readyNodes.map(() => 0);
  const find = (index: number): number => {
    let root = index;
    while (parent[root] !== root) {
      root = parent[root];
    }
    while (parent[index] !== index) {
      const next = parent[index];
      parent[index] = root;
      index = next;
    }
    return root;
  };
  const union = (left: number, right: number): void => {
    let leftRoot = find(left);
    let rightRoot = find(right);
    if (leftRoot === rightRoot) return;
    if (rank[leftRoot] < rank[rightRoot]) {
      [leftRoot, rightRoot] = [rightRoot, leftRoot];
    }
    parent[rightRoot] = leftRoot;
    if (rank[leftRoot] === rank[rightRoot]) rank[leftRoot] += 1;
  };

  const spenderByOutRef = new Map<string, number>();
  const referencersByOutRef = new Map<string, number[]>();
  readyNodes.forEach((node, nodeIndex) => {
    for (const outRef of node.spentOutRefs) {
      const spender = spenderByOutRef.get(outRef);
      if (spender !== undefined) union(nodeIndex, spender);
      for (const referencer of referencersByOutRef.get(outRef) ?? []) {
        union(nodeIndex, referencer);
      }
      spenderByOutRef.set(outRef, nodeIndex);
    }
    for (const outRef of node.referenceOutRefs) {
      const spender = spenderByOutRef.get(outRef);
      if (spender !== undefined) union(nodeIndex, spender);
      const referencers = referencersByOutRef.get(outRef) ?? [];
      referencers.push(nodeIndex);
      referencersByOutRef.set(outRef, referencers);
    }
  });

  const componentByRoot = new Map<number, T[]>();
  readyNodes.forEach((node, index) => {
    const root = find(index);
    const component = componentByRoot.get(root) ?? [];
    component.push(node);
    componentByRoot.set(root, component);
  });
  return Array.from(componentByRoot.values());
};

export const buildNodes = (
  candidates: readonly PhaseAValidatedTx[],
): readonly CandidateNode[] => {
  const producerByOutRef = new Map<string, number>();

  for (let i = 0; i < candidates.length; i++) {
    for (const produced of candidates[i].graph.produced) {
      producerByOutRef.set(produced[LedgerColumns.OUTREF].toString("hex"), i);
    }
  }

  const nodes: CandidateNode[] = [];

  for (let i = 0; i < candidates.length; i++) {
    const candidate = candidates[i];
    const spentOutRefs = new Set(candidate.graph.spentOutRefHexes);
    const referenceOutRefs = new Set(candidate.graph.referenceOutRefHexes);
    const parents = new Set<number>();
    for (const spentOutRef of spentOutRefs) {
      const parent = producerByOutRef.get(spentOutRef);
      if (parent !== undefined && parent !== i) {
        parents.add(parent);
      }
    }
    for (const referenceOutRef of referenceOutRefs) {
      const parent = producerByOutRef.get(referenceOutRef);
      if (parent !== undefined && parent !== i) {
        parents.add(parent);
      }
    }

    nodes.push({
      index: i,
      candidate,
      spentOutRefs,
      referenceOutRefs,
      parents,
      children: new Set<number>(),
    });
  }

  for (const node of nodes) {
    for (const parent of node.parents) {
      nodes[parent].children.add(node.index);
    }
  }

  return nodes;
};

export const findCycleNodes = (
  nodes: readonly CandidateNode[],
): Set<number> => {
  const indegree = new Map<number, number>(
    nodes.map((node) => [node.index, node.parents.size]),
  );
  const queue: number[] = nodes
    .filter((node) => node.parents.size === 0)
    .map((node) => node.index);
  let visited = 0;

  while (queue.length > 0) {
    const index = queue.shift()!;
    visited += 1;
    for (const child of nodes[index].children) {
      const next = (indegree.get(child) ?? 0) - 1;
      indegree.set(child, next);
      if (next === 0) {
        queue.push(child);
      }
    }
  }

  if (visited === nodes.length) {
    return new Set<number>();
  }

  const cycleNodes = new Set<number>();
  for (const node of nodes) {
    if ((indegree.get(node.index) ?? 0) > 0) {
      cycleNodes.add(node.index);
    }
  }

  return cycleNodes;
};

export const cascadeRejectDescendants = (
  nodes: readonly CandidateNode[],
  rejectedRoot: number,
  statusByIndex: CandidateStatus[],
  rejected: RejectedTx[],
): void => {
  const queue = [...nodes[rejectedRoot].children];

  while (queue.length > 0) {
    const child = queue.shift()!;
    if (statusByIndex[child] !== "pending") {
      continue;
    }

    statusByIndex[child] = "rejected";
    rejected.push(
      reject(
        nodes[child].candidate.ledgerTx.txId,
        RejectCodes.DependsOnRejectedTx,
        `depends on rejected tx ${nodes[rejectedRoot].candidate.ledgerTx.txId.toString("hex")}`,
      ),
    );

    queue.push(...nodes[child].children);
  }
};
