import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import {
  type StateQueueAuthenticatedTransition,
  type StateQueueTransitionNode,
} from "@al-ft/midgard-sdk";

/**
 * Bounds the correction observer's admitted list and its per-reconcile L1
 * reads on an indefinitely running node.
 *
 * Cardano finality is k = 2160 blocks (the manifest's
 * automaticRecoveryMaxDepth). With canonicalDepth counting inclusion, retirement requires > k + 1.
 * A transition proven canonical deeper than that
 * can never be rolled back, so the observer neither re-reads its depth nor,
 * once nothing this node still journals depends on it, keeps it.
 */
export const AUTOMATIC_RECOVERY_MAX_DEPTH = BigInt(
  DEPLOYMENT_MANIFEST_L1_FINALITY.automaticRecoveryMaxDepth,
);

/** One retained journal, including finalized displacement/rewind evidence. */
export type CorrectionObserverJournalDependency = Readonly<{
  headerHash: string;
  baseTailHeaderHash: string;
  baseTailOutRef: string;
  abandoned: boolean;
}>;

/** The identity a proven-final memo entry is valid for. */
export const finalityKey = (
  transition: StateQueueAuthenticatedTransition,
): string =>
  `${transition.transactionHash}:${transition.blockHash}:${transition.transitionDigest}`;

const provenFinalByAuthority = new Map<string, Set<string>>();

/** Transitions this process has proven canonical deeper than
 * AUTOMATIC_RECOVERY_MAX_DEPTH, per observer authority. A restart re-proves
 * each once. */
export const provenFinalTransitions = (
  deploymentIdentityDigest: string,
  stateQueuePolicyId: string,
): Set<string> => {
  const key = `${deploymentIdentityDigest}:${stateQueuePolicyId}`;
  let set = provenFinalByAuthority.get(key);
  if (set === undefined) {
    set = new Set();
    provenFinalByAuthority.set(key, set);
  }
  return set;
};

const chainOrder = (
  left: StateQueueAuthenticatedTransition,
  right: StateQueueAuthenticatedTransition,
): number => {
  const block = BigInt(left.blockNo) - BigInt(right.blockNo);
  const index =
    block === 0n
      ? BigInt(left.transactionIndex) - BigInt(right.transactionIndex)
      : block;
  return index === 0n ? 0 : index < 0n ? -1 : 1;
};

const queueNodes = (
  transition: StateQueueAuthenticatedTransition,
): readonly StateQueueTransitionNode[] => [
  ...transition.previousQueue,
  ...transition.nextQueue,
];

/**
 * The admitted list without the merges nothing can read any more. A merge is
 * dropped only when all of these hold:
 *  - it is proven canonical more than AUTOMATIC_RECOVERY_MAX_DEPTH blocks after inclusion;
 *  - it is not the newest admitted merge (DA retention holds that merge's
 *    header for finality);
 *  - it names no header, base header or base output of a retained journal,
 *    and neither does the transition just before it in
 *    chain order (the root-base reading takes the transition after the one
 *    that emptied the queue).
 * Retained abandoned journals remain dependencies: only owned journal
 * retirement can declare their recovery evidence unnecessary.
 * Timeout corrections and fraud removals are never dropped: rewind plans and
 * DA removal outcomes read them for as long as they stand.
 */
export const pruneAdmittedBeyondRollbackHorizon = ({
  pending,
  admitted,
  provenFinal,
  dependencies,
}: {
  readonly pending: readonly StateQueueAuthenticatedTransition[];
  readonly admitted: readonly StateQueueAuthenticatedTransition[];
  readonly provenFinal: (
    transition: StateQueueAuthenticatedTransition,
  ) => boolean;
  readonly dependencies: readonly CorrectionObserverJournalDependency[];
}): StateQueueAuthenticatedTransition[] => {
  const recorded = [...pending, ...admitted].sort(chainOrder);
  const headers = new Set<string>();
  const outRefs = new Set<string>();
  for (const dependency of dependencies) {
    headers.add(dependency.headerHash);
    headers.add(dependency.baseTailHeaderHash);
    outRefs.add(dependency.baseTailOutRef);
  }
  const depends = (transition: StateQueueAuthenticatedTransition) =>
    transition.removedHeaderHashes.some((hash) => headers.has(hash)) ||
    transition.consumedQueueOutRefs.some((label) => outRefs.has(label)) ||
    queueNodes(transition).some(
      (node) =>
        (node.headerHash !== null && headers.has(node.headerHash)) ||
        outRefs.has(node.outRef),
    );
  const kept = new Set<string>();
  recorded.forEach((transition, index) => {
    if (!depends(transition)) return;
    kept.add(transition.transactionHash);
    const after = recorded[index + 1];
    if (after !== undefined) kept.add(after.transactionHash);
  });
  const newestMerge = admitted
    .filter((transition) => transition.transitionKind === "merge")
    .sort(chainOrder)
    .at(-1);
  return admitted.filter(
    (transition) =>
      transition.transitionKind !== "merge" ||
      transition === newestMerge ||
      kept.has(transition.transactionHash) ||
      !provenFinal(transition),
  );
};
