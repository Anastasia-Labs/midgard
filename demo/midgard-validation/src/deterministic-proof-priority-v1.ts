/**
 * C52 "Aggregate script-execution floor" (GOAL_SPEC.md §8.3, line 901) needs
 * a deterministic way to pick which candidate proof transactions fill the
 * bounded aggregate-floor sequence
 * (`aggregate-script-execution-floor-v1.ts`'s
 * `BOUNDED_PROOF_TRANSACTION_COUNT_V1`) whenever more candidates exist than
 * there are bounded slots. GOAL_SPEC.md §3.1.6 requires proof selection to be
 * deterministic; this module is that total order.
 *
 * A candidate is described only by what the floor arithmetic and canonical
 * identity need: its measured execution units and its canonical proof
 * transaction id.
 */

/**
 * One candidate proof transaction competing for a bounded aggregate-floor
 * slot. `proofTransactionId` is the candidate's canonical transaction
 * identifier (a hex-encoded content hash) and is, by domain construction,
 * distinct for every distinct descriptor — two descriptors sharing an id are
 * the same proof transaction, not two candidates.
 */
export type ProofTransactionDescriptorV1 = Readonly<{
  proofTransactionId: string;
  memoryUnits: bigint;
  cpuUnits: bigint;
}>;

const compareBigintDescending = (left: bigint, right: bigint): number =>
  left === right ? 0 : left > right ? -1 : 1;

const compareStringAscending = (left: string, right: string): number =>
  left === right ? 0 : left < right ? -1 : 1;

/**
 * Canonical deterministic total order over proof-transaction descriptors.
 *
 * Priority is descending execution weight — the heaviest candidates sort
 * first, so a bounded sequence filled greedily from this order reaches the
 * aggregate floor using the fewest, most-substantial contributions — ranked
 * `memoryUnits` first, then `cpuUnits`, both descending.
 *
 * The canonical tie-break is `proofTransactionId` ascending, compared
 * byte-lexicographically over its hex encoding. Because every descriptor in
 * the domain carries a distinct canonical id, this comparator returns `0`
 * for two inputs only when they are the same descriptor: two syntactically
 * distinct descriptors always compare as strictly ordered, which is what
 * makes this a genuine total order rather than a partial order with an
 * unresolved tie.
 */
export const compareProofTransactionPriorityV1 = (
  left: ProofTransactionDescriptorV1,
  right: ProofTransactionDescriptorV1,
): number =>
  compareBigintDescending(left.memoryUnits, right.memoryUnits) ||
  compareBigintDescending(left.cpuUnits, right.cpuUnits) ||
  compareStringAscending(left.proofTransactionId, right.proofTransactionId);

/**
 * Selects the candidates that fill a bounded proof-transaction sequence:
 * the `boundedProofTransactionCount` highest-priority candidates under
 * `compareProofTransactionPriorityV1`, in priority order.
 *
 * `candidates` is not mutated. When `candidates.length` is at or below the
 * bound, every candidate is selected (still canonically ordered).
 */
export const selectBoundedProofTransactionSequenceV1 = (
  candidates: readonly ProofTransactionDescriptorV1[],
  boundedProofTransactionCount: bigint,
): readonly ProofTransactionDescriptorV1[] => {
  if (boundedProofTransactionCount < 0n) {
    throw new Error("bounded proof-transaction count must be non-negative");
  }
  const ordered = [...candidates].sort(compareProofTransactionPriorityV1);
  const bound =
    boundedProofTransactionCount > BigInt(ordered.length)
      ? ordered.length
      : Number(boundedProofTransactionCount);
  return ordered.slice(0, bound);
};
