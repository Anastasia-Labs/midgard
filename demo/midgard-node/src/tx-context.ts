/**
 * Canonical TxOutRef ordering and indexing helpers for the node.
 * This module keeps ledger-context input/reference-input ordering rules in one
 * place so workers and transaction builders do not reimplement them.
 */
import {
  compareOutRefs as compareCoreOutRefs,
  findOutRefIndex as findCoreOutRefIndex,
  outRefLabel as coreOutRefLabel,
  type OutRefLike,
} from "@al-ft/midgard-core/out-ref";
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

/**
 * Lightweight transaction-output reference shape used for ordering/indexing.
 */
export type { OutRefLike };

/**
 * Transaction output paired with its authored output index.
 */
export type IndexedTxOutput = ReturnType<typeof coreToTxOutput> & {
  readonly index: number;
};

/**
 * Canonical ledger ordering for inputs and reference inputs: lexicographic by
 * TxOutRef (`txHash`, then `outputIndex`).
 */
export const compareOutRefs = compareCoreOutRefs;

/**
 * Formats an outref as `txHash#outputIndex`.
 */
export const outRefLabel = coreOutRefLabel;

/**
 * Removes duplicate outrefs while preserving first-seen order.
 */
export const dedupeByOutRef = <T extends OutRefLike>(
  outRefs: readonly T[],
): readonly T[] => {
  const byOutRef = new Map<string, T>();
  for (const outRef of outRefs) {
    const label = outRefLabel(outRef);
    if (!byOutRef.has(label)) {
      byOutRef.set(label, outRef);
    }
  }
  return [...byOutRef.values()];
};

/**
 * Collects transaction inputs into canonical ledger order.
 */
export const collectSortedInputOutRefs = (
  inputs: CML.TransactionInputList,
): readonly OutRefLike[] =>
  [...Array(inputs.len()).keys()]
    .map((index) => {
      const input = inputs.get(index);
      return {
        txHash: input.transaction_id().to_hex(),
        outputIndex: Number(input.index()),
      };
    })
    .sort(compareOutRefs);

/**
 * Collects transaction outputs while preserving authored output order.
 */
export const collectIndexedOutputs = (
  outputs: CML.TransactionOutputList,
): readonly IndexedTxOutput[] => {
  const indexed: IndexedTxOutput[] = [];
  for (let index = 0; index < outputs.len(); index += 1) {
    indexed.push({
      index,
      ...coreToTxOutput(outputs.get(index)),
    });
  }
  return indexed;
};

/**
 * Finds the index of an outref in an already-ordered outref list.
 */
export const findOutRefIndex = (
  orderedOutRefs: readonly OutRefLike[],
  target: OutRefLike,
): number | undefined => findCoreOutRefIndex(orderedOutRefs, target);

/**
 * Finds the index of an outref in an ordered list or throws if absent.
 */
export const requireOutRefIndex = (
  orderedOutRefs: readonly OutRefLike[],
  target: OutRefLike,
): bigint => {
  const index = findOutRefIndex(orderedOutRefs, target);
  if (index === undefined) {
    throw new Error(
      `Failed to resolve ordered index for ${outRefLabel(target)}`,
    );
  }
  return BigInt(index);
};

/**
 * Resolves the canonical ledger input/reference-input index for a target outref
 * inside an unordered set.
 */
export const resolveOutRefIndexFromSet = (
  target: OutRefLike,
  outRefs: readonly OutRefLike[],
): bigint => requireOutRefIndex([...outRefs].sort(compareOutRefs), target);
