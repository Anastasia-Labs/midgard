/**
 * Canonical TxOutRef ordering and indexing helpers for the node.
 * This module keeps ledger-context input/reference-input ordering rules in one
 * place so workers and transaction builders do not reimplement them.
 */
import { CML, coreToTxOutput } from "@lucid-evolution/lucid";

/**
 * Lightweight transaction-output reference shape used for ordering/indexing.
 */
export type OutRefLike = {
  readonly txHash: string;
  readonly outputIndex: number;
};

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
export const compareOutRefs = (a: OutRefLike, b: OutRefLike): number => {
  const txHashComparison = Buffer.from(a.txHash, "hex").compare(
    Buffer.from(b.txHash, "hex"),
  );
  if (txHashComparison !== 0) {
    return txHashComparison;
  }
  return a.outputIndex - b.outputIndex;
};

/**
 * Formats an outref as `txHash#outputIndex`.
 */
export const outRefLabel = (outRef: OutRefLike): string =>
  `${outRef.txHash}#${outRef.outputIndex.toString()}`;

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
): number | undefined => {
  for (let index = 0; index < orderedOutRefs.length; index += 1) {
    const candidate = orderedOutRefs[index]!;
    if (
      candidate.txHash === target.txHash &&
      candidate.outputIndex === target.outputIndex
    ) {
      return index;
    }
  }
  return undefined;
};

/**
 * Finds the index of an outref in an ordered list or throws if absent.
 */
export const requireOutRefIndex = (
  orderedOutRefs: readonly OutRefLike[],
  target: OutRefLike,
): bigint => {
  const index = findOutRefIndex(orderedOutRefs, target);
  if (index === undefined) {
    throw new Error(`Failed to resolve ordered index for ${outRefLabel(target)}`);
  }
  return BigInt(index);
};

/**
 * Resolves the canonical reference-input index for a target outref inside an
 * unordered reference-input set.
 */
export const resolveReferenceInputIndexFromSet = (
  target: OutRefLike,
  referenceInputs: readonly OutRefLike[],
): bigint =>
  requireOutRefIndex([...referenceInputs].sort(compareOutRefs), target);
