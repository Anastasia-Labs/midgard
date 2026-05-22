import {
  compareOutRefs,
  findOutRefIndex as findCoreOutRefIndex,
  outRefLabel,
  type OutRefLike,
} from "@al-ft/midgard-core/out-ref";
import { CML, coreToTxOutput, type UTxO } from "@lucid-evolution/lucid";

/**
 * Ledger ordering for transaction inputs and reference inputs:
 * lexicographic transaction hash bytes, then output index.
 */
export const dedupeAndSortUtxos = (utxos: readonly UTxO[]): UTxO[] =>
  [...new Map(utxos.map((utxo) => [outRefLabel(utxo), utxo])).values()].sort(
    compareOutRefs,
  );

export type IndexedTxOutput = ReturnType<typeof coreToTxOutput> & {
  readonly index: number;
};

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

export const findOutRefIndex = (
  orderedOutRefs: readonly OutRefLike[],
  target: OutRefLike,
): number | undefined => findCoreOutRefIndex(orderedOutRefs, target);

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

export const resolveOutRefIndexFromSet = (
  target: OutRefLike,
  outRefs: readonly OutRefLike[],
): bigint => requireOutRefIndex([...outRefs].sort(compareOutRefs), target);
