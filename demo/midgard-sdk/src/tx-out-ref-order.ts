import { UTxO } from "@lucid-evolution/lucid";

/**
 * Ledger ordering for transaction inputs and reference inputs:
 * lexicographic transaction hash bytes, then output index.
 */
export const compareUtxoOutRefs = (left: UTxO, right: UTxO): number => {
  const hashOrder = Buffer.from(left.txHash, "hex").compare(
    Buffer.from(right.txHash, "hex"),
  );
  if (hashOrder !== 0) {
    return hashOrder;
  }
  return left.outputIndex - right.outputIndex;
};

export const dedupeAndSortUtxos = (utxos: readonly UTxO[]): UTxO[] => {
  const byOutRef = new Map<string, UTxO>();
  for (const utxo of utxos) {
    byOutRef.set(`${utxo.txHash}#${utxo.outputIndex.toString()}`, utxo);
  }
  return [...byOutRef.values()].sort(compareUtxoOutRefs);
};
