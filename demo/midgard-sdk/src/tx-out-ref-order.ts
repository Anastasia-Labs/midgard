import { compareOutRefs, outRefLabel } from "@al-ft/midgard-core/out-ref";
import { type UTxO } from "@lucid-evolution/lucid";

/**
 * Deduplicates UTxOs by outref, then applies canonical ledger ordering:
 * lexicographic transaction hash bytes, then output index.
 */
export const dedupeAndSortUtxos = (utxos: readonly UTxO[]): UTxO[] =>
  [...new Map(utxos.map((utxo) => [outRefLabel(utxo), utxo])).values()].sort(
    compareOutRefs,
  );
