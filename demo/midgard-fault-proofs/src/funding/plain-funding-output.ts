import type { UTxO } from "@lucid-evolution/lucid";

/**
 * Whether `utxo` is a plain coin the prover may spend to fund a transaction:
 * no datum, no datum hash, no reference script, and nothing but ada. Our own
 * carriage (which holds an inline datum) and any output carrying other
 * assets stay out of every coin selection.
 */
export const isPlainFundingOutput = (utxo: UTxO): boolean =>
  utxo.datum == null &&
  utxo.datumHash == null &&
  utxo.scriptRef == null &&
  Object.entries(utxo.assets).every(
    ([unit, quantity]) => unit === "lovelace" || quantity === 0n,
  );
