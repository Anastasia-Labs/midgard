import type { LucidEvolution, UTxO } from "@lucid-evolution/lucid";

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

/**
 * `complete`'s wallet inputs for the wallet `lucid` has selected: its
 * plain coins alone (`isPlainFundingOutput`). A wallet offering nothing
 * else, such as the watcher's reserved wallet
 * (`restrictWorkflowFundingSigner`), gets no preset, so its own collateral
 * survives: Lucid takes collateral only from among preset inputs.
 */
export const plainWalletInputs = async (
  lucid: LucidEvolution,
): Promise<{ presetWalletInputs?: UTxO[] }> => {
  const utxos = await lucid.wallet().getUtxos();
  const plain = utxos.filter(isPlainFundingOutput);
  return plain.length === utxos.length ? {} : { presetWalletInputs: plain };
};
