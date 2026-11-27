import type { UTxO } from "@lucid-evolution/lucid";

/**
 * Whether `utxo` has the shape the authenticated publication observer accepts
 * for carriage without a token: an inline datum, no datum hash, no reference
 * script, and nothing but ada. Resolvers that select carriage for a step skip
 * any other output, because the step would refuse it at authentication.
 */
export const isAdaOnlyInlineDatumOutput = (
  utxo: UTxO,
): utxo is UTxO & { readonly datum: string } =>
  typeof utxo.datum === "string" &&
  utxo.datumHash == null &&
  utxo.scriptRef == null &&
  Object.entries(utxo.assets).every(
    ([asset, quantity]) => asset === "lovelace" || quantity === 0n,
  );
