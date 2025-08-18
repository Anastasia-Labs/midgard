import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

/**
 * Parameters for the burn transaction.
 */
export type BurnParams = {};

/**
 * Creates a burn transaction builder.
 *
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {BurnParams} params - The parameters for the burn transaction.
 * @returns {TxBuilder} The transaction builder.
 */
export const burnTxBuilder = (
  lucid: LucidEvolution,
  params: BurnParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
