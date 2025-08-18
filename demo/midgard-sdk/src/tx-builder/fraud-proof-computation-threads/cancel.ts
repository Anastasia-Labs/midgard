import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type CancelParams = {};

/**
 * Cancel
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const cancelTxBuilder = (
  lucid: LucidEvolution,
  params: CancelParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
