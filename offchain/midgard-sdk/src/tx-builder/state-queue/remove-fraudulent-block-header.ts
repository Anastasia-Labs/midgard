import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type RemoveParams = {};

/**
 * Remove
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const removeTxBuilder = (
  lucid: LucidEvolution,
  params: RemoveParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
