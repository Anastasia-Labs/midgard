import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type NewCategoryParams = {};

/**
 * NewCategory
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const newCategoryTxBuilder = (
  lucid: LucidEvolution,
  params: NewCategoryParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
