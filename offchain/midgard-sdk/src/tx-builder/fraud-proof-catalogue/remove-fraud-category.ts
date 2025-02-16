import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type RemoveCategoryParams = {};

/**
 * RemoveCategory
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const removeCategoryTxBuilder = (
  lucid: LucidEvolution,
  params: RemoveCategoryParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
