import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type RemoveDeplicateParams = {};

/**
 * RemoveDeplicate
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const removeDeplicateTxBuilder = (
  lucid: LucidEvolution,
  params: RemoveDeplicateParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
