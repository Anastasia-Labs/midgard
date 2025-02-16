import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type RemoveOperatorParams = {};

/**
 * RemoveOperator
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const removeOperatorTxBuilder = (
  lucid: LucidEvolution,
  params: RemoveOperatorParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
