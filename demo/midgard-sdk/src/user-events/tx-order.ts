import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type TransactionOrderParams = {};

/**
 * TransactionOrder
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const transactionOrderTxBuilder = (
  lucid: LucidEvolution,
  params: TransactionOrderParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
