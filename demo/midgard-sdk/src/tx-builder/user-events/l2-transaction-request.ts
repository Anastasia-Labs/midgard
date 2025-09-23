import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type TransactionRequestParams = {};

/**
 * TransactionRequest
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const transactionRequestTxBuilder = (
  lucid: LucidEvolution,
  params: TransactionRequestParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
