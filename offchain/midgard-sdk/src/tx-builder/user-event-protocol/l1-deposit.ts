import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type DepositParams = {};

/**
 * Deposit
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const depositTxBuilder = (
  lucid: LucidEvolution,
  params: DepositParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
