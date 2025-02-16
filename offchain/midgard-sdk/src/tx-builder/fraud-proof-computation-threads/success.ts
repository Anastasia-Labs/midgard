import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type SuccessParams = {};

/**
 * Success
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const successTxBuilder = (
  lucid: LucidEvolution,
  params: SuccessParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
