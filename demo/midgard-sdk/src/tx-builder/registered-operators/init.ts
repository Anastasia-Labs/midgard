import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type InitParams = {};

/**
 * Init
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const initTxBuilder = (
  lucid: LucidEvolution,
  params: InitParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
