import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type RegisterParams = {};

/**
 * Register
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const registerTxBuilder = (
  lucid: LucidEvolution,
  params: RegisterParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
