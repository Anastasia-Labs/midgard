import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type RecoverParams = {};

/**
 * Recover
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const recoverTxBuilder = (
  lucid: LucidEvolution,
  params: RecoverParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
