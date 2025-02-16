import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type DeregisterParams = {};

/**
 * Deregister
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const deregisterTxBuilder = (
  lucid: LucidEvolution,
  params: DeregisterParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
