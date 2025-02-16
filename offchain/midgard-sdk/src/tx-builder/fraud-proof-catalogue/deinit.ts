import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type DeinitParams = {};

/**
 * Deinit
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const deinitTxBuilder = (
  lucid: LucidEvolution,
  params: DeinitParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
