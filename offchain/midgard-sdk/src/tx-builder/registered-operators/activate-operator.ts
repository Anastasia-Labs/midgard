import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type ActivateParams = {};

/**
 * Activate
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const activateTxBuilder = (
  lucid: LucidEvolution,
  params: ActivateParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
