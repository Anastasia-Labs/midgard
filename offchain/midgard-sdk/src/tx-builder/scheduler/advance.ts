import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type AdvanceParams = {};

/**
 * Advance
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const advanceTxBuilder = (
  lucid: LucidEvolution,
  params: AdvanceParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
