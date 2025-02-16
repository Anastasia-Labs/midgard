import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type RetireParams = {};

/**
 * Retire
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const retireTxBuilder = (
  lucid: LucidEvolution,
  params: RetireParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
