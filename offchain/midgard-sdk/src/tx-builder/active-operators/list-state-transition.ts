import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type ListStateTransitionParams = {};

/**
 * ListStateTransition
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const listStateTransitionTxBuilder = (
  lucid: LucidEvolution,
  params: ListStateTransitionParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
