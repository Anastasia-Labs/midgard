import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type ActivateParams = {};

/**
 * The program that performs the activation of an operator.
 *
 * @param lucid - The LucidEvolution instance to use for the activation.
 * @param params - The parameters required for activation.
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const activateTxBuilder = (
  lucid: LucidEvolution,
  params: ActivateParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
