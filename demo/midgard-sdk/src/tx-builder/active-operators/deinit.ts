import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type DeinitParams = {};

/**
 * The program that performs the deinit of an operator.
 *
 * @param lucid - The LucidEvolution instance to use for the deinit.
 * @param params - The parameters required for deinit.
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const deinitTxBuilder = (
  lucid: LucidEvolution,
  params: DeinitParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
