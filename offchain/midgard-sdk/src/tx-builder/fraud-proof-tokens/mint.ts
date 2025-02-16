import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type MintParams = {};

/**
 * Mint
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const mintTxBuilder = (
  lucid: LucidEvolution,
  params: MintParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
