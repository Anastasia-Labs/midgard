import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type RewindParams = {};

/**
 * Rewind
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const rewindTxBuilder = (
  lucid: LucidEvolution,
  params: RewindParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
