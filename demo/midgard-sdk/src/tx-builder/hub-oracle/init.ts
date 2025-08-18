import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

/**
 * Parameters for the init transaction.
 */
export type InitParams = {};

/**
 * Creates a init transaction builder.
 *
 * @param {LucidEvolution} lucid - The LucidEvolution instance.
 * @param {InitParams} params - The parameters for the init transaction.
 * @returns {TxBuilder} The transaction builder.
 */
export const initTxBuilder = (
  lucid: LucidEvolution,
  params: InitParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
