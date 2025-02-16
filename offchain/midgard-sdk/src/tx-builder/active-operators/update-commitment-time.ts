import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

export type UpdateCommitmentTimeParams = {};

/**
 * UpdateCommitmentTime
 *
 * @param lucid - The LucidEvolution
 * @param params - The parameters
 * @returns {TxBuilder} A TxBuilder instance that can be used to build the transaction.
 */
export const updateCommitmentTimeTxBuilder = (
  lucid: LucidEvolution,
  params: UpdateCommitmentTimeParams,
): TxBuilder => {
  const tx = lucid.newTx();
  return tx;
};
