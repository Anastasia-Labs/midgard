import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

/**
 * Placeholder parameters for withdrawal-order requests.
 */
export type WithdrawalOrderParams = {};

/**
 * Starts building a withdrawal-order user event.
 *
 * Like the transaction-order builder, this currently exposes a stable entry
 * point while the concrete withdrawal payload is filled in elsewhere.
 */
export const withdrawalOrderTxBuilder = (
  lucid: LucidEvolution,
  params: WithdrawalOrderParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};
