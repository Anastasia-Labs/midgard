import { LucidEvolution, TxBuilder } from "@lucid-evolution/lucid";

/**
 * Placeholder parameters for transaction-order requests.
 *
 * The type stays explicit so future additions do not silently widen the API.
 */
export type TransactionOrderParams = {};

/**
 * Starts building a transaction-order user event.
 *
 * The current implementation is a stub that only returns a fresh builder; the
 * explicit wrapper keeps the public SDK surface stable while the order payload
 * format evolves.
 */
export const transactionOrderTxBuilder = (
  lucid: LucidEvolution,
  params: TransactionOrderParams,
): TxBuilder => {
  void params;
  const tx = lucid.newTx();
  return tx;
};
