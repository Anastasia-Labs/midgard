import type {
  FraudProofWorkflowReconcileResult,
  FraudProofWorkflowTerminalVerifier,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import { reconcileSignedWorkflowTransaction } from "./signed-transaction-reconciliation.js";

/**
 * Where an exact signed attempt stands at the tip, read from its own bytes
 * through the workflow's authenticated L1 and never through its family, so
 * every family gets the same answer. Included within k is `confirmed`, and
 * deeper `pending` with its retirement; expired or invalidated is `not_found`,
 * with a retirement past k. Anything the chain does not settle, missing bytes
 * among it, reads `pending`, `unknown` or `conflict`.
 */
export const readSignedAttempt = async (
  verifier: Pick<
    FraudProofWorkflowTerminalVerifier,
    "observeSignedTransaction"
  >,
  {
    transactionHash,
    signedTransactionCborHex,
  }: {
    readonly transactionHash: string;
    readonly signedTransactionCborHex?: string | null;
  },
): Promise<FraudProofWorkflowReconcileResult> =>
  await reconcileSignedWorkflowTransaction({
    transactionHash,
    ...(signedTransactionCborHex === undefined ||
    signedTransactionCborHex === null
      ? {}
      : { signedTransactionCborHex }),
    observe: async (input) => await verifier.observeSignedTransaction(input),
    reportInclusion: true,
  });
