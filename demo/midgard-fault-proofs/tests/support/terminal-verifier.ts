import {
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  type FraudProofWorkflowTerminalVerifier,
} from "../../src/workflow/orchestrator.js";

/** A read of a superseded attempt's signed bytes no row here expects. */
export const noSignedRead = async (): Promise<never> => {
  throw new Error("no signed transaction read expected");
};

/** A terminal verifier for a workflow that reads no superseded attempt. */
export const terminalVerifierOf = (
  verify: FraudProofWorkflowTerminalVerifier["verify"],
): FraudProofWorkflowTerminalVerifier => ({
  verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  verify,
  observeSignedTransaction: noSignedRead,
});
