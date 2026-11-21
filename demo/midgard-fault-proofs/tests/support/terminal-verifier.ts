import {
  FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  type FraudProofWorkflowTerminalVerifier,
} from "../../src/workflow/orchestrator.fraud-proof-family-workflow-adapter.js";

/** A terminal verifier answering by `verify`. */
export const terminalVerifierOf = (
  verify: FraudProofWorkflowTerminalVerifier["verify"],
): FraudProofWorkflowTerminalVerifier => ({
  verifierVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_VERIFIER,
  verify,
});
