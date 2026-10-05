import {
  computeFraudProofWorkflowId,
  type FraudProofWorkflowIdentity,
  normalizeFraudProofWorkflowIdentity,
} from "./journal.fraud-proof-workflow-terminal.js";
export const requireTxHash = (value: string, field: string): void => {
  if (!/^[0-9a-f]{64}$/u.test(value)) {
    throw new Error(`${field} must be 32-byte lowercase hex`);
  }
};
export const requireOutRef = (value: string, field: string): void => {
  if (!/^[0-9a-f]{64}#(0|[1-9][0-9]*)$/u.test(value)) {
    throw new Error(`${field} must be a canonical transaction outRef`);
  }
};

export const expectedWorkflowIdentityMatches = (
  workflowId: string,
  expectedIdentity?: FraudProofWorkflowIdentity,
): boolean =>
  expectedIdentity === undefined ||
  computeFraudProofWorkflowId(
    normalizeFraudProofWorkflowIdentity(expectedIdentity),
  ) === workflowId;
