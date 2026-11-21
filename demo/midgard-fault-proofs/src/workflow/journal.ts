import "@al-ft/midgard-sdk/core";
export {
  computeFraudProofWorkflowId,
  FRAUD_PROOF_WORKFLOW_IDENTITY_SCHEMA_VERSION,
  FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  type FraudProofWorkflowIdentity,
  type FraudProofWorkflowTarget,
  type FraudProofWorkflowTerminal,
  journalJsonDigest,
  type JournalJsonObject,
  type JournalJsonPrimitive,
  type JournalJsonValue,
  normalizeFraudProofWorkflowIdentity,
  normalizeJournalJson,
} from "./journal.fraud-proof-workflow-terminal.js";
