import {
  FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
  type FraudProofWorkflowTerminal,
} from "@al-ft/midgard-fault-proofs";

export const fundingTerminal = (
  headerHash: string,
  proofTxHash: string,
  removalTxHash: string,
): FraudProofWorkflowTerminal => {
  return {
    schemaVersion: FRAUD_PROOF_WORKFLOW_TERMINAL_SCHEMA_VERSION,
    category: "doubleSpend" as const,
    headerHash: headerHash,
    proofToken: {
      unit: "11".repeat(28),
      outRef: `${proofTxHash}#0`,
      createdByTxHash: proofTxHash,
      retainedAtFinalState: true as const,
    },
    correction: {
      removalTxHash: removalTxHash,
      removedStateQueueOutRef: `${"cc".repeat(32)}#0`,
      fraudulentHeaderAbsent: true as const,
      referencedProofTokenOutRef: `${proofTxHash}#0`,
    },
    economics: {
      operatorCredential: "22".repeat(28),
      proverCredential: "33".repeat(28),
      operatorBondInputOutRef: `${"dd".repeat(32)}#0`,
      operatorBondInputLovelace: "10000000",
      slashedLovelace: "10000000",
      proverRewardOutputOutRef: `${removalTxHash}#0`,
      proverRewardLovelace: "5000000",
      removalFeeLovelace: "200000",
      duplicateRewardAbsent: true as const,
    },
    observedAt: {
      slot: "4242",
      blockHash: "44".repeat(32),
      confirmationDepth: 30,
    },
  };
};
