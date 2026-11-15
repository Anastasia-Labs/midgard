import { defineDeploymentFamily } from "../members.types.js";

export const transactionOutputNonCanonicalMembers = defineDeploymentFamily(
  "transactionOutputNonCanonical",
  {
    fraudProofTransactionOutputNonCanonical: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transaction-output-non-canonical step-01",
      commands: [],
      validatorTitle:
        "fraud_proofs/transaction_output_non_canonical/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofTransactionOutputNonCanonicalStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transaction-output-non-canonical step-02",
      commands: [],
      validatorTitle:
        "fraud_proofs/transaction_output_non_canonical/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofTransactionOutputNonCanonicalStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transaction-output-non-canonical step-03",
      commands: [],
      validatorTitle:
        "fraud_proofs/transaction_output_non_canonical/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofTransactionOutputNonCanonicalStep03Datum: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof transaction-output-non-canonical step-03 datum",
      commands: [],
      validatorTitle:
        "fraud_proofs/transaction_output_non_canonical/step_03_datum.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofTransactionOutputNonCanonicalStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transaction-output-non-canonical step-04",
      commands: [],
      validatorTitle:
        "fraud_proofs/transaction_output_non_canonical/step_04.main.spend",
      familyPaths: [["steps", "4"]],
    },
  },
);
