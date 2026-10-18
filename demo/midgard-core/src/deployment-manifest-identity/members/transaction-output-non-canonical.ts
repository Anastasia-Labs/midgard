import { defineDeploymentFamily } from "../members.types.js";

export const transactionOutputNonCanonicalMembers = defineDeploymentFamily(
  "transactionOutputNonCanonical",
  {
    fraudProofTransactionOutputNonCanonical: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transaction-output-non-canonical step-01",
      manifestPosition: 277,
      publicationPosition: 183,
      referenceRolePosition: 415,
      commands: [],
      validatorTitle:
        "fraud_proofs/transaction_output_non_canonical/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofTransactionOutputNonCanonicalStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transaction-output-non-canonical step-02",
      manifestPosition: 278,
      publicationPosition: 184,
      referenceRolePosition: 417,
      commands: [],
      validatorTitle:
        "fraud_proofs/transaction_output_non_canonical/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofTransactionOutputNonCanonicalStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transaction-output-non-canonical step-03",
      manifestPosition: 279,
      publicationPosition: 185,
      referenceRolePosition: 419,
      commands: [],
      validatorTitle:
        "fraud_proofs/transaction_output_non_canonical/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofTransactionOutputNonCanonicalStep03Datum: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof transaction-output-non-canonical step-03 datum",
      manifestPosition: 280,
      publicationPosition: 186,
      referenceRolePosition: 420,
      commands: [],
      validatorTitle:
        "fraud_proofs/transaction_output_non_canonical/step_03_datum.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofTransactionOutputNonCanonicalStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transaction-output-non-canonical step-04",
      manifestPosition: 281,
      publicationPosition: 187,
      referenceRolePosition: 422,
      commands: [],
      validatorTitle:
        "fraud_proofs/transaction_output_non_canonical/step_04.main.spend",
      familyPaths: [["steps", "4"]],
    },
  },
);
