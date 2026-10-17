import { defineDeploymentFamily } from "../members.types.js";

export const transactionOutputNonCanonicalMembers = defineDeploymentFamily(
  "transactionOutputNonCanonical",
  {
    fraudProofTransactionOutputNonCanonical: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transaction-output-non-canonical step-01",
      manifestPosition: 270,
      publicationPosition: 176,
      referenceRolePosition: 411,
      commands: [],
      validatorTitle:
        "fraud_proofs/transaction_output_non_canonical/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofTransactionOutputNonCanonicalStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transaction-output-non-canonical step-02",
      manifestPosition: 271,
      publicationPosition: 177,
      referenceRolePosition: 413,
      commands: [],
      validatorTitle:
        "fraud_proofs/transaction_output_non_canonical/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofTransactionOutputNonCanonicalStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transaction-output-non-canonical step-03",
      manifestPosition: 272,
      publicationPosition: 178,
      referenceRolePosition: 415,
      commands: [],
      validatorTitle:
        "fraud_proofs/transaction_output_non_canonical/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofTransactionOutputNonCanonicalStep03Datum: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof transaction-output-non-canonical step-03 datum",
      manifestPosition: 273,
      publicationPosition: 179,
      referenceRolePosition: 416,
      commands: [],
      validatorTitle:
        "fraud_proofs/transaction_output_non_canonical/step_03_datum.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofTransactionOutputNonCanonicalStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof transaction-output-non-canonical step-04",
      manifestPosition: 274,
      publicationPosition: 180,
      referenceRolePosition: 418,
      commands: [],
      validatorTitle:
        "fraud_proofs/transaction_output_non_canonical/step_04.main.spend",
      familyPaths: [["steps", "4"]],
    },
  },
);
