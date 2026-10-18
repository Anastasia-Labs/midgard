import { defineDeploymentFamily } from "../members.types.js";

export const spendInputSignerMissingMembers = defineDeploymentFamily(
  "spendInputSignerMissing",
  {
    fraudProofSpendInputSignerMissing: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof spend-input-signer-missing step-01",
      manifestPosition: 296,
      publicationPosition: 202,
      referenceRolePosition: 434,
      commands: [],
      validatorTitle:
        "fraud_proofs/spend_input_signer_missing/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofSpendInputSignerMissingStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof spend-input-signer-missing step-02",
      manifestPosition: 297,
      publicationPosition: 203,
      referenceRolePosition: 435,
      commands: [],
      validatorTitle:
        "fraud_proofs/spend_input_signer_missing/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofSpendInputSignerMissingStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof spend-input-signer-missing step-03",
      manifestPosition: 298,
      publicationPosition: 204,
      referenceRolePosition: 436,
      commands: [],
      validatorTitle:
        "fraud_proofs/spend_input_signer_missing/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofSpendInputSignerMissingStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof spend-input-signer-missing step-04",
      manifestPosition: 299,
      publicationPosition: 205,
      referenceRolePosition: 437,
      commands: [],
      validatorTitle:
        "fraud_proofs/spend_input_signer_missing/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofSpendInputSignerMissingStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof spend-input-signer-missing step-05",
      manifestPosition: 300,
      publicationPosition: 206,
      referenceRolePosition: 438,
      commands: [],
      validatorTitle:
        "fraud_proofs/spend_input_signer_missing/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
  },
);
