import { defineDeploymentFamily } from "../members.types.js";

export const spendInputSignerMissingMembers = defineDeploymentFamily(
  "spendInputSignerMissing",
  {
    fraudProofSpendInputSignerMissing: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof spend-input-signer-missing step-01",
      manifestPosition: 289,
      publicationPosition: 195,
      referenceRolePosition: 430,
      commands: [],
      validatorTitle:
        "fraud_proofs/spend_input_signer_missing/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofSpendInputSignerMissingStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof spend-input-signer-missing step-02",
      manifestPosition: 290,
      publicationPosition: 196,
      referenceRolePosition: 431,
      commands: [],
      validatorTitle:
        "fraud_proofs/spend_input_signer_missing/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofSpendInputSignerMissingStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof spend-input-signer-missing step-03",
      manifestPosition: 291,
      publicationPosition: 197,
      referenceRolePosition: 432,
      commands: [],
      validatorTitle:
        "fraud_proofs/spend_input_signer_missing/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofSpendInputSignerMissingStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof spend-input-signer-missing step-04",
      manifestPosition: 292,
      publicationPosition: 198,
      referenceRolePosition: 433,
      commands: [],
      validatorTitle:
        "fraud_proofs/spend_input_signer_missing/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofSpendInputSignerMissingStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof spend-input-signer-missing step-05",
      manifestPosition: 293,
      publicationPosition: 199,
      referenceRolePosition: 434,
      commands: [],
      validatorTitle:
        "fraud_proofs/spend_input_signer_missing/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
  },
);
