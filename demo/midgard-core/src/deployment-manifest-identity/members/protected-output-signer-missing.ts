import { defineDeploymentFamily } from "../members.types.js";

export const protectedOutputSignerMissingMembers = defineDeploymentFamily(
  "protectedOutputSignerMissing",
  {
    fraudProofProtectedOutputSignerMissing: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof protected-output-signer-missing step-01",
      manifestPosition: 301,
      publicationPosition: 207,
      referenceRolePosition: 439,
      commands: [],
      validatorTitle:
        "fraud_proofs/protected_output_signer_missing/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofProtectedOutputSignerMissingStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof protected-output-signer-missing step-02",
      manifestPosition: 302,
      publicationPosition: 208,
      referenceRolePosition: 440,
      commands: [],
      validatorTitle:
        "fraud_proofs/protected_output_signer_missing/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofProtectedOutputSignerMissingStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof protected-output-signer-missing step-03",
      manifestPosition: 303,
      publicationPosition: 209,
      referenceRolePosition: 441,
      commands: [],
      validatorTitle:
        "fraud_proofs/protected_output_signer_missing/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofProtectedOutputSignerMissingStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof protected-output-signer-missing step-04",
      manifestPosition: 304,
      publicationPosition: 210,
      referenceRolePosition: 442,
      commands: [],
      validatorTitle:
        "fraud_proofs/protected_output_signer_missing/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofProtectedOutputSignerMissingStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof protected-output-signer-missing step-05",
      manifestPosition: 305,
      publicationPosition: 211,
      referenceRolePosition: 443,
      commands: [],
      validatorTitle:
        "fraud_proofs/protected_output_signer_missing/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
  },
);
