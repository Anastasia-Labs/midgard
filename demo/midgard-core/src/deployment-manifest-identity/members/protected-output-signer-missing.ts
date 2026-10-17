import { defineDeploymentFamily } from "../members.types.js";

export const protectedOutputSignerMissingMembers = defineDeploymentFamily(
  "protectedOutputSignerMissing",
  {
    fraudProofProtectedOutputSignerMissing: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof protected-output-signer-missing step-01",
      manifestPosition: 294,
      publicationPosition: 200,
      referenceRolePosition: 435,
      commands: [],
      validatorTitle:
        "fraud_proofs/protected_output_signer_missing/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofProtectedOutputSignerMissingStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof protected-output-signer-missing step-02",
      manifestPosition: 295,
      publicationPosition: 201,
      referenceRolePosition: 436,
      commands: [],
      validatorTitle:
        "fraud_proofs/protected_output_signer_missing/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofProtectedOutputSignerMissingStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof protected-output-signer-missing step-03",
      manifestPosition: 296,
      publicationPosition: 202,
      referenceRolePosition: 437,
      commands: [],
      validatorTitle:
        "fraud_proofs/protected_output_signer_missing/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofProtectedOutputSignerMissingStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof protected-output-signer-missing step-04",
      manifestPosition: 297,
      publicationPosition: 203,
      referenceRolePosition: 438,
      commands: [],
      validatorTitle:
        "fraud_proofs/protected_output_signer_missing/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofProtectedOutputSignerMissingStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof protected-output-signer-missing step-05",
      manifestPosition: 298,
      publicationPosition: 204,
      referenceRolePosition: 439,
      commands: [],
      validatorTitle:
        "fraud_proofs/protected_output_signer_missing/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
  },
);
