import { defineDeploymentFamily } from "../members.types.js";

export const protectedOutputSignerMissingMembers = defineDeploymentFamily(
  "protectedOutputSignerMissing",
  {
    fraudProofProtectedOutputSignerMissing: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof protected-output-signer-missing step-01",
        token: "V1FpProtectedSignerS01",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/protected_output_signer_missing/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofProtectedOutputSignerMissingStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof protected-output-signer-missing step-02",
        token: "V1FpProtectedSignerS02",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/protected_output_signer_missing/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofProtectedOutputSignerMissingStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof protected-output-signer-missing step-03",
        token: "V1FpProtectedSignerS03",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/protected_output_signer_missing/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofProtectedOutputSignerMissingStep04: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof protected-output-signer-missing step-04",
        token: "V1FpProtectedSignerS04",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/protected_output_signer_missing/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofProtectedOutputSignerMissingStep05: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof protected-output-signer-missing step-05",
        token: "V1FpProtectedSignerS05",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/protected_output_signer_missing/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
  },
);
