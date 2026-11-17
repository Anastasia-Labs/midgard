import { defineDeploymentFamily } from "../members.types.js";

export const spendInputSignerMissingMembers = defineDeploymentFamily(
  "spendInputSignerMissing",
  {
    fraudProofSpendInputSignerMissing: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof spend-input-signer-missing step-01",
        token: "V1FpSpendSignerS01",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/spend_input_signer_missing/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofSpendInputSignerMissingStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof spend-input-signer-missing step-02",
        token: "V1FpSpendSignerS02",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/spend_input_signer_missing/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofSpendInputSignerMissingStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof spend-input-signer-missing step-03",
        token: "V1FpSpendSignerS03",
      },
      commands: [],
      validatorTitle: "fraud_proofs/signer_witness_scan/scan.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofSpendInputSignerMissingStep04: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof spend-input-signer-missing step-04",
        token: "V1FpSpendSignerS04",
      },
      commands: [],
      validatorTitle: "fraud_proofs/signer_witness_scan/terminal.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
