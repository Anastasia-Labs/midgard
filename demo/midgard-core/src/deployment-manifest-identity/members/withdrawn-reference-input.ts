import { defineDeploymentFamily } from "../members.types.js";

export const withdrawnReferenceInputMembers = defineDeploymentFamily(
  "withdrawnReferenceInput",
  {
    fraudProofWithdrawnReferenceInput: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof withdrawn-reference-input step-01",
        token: "V1FpWithdrawnReferenceInputS01",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/withdrawn_reference_input/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofWithdrawnReferenceInputStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof withdrawn-reference-input step-02",
        token: "V1FpWithdrawnReferenceInputS02",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/withdrawn_reference_input/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofWithdrawnReferenceInputStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof withdrawn-reference-input step-03",
        token: "V1FpWithdrawnReferenceInputS03",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/withdrawn_reference_input/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
  },
);
