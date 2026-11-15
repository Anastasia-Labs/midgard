import { defineDeploymentFamily } from "../members.types.js";

export const noReferenceInputMembers = defineDeploymentFamily(
  "noReferenceInput",
  {
    fraudProofNoReferenceInput: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof no-reference-input step-01",
        token: "V1FpNoReferenceInputS01",
      },
      commands: [],
      validatorTitle: "fraud_proofs/no_reference_input/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofNoReferenceInputStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof no-reference-input step-02",
        token: "V1FpNoReferenceInputS02",
      },
      commands: [],
      validatorTitle: "fraud_proofs/no_reference_input/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofNoReferenceInputStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof no-reference-input step-03",
        token: "V1FpNoReferenceInputS03",
      },
      commands: [],
      validatorTitle: "fraud_proofs/no_reference_input/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofNoReferenceInputStep04: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof no-reference-input step-04",
        token: "V1FpNoReferenceInputS04",
      },
      commands: [],
      validatorTitle: "fraud_proofs/no_reference_input/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
