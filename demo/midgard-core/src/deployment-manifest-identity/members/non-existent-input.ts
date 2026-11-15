import { defineDeploymentFamily } from "../members.types.js";

export const nonExistentInputMembers = defineDeploymentFamily(
  "nonExistentInput",
  {
    fraudProofNonExistentInput: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof non-existent-input step-01",
        token: "V1FpNonExistentInputS01",
      },
      commands: [],
      validatorTitle: "fraud_proofs/no_input/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofNonExistentInputStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof non-existent-input step-02",
        token: "V1FpNonExistentInputS02",
      },
      commands: [],
      validatorTitle: "fraud_proofs/no_input/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofNonExistentInputStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof non-existent-input step-03",
        token: "V1FpNonExistentInputS03",
      },
      commands: [],
      validatorTitle: "fraud_proofs/no_input/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofNonExistentInputStep04: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof non-existent-input step-04",
        token: "V1FpNonExistentInputS04",
      },
      commands: [],
      validatorTitle: "fraud_proofs/no_input/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
