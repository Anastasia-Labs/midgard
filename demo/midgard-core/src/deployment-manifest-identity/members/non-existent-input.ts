import { defineDeploymentFamily } from "../members.types.js";

export const nonExistentInputMembers = defineDeploymentFamily(
  "nonExistentInput",
  {
    fraudProofNonExistentInput: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof non-existent-input step-01",
      commands: [],
      validatorTitle: "fraud_proofs/no_input/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofNonExistentInputStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof non-existent-input step-02",
      commands: [],
      validatorTitle: "fraud_proofs/no_input/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofNonExistentInputStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof non-existent-input step-03",
      commands: [],
      validatorTitle: "fraud_proofs/no_input/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofNonExistentInputStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof non-existent-input step-04",
      commands: [],
      validatorTitle: "fraud_proofs/no_input/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
