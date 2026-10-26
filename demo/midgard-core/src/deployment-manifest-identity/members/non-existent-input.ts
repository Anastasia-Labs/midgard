import { defineDeploymentFamily } from "../members.types.js";

export const nonExistentInputMembers = defineDeploymentFamily(
  "nonExistentInput",
  {
    fraudProofNonExistentInput: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof non-existent-input step-01",
      manifestPosition: 49,
      publicationPosition: 76,
      referenceRolePosition: 153,
      commands: [],
      validatorTitle: "fraud_proofs/no_input/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofNonExistentInputStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof non-existent-input step-02",
      manifestPosition: 494,
      publicationPosition: 77,
      referenceRolePosition: 154,
      commands: [],
      validatorTitle: "fraud_proofs/no_input/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofNonExistentInputStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof non-existent-input step-03",
      manifestPosition: 495,
      publicationPosition: 78,
      referenceRolePosition: 155,
      commands: [],
      validatorTitle: "fraud_proofs/no_input/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofNonExistentInputStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof non-existent-input step-04",
      manifestPosition: 496,
      publicationPosition: 79,
      referenceRolePosition: 156,
      commands: [],
      validatorTitle: "fraud_proofs/no_input/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
