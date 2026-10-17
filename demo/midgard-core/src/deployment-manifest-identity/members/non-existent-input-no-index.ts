import { defineDeploymentFamily } from "../members.types.js";

export const nonExistentInputNoIndexMembers = defineDeploymentFamily(
  "nonExistentInputNoIndex",
  {
    fraudProofNonExistentInputNoIndex: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof non-existent-input-no-index step-01",
      manifestPosition: 50,
      publicationPosition: 74,
      referenceRolePosition: 151,
      commands: [],
      validatorTitle: "fraud_proofs/input_no_idx/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofNonExistentInputNoIndexStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof non-existent-input-no-index step-02",
      manifestPosition: 412,
      publicationPosition: 75,
      referenceRolePosition: 152,
      commands: [],
      validatorTitle: "fraud_proofs/input_no_idx/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofNonExistentInputNoIndexStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof non-existent-input-no-index step-03",
      manifestPosition: 413,
      publicationPosition: 76,
      referenceRolePosition: 153,
      commands: [],
      validatorTitle: "fraud_proofs/input_no_idx/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofNonExistentInputNoIndexStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof non-existent-input-no-index step-04",
      manifestPosition: 414,
      publicationPosition: 77,
      referenceRolePosition: 154,
      commands: [],
      validatorTitle: "fraud_proofs/input_no_idx/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
