import { defineDeploymentFamily } from "../members.types.js";

export const nonExistentInputNoIndexMembers = defineDeploymentFamily(
  "nonExistentInputNoIndex",
  {
    fraudProofNonExistentInputNoIndex: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof non-existent-input-no-index step-01",
      manifestPosition: 50,
      publicationPosition: 80,
      referenceRolePosition: 157,
      commands: [],
      validatorTitle: "fraud_proofs/input_no_idx/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofNonExistentInputNoIndexStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof non-existent-input-no-index step-02",
      manifestPosition: 418,
      publicationPosition: 81,
      referenceRolePosition: 158,
      commands: [],
      validatorTitle: "fraud_proofs/input_no_idx/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofNonExistentInputNoIndexStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof non-existent-input-no-index step-03",
      manifestPosition: 419,
      publicationPosition: 82,
      referenceRolePosition: 159,
      commands: [],
      validatorTitle: "fraud_proofs/input_no_idx/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofNonExistentInputNoIndexStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof non-existent-input-no-index step-04",
      manifestPosition: 420,
      publicationPosition: 83,
      referenceRolePosition: 160,
      commands: [],
      validatorTitle: "fraud_proofs/input_no_idx/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
