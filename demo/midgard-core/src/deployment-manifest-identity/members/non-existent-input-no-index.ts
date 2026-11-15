import { defineDeploymentFamily } from "../members.types.js";

export const nonExistentInputNoIndexMembers = defineDeploymentFamily(
  "nonExistentInputNoIndex",
  {
    fraudProofNonExistentInputNoIndex: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof non-existent-input-no-index step-01",
        token: "V1FpNonExistentInputNoIdxS01",
      },
      commands: [],
      validatorTitle: "fraud_proofs/input_no_idx/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofNonExistentInputNoIndexStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof non-existent-input-no-index step-02",
        token: "V1FpNonExistentInputNoIdxS02",
      },
      commands: [],
      validatorTitle: "fraud_proofs/input_no_idx/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofNonExistentInputNoIndexStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof non-existent-input-no-index step-03",
        token: "V1FpNonExistentInputNoIdxS03",
      },
      commands: [],
      validatorTitle: "fraud_proofs/input_no_idx/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofNonExistentInputNoIndexStep04: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof non-existent-input-no-index step-04",
        token: "V1FpNonExistentInputNoIdxS04",
      },
      commands: [],
      validatorTitle: "fraud_proofs/input_no_idx/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
