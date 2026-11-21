import { defineDeploymentFamily } from "../members.types.js";

export const inputSetUniquenessMembers = defineDeploymentFamily(
  "inputSetUniqueness",
  {
    fraudProofInputSetUniqueness: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof input-set-uniqueness step-01",
        token: "V1FpInputSetUniquenessS01",
      },
      commands: [],
      validatorTitle: "fraud_proofs/input_set_uniqueness/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofInputSetUniquenessStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof input-set-uniqueness step-02",
        token: "V1FpInputSetUniquenessS02",
      },
      commands: [],
      validatorTitle: "fraud_proofs/input_set_uniqueness/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofInputSetUniquenessStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof input-set-uniqueness step-03",
        token: "V1FpInputSetUniquenessS03",
      },
      commands: [],
      validatorTitle: "fraud_proofs/input_set_uniqueness/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofInputSetUniquenessStep04: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof input-set-uniqueness step-04",
        token: "V1FpInputSetUniquenessS04",
      },
      commands: [],
      validatorTitle: "fraud_proofs/input_set_uniqueness/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
