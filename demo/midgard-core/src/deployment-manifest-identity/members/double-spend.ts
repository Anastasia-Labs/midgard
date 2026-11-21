import { defineDeploymentFamily } from "../members.types.js";

export const doubleSpendMembers = defineDeploymentFamily("doubleSpend", {
  fraudProofDoubleSpend: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof double-spend step-01",
      token: "V1FpDoubleSpendS01",
    },
    commands: [],
    validatorTitle: "fraud_proofs/double_spend/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofDoubleSpendStep02: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof double-spend step-02",
      token: "V1FpDoubleSpendS02",
    },
    commands: [],
    validatorTitle: "fraud_proofs/double_spend/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
  fraudProofDoubleSpendStep03: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof double-spend step-03",
      token: "V1FpDoubleSpendS03",
    },
    commands: [],
    validatorTitle: "fraud_proofs/double_spend/step_03.main.spend",
    familyPaths: [["steps", "2"]],
  },
  fraudProofDoubleSpendStep04: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof double-spend step-04",
      token: "V1FpDoubleSpendS04",
    },
    commands: [],
    validatorTitle: "fraud_proofs/double_spend/step_04.main.spend",
    familyPaths: [["steps", "3"]],
  },
});
