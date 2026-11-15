import { defineDeploymentFamily } from "../members.types.js";

export const doubleWithdrawMembers = defineDeploymentFamily("doubleWithdraw", {
  fraudProofDoubleWithdraw: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof double-withdraw step-01",
      token: "V1FpDoubleWithdrawS01",
    },
    commands: [],
    validatorTitle: "fraud_proofs/double_withdraw/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofDoubleWithdrawStep02: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof double-withdraw step-02",
      token: "V1FpDoubleWithdrawS02",
    },
    commands: [],
    validatorTitle: "fraud_proofs/double_withdraw/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
});
