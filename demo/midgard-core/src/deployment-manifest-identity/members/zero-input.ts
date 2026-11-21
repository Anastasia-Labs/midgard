import { defineDeploymentFamily } from "../members.types.js";

export const zeroInputMembers = defineDeploymentFamily("zeroInput", {
  fraudProofZeroInput: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof zero-input step-01",
      token: "V1FpZeroInputS01",
    },
    commands: [],
    validatorTitle: "fraud_proofs/zero_input/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofZeroInputStep02: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof zero-input step-02",
      token: "V1FpZeroInputS02",
    },
    commands: [],
    validatorTitle: "fraud_proofs/zero_input/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
});
