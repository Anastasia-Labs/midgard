import { defineDeploymentFamily } from "../members.types.js";

export const withdrawnInputMembers = defineDeploymentFamily("withdrawnInput", {
  fraudProofWithdrawnInput: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof withdrawn-input step-01",
      token: "V1FpWithdrawnInputS01",
    },
    commands: [],
    validatorTitle: "fraud_proofs/withdrawn_input/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofWithdrawnInputStep02: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof withdrawn-input step-02",
      token: "V1FpWithdrawnInputS02",
    },
    commands: [],
    validatorTitle: "fraud_proofs/withdrawn_input/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
  fraudProofWithdrawnInputStep03: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof withdrawn-input step-03",
      token: "V1FpWithdrawnInputS03",
    },
    commands: [],
    validatorTitle: "fraud_proofs/withdrawn_input/step_03.main.spend",
    familyPaths: [["steps", "2"]],
  },
});
