import { defineDeploymentFamily } from "../members.types.js";

export const withdrawnInputMembers = defineDeploymentFamily("withdrawnInput", {
  fraudProofWithdrawnInput: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof withdrawn-input step-01",
    commands: [],
    validatorTitle: "fraud_proofs/withdrawn_input/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofWithdrawnInputStep02: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof withdrawn-input step-02",
    commands: [],
    validatorTitle: "fraud_proofs/withdrawn_input/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
  fraudProofWithdrawnInputStep03: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof withdrawn-input step-03",
    commands: [],
    validatorTitle: "fraud_proofs/withdrawn_input/step_03.main.spend",
    familyPaths: [["steps", "2"]],
  },
});
