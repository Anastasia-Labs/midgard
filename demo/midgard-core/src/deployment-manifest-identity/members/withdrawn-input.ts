import { defineDeploymentFamily } from "../members.types.js";

export const withdrawnInputMembers = defineDeploymentFamily("withdrawnInput", {
  fraudProofWithdrawnInput: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof withdrawn-input step-01",
    manifestPosition: 230,
    publicationPosition: 136,
    referenceRolePosition: 101,
    commands: [],
    validatorTitle: "fraud_proofs/withdrawn_input/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofWithdrawnInputStep02: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof withdrawn-input step-02",
    manifestPosition: 231,
    publicationPosition: 137,
    referenceRolePosition: 102,
    commands: [],
    validatorTitle: "fraud_proofs/withdrawn_input/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
  fraudProofWithdrawnInputStep03: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof withdrawn-input step-03",
    manifestPosition: 232,
    publicationPosition: 138,
    referenceRolePosition: 103,
    commands: [],
    validatorTitle: "fraud_proofs/withdrawn_input/step_03.main.spend",
    familyPaths: [["steps", "2"]],
  },
});
