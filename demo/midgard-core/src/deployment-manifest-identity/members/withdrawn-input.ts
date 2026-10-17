import { defineDeploymentFamily } from "../members.types.js";

export const withdrawnInputMembers = defineDeploymentFamily("withdrawnInput", {
  fraudProofWithdrawnInput: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof withdrawn-input step-01",
    manifestPosition: 224,
    publicationPosition: 130,
    referenceRolePosition: 95,
    commands: [],
    validatorTitle: "fraud_proofs/withdrawn_input/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofWithdrawnInputStep02: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof withdrawn-input step-02",
    manifestPosition: 225,
    publicationPosition: 131,
    referenceRolePosition: 96,
    commands: [],
    validatorTitle: "fraud_proofs/withdrawn_input/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
  fraudProofWithdrawnInputStep03: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof withdrawn-input step-03",
    manifestPosition: 226,
    publicationPosition: 132,
    referenceRolePosition: 97,
    commands: [],
    validatorTitle: "fraud_proofs/withdrawn_input/step_03.main.spend",
    familyPaths: [["steps", "2"]],
  },
});
