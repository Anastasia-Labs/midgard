import { defineDeploymentFamily } from "../members.types.js";

export const zeroInputMembers = defineDeploymentFamily("zeroInput", {
  fraudProofZeroInput: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof zero-input step-01",
    manifestPosition: 51,
    publicationPosition: 79,
    referenceRolePosition: 156,
    commands: [],
    validatorTitle: "fraud_proofs/zero_input/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofZeroInputStep02: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof zero-input step-02",
    manifestPosition: 415,
    publicationPosition: 80,
    referenceRolePosition: 157,
    commands: [],
    validatorTitle: "fraud_proofs/zero_input/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
});
