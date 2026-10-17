import { defineDeploymentFamily } from "../members.types.js";

export const doubleWithdrawMembers = defineDeploymentFamily("doubleWithdraw", {
  fraudProofDoubleWithdraw: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof double-withdraw step-01",
    manifestPosition: 219,
    publicationPosition: 125,
    referenceRolePosition: 90,
    commands: [],
    validatorTitle: "fraud_proofs/double_withdraw/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofDoubleWithdrawStep02: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof double-withdraw step-02",
    manifestPosition: 220,
    publicationPosition: 126,
    referenceRolePosition: 91,
    commands: [],
    validatorTitle: "fraud_proofs/double_withdraw/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
});
