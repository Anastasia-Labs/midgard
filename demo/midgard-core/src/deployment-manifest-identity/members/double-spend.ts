import { defineDeploymentFamily } from "../members.types.js";

export const doubleSpendMembers = defineDeploymentFamily("doubleSpend", {
  fraudProofDoubleSpend: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof double-spend step-01",
    manifestPosition: 48,
    publicationPosition: 72,
    referenceRolePosition: 149,
    commands: [],
    validatorTitle: "fraud_proofs/double_spend/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofDoubleSpendStep02: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof double-spend step-02",
    manifestPosition: 460,
    publicationPosition: 73,
    referenceRolePosition: 150,
    commands: [],
    validatorTitle: "fraud_proofs/double_spend/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
  fraudProofDoubleSpendStep03: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof double-spend step-03",
    manifestPosition: 461,
    publicationPosition: 74,
    referenceRolePosition: 151,
    commands: [],
    validatorTitle: "fraud_proofs/double_spend/step_03.main.spend",
    familyPaths: [["steps", "2"]],
  },
  fraudProofDoubleSpendStep04: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof double-spend step-04",
    manifestPosition: 462,
    publicationPosition: 75,
    referenceRolePosition: 152,
    commands: [],
    validatorTitle: "fraud_proofs/double_spend/step_04.main.spend",
    familyPaths: [["steps", "3"]],
  },
});
