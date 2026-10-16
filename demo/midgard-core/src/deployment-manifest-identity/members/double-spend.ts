import { defineDeploymentFamily } from "../members.types.js";

export const doubleSpendMembers = defineDeploymentFamily("doubleSpend", {
  fraudProofDoubleSpend: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof double-spend step-01",
    manifestPosition: 47,
    publicationPosition: 65,
    referenceRolePosition: 142,
    commands: [],
    validatorTitle: "fraud_proofs/double_spend/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofDoubleSpendStep02: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof double-spend step-02",
    manifestPosition: 405,
    publicationPosition: 66,
    referenceRolePosition: 143,
    commands: [],
    validatorTitle: "fraud_proofs/double_spend/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
  fraudProofDoubleSpendStep03: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof double-spend step-03",
    manifestPosition: 406,
    publicationPosition: 67,
    referenceRolePosition: 144,
    commands: [],
    validatorTitle: "fraud_proofs/double_spend/step_03.main.spend",
    familyPaths: [["steps", "2"]],
  },
  fraudProofDoubleSpendStep04: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof double-spend step-04",
    manifestPosition: 407,
    publicationPosition: 68,
    referenceRolePosition: 145,
    commands: [],
    validatorTitle: "fraud_proofs/double_spend/step_04.main.spend",
    familyPaths: [["steps", "3"]],
  },
});
