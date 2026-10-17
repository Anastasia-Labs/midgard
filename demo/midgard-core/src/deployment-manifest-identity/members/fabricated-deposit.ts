import { defineDeploymentFamily } from "../members.types.js";

export const fabricatedDepositMembers = defineDeploymentFamily(
  "fabricatedDeposit",
  {
    fraudProofFabricatedDeposit: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof fabricated-deposit step-01",
      manifestPosition: 187,
      publicationPosition: 93,
      referenceRolePosition: 55,
      commands: [],
      validatorTitle: "fraud_proofs/fabricated_deposit/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofFabricatedDepositStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof fabricated-deposit step-02",
      manifestPosition: 188,
      publicationPosition: 94,
      referenceRolePosition: 56,
      commands: [],
      validatorTitle: "fraud_proofs/fabricated_deposit/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofFabricatedDepositStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof fabricated-deposit step-03",
      manifestPosition: 189,
      publicationPosition: 95,
      referenceRolePosition: 57,
      commands: [],
      validatorTitle: "fraud_proofs/fabricated_deposit/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofFabricatedDepositStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof fabricated-deposit step-04",
      manifestPosition: 190,
      publicationPosition: 96,
      referenceRolePosition: 58,
      commands: [],
      validatorTitle: "fraud_proofs/fabricated_deposit/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
