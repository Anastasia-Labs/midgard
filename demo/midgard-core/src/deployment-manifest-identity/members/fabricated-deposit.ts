import { defineDeploymentFamily } from "../members.types.js";

export const fabricatedDepositMembers = defineDeploymentFamily(
  "fabricatedDeposit",
  {
    fraudProofFabricatedDeposit: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof fabricated-deposit step-01",
      manifestPosition: 193,
      publicationPosition: 99,
      referenceRolePosition: 61,
      commands: [],
      validatorTitle: "fraud_proofs/fabricated_deposit/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofFabricatedDepositStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof fabricated-deposit step-02",
      manifestPosition: 194,
      publicationPosition: 100,
      referenceRolePosition: 62,
      commands: [],
      validatorTitle: "fraud_proofs/fabricated_deposit/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofFabricatedDepositStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof fabricated-deposit step-03",
      manifestPosition: 195,
      publicationPosition: 101,
      referenceRolePosition: 63,
      commands: [],
      validatorTitle: "fraud_proofs/fabricated_deposit/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofFabricatedDepositStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof fabricated-deposit step-04",
      manifestPosition: 196,
      publicationPosition: 102,
      referenceRolePosition: 64,
      commands: [],
      validatorTitle: "fraud_proofs/fabricated_deposit/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
