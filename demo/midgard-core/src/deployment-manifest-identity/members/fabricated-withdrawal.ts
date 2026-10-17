import { defineDeploymentFamily } from "../members.types.js";

export const fabricatedWithdrawalMembers = defineDeploymentFamily(
  "fabricatedWithdrawal",
  {
    fraudProofFabricatedWithdrawal: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof fabricated-withdrawal step-01",
      manifestPosition: 192,
      publicationPosition: 98,
      referenceRolePosition: 60,
      commands: [],
      validatorTitle: "fraud_proofs/fabricated_withdrawal/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofFabricatedWithdrawalStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof fabricated-withdrawal step-02",
      manifestPosition: 193,
      publicationPosition: 99,
      referenceRolePosition: 61,
      commands: [],
      validatorTitle: "fraud_proofs/fabricated_withdrawal/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofFabricatedWithdrawalStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof fabricated-withdrawal step-03",
      manifestPosition: 194,
      publicationPosition: 100,
      referenceRolePosition: 62,
      commands: [],
      validatorTitle: "fraud_proofs/fabricated_withdrawal/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofFabricatedWithdrawalStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof fabricated-withdrawal step-04",
      manifestPosition: 195,
      publicationPosition: 101,
      referenceRolePosition: 63,
      commands: [],
      validatorTitle: "fraud_proofs/fabricated_withdrawal/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
