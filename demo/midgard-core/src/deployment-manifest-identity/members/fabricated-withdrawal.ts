import { defineDeploymentFamily } from "../members.types.js";

export const fabricatedWithdrawalMembers = defineDeploymentFamily(
  "fabricatedWithdrawal",
  {
    fraudProofFabricatedWithdrawal: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof fabricated-withdrawal step-01",
        token: "V1FpFabricatedWithdrawalS01",
      },
      commands: [],
      validatorTitle: "fraud_proofs/fabricated_withdrawal/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofFabricatedWithdrawalStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof fabricated-withdrawal step-02",
        token: "V1FpFabricatedWithdrawalS02",
      },
      commands: [],
      validatorTitle: "fraud_proofs/fabricated_withdrawal/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofFabricatedWithdrawalStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof fabricated-withdrawal step-03",
        token: "V1FpFabricatedWithdrawalS03",
      },
      commands: [],
      validatorTitle: "fraud_proofs/fabricated_withdrawal/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofFabricatedWithdrawalStep04: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof fabricated-withdrawal step-04",
        token: "V1FpFabricatedWithdrawalS04",
      },
      commands: [],
      validatorTitle: "fraud_proofs/fabricated_withdrawal/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
