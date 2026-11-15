import { defineDeploymentFamily } from "../members.types.js";

export const fabricatedDepositMembers = defineDeploymentFamily(
  "fabricatedDeposit",
  {
    fraudProofFabricatedDeposit: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof fabricated-deposit step-01",
        token: "V1FpFabricatedDepositS01",
      },
      commands: [],
      validatorTitle: "fraud_proofs/fabricated_deposit/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofFabricatedDepositStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof fabricated-deposit step-02",
        token: "V1FpFabricatedDepositS02",
      },
      commands: [],
      validatorTitle: "fraud_proofs/fabricated_deposit/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofFabricatedDepositStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof fabricated-deposit step-03",
        token: "V1FpFabricatedDepositS03",
      },
      commands: [],
      validatorTitle: "fraud_proofs/fabricated_deposit/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofFabricatedDepositStep04: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof fabricated-deposit step-04",
        token: "V1FpFabricatedDepositS04",
      },
      commands: [],
      validatorTitle: "fraud_proofs/fabricated_deposit/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
