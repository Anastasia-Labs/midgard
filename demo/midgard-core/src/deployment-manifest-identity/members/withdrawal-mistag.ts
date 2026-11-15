import { defineDeploymentFamily } from "../members.types.js";

export const withdrawalMistagMembers = defineDeploymentFamily(
  "withdrawalMistag",
  {
    fraudProofWithdrawalMistag: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof withdrawal-mistag step-01",
        token: "V1FpWithdrawalMistagS01",
      },
      commands: [],
      validatorTitle: "fraud_proofs/withdrawal_mistag/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofWithdrawalMistagStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof withdrawal-mistag step-02",
        token: "V1FpWithdrawalMistagS02",
      },
      commands: [],
      validatorTitle: "fraud_proofs/withdrawal_mistag/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofWithdrawalMistagStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof withdrawal-mistag step-03",
        token: "V1FpWithdrawalMistagS03",
      },
      commands: [],
      validatorTitle: "fraud_proofs/withdrawal_mistag/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofWithdrawalMistagStep04: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof withdrawal-mistag step-04",
        token: "V1FpWithdrawalMistagS04",
      },
      commands: [],
      validatorTitle: "fraud_proofs/withdrawal_mistag/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofWithdrawalMistagStep05: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof withdrawal-mistag step-05",
        token: "V1FpWithdrawalMistagS05",
      },
      commands: [],
      validatorTitle: "fraud_proofs/withdrawal_mistag/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
  },
);
