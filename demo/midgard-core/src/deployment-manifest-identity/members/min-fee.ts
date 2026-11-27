import { defineDeploymentFamily } from "../members.types.js";

export const minFeeMembers = defineDeploymentFamily("minFee", {
  fraudProofMinFee: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof min-fee step-01",
      token: "V1FpMinFeeS01",
    },
    commands: [],
    validatorTitle: "fraud_proofs/min_fee/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofMinFeeStep02: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof min-fee step-02",
      token: "V1FpMinFeeS02",
    },
    commands: [],
    validatorTitle: "fraud_proofs/min_fee/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
});
