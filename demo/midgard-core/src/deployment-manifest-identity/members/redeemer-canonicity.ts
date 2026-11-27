import { defineDeploymentFamily } from "../members.types.js";

export const redeemerCanonicityMembers = defineDeploymentFamily(
  "redeemerCanonicity",
  {
    fraudProofRedeemerCanonicity: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof redeemer-canonicity step-01",
        token: "V1FpRedeemerCanonS01",
      },
      commands: [],
      validatorTitle: "fraud_proofs/redeemer_canonicity/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofRedeemerCanonicityStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof redeemer-canonicity step-02",
        token: "V1FpRedeemerCanonS02",
      },
      commands: [],
      validatorTitle: "fraud_proofs/redeemer_canonicity/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofRedeemerCanonicityStep02Data: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof redeemer-canonicity step-02 data",
        token: "V1FpRedeemerCanonS02Data",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/redeemer_canonicity/step_02_data.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofRedeemerCanonicityStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof redeemer-canonicity step-03",
        token: "V1FpRedeemerCanonS03",
      },
      commands: [],
      validatorTitle: "fraud_proofs/redeemer_canonicity/step_03.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
