import { defineDeploymentFamily } from "../members.types.js";

export const redeemerCanonicityMembers = defineDeploymentFamily(
  "redeemerCanonicity",
  {
    fraudProofRedeemerCanonicity: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof redeemer-canonicity step-01",
      manifestPosition: 311,
      publicationPosition: 217,
      referenceRolePosition: 465,
      commands: [],
      validatorTitle: "fraud_proofs/redeemer_canonicity/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofRedeemerCanonicityStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof redeemer-canonicity step-02",
      manifestPosition: 312,
      publicationPosition: 218,
      referenceRolePosition: 466,
      commands: [],
      validatorTitle: "fraud_proofs/redeemer_canonicity/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofRedeemerCanonicityStep02Data: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof redeemer-canonicity step-02 data",
      manifestPosition: 313,
      publicationPosition: 219,
      referenceRolePosition: 467,
      commands: [],
      validatorTitle:
        "fraud_proofs/redeemer_canonicity/step_02_data.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofRedeemerCanonicityStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof redeemer-canonicity step-03",
      manifestPosition: 314,
      publicationPosition: 220,
      referenceRolePosition: 468,
      commands: [],
      validatorTitle: "fraud_proofs/redeemer_canonicity/step_03.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
