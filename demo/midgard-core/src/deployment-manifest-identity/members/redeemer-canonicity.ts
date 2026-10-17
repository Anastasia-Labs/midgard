import { defineDeploymentFamily } from "../members.types.js";

export const redeemerCanonicityMembers = defineDeploymentFamily(
  "redeemerCanonicity",
  {
    fraudProofRedeemerCanonicity: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof redeemer-canonicity step-01",
      manifestPosition: 305,
      publicationPosition: 211,
      referenceRolePosition: 457,
      commands: [],
      validatorTitle: "fraud_proofs/redeemer_canonicity/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofRedeemerCanonicityStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof redeemer-canonicity step-02",
      manifestPosition: 306,
      publicationPosition: 212,
      referenceRolePosition: 458,
      commands: [],
      validatorTitle: "fraud_proofs/redeemer_canonicity/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofRedeemerCanonicityStep02Data: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof redeemer-canonicity step-02 data",
      manifestPosition: 307,
      publicationPosition: 213,
      referenceRolePosition: 459,
      commands: [],
      validatorTitle:
        "fraud_proofs/redeemer_canonicity/step_02_data.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofRedeemerCanonicityStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof redeemer-canonicity step-03",
      manifestPosition: 308,
      publicationPosition: 214,
      referenceRolePosition: 460,
      commands: [],
      validatorTitle: "fraud_proofs/redeemer_canonicity/step_03.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
