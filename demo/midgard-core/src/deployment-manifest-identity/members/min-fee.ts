import { defineDeploymentFamily } from "../members.types.js";

export const minFeeMembers = defineDeploymentFamily("minFee", {
  fraudProofMinFee: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof min-fee step-01",
    manifestPosition: 218,
    publicationPosition: 124,
    referenceRolePosition: 89,
    commands: [],
    validatorTitle: "fraud_proofs/min_fee/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofMinFeeStep02: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof min-fee step-02",
    manifestPosition: 219,
    publicationPosition: 125,
    referenceRolePosition: 90,
    commands: [],
    validatorTitle: "fraud_proofs/min_fee/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
});
