import { defineDeploymentFamily } from "../members.types.js";

export const invalidRangeMembers = defineDeploymentFamily("invalidRange", {
  fraudProofInvalidRange: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof invalid-range step-01",
    manifestPosition: 51,
    publicationPosition: 84,
    referenceRolePosition: 161,
    commands: [],
    validatorTitle: "fraud_proofs/invalid_range/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofInvalidRangeStep02: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof invalid-range step-02",
    manifestPosition: 453,
    publicationPosition: 85,
    referenceRolePosition: 162,
    commands: [],
    validatorTitle: "fraud_proofs/invalid_range/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
});
