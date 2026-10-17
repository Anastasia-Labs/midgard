import { defineDeploymentFamily } from "../members.types.js";

export const invalidRangeMembers = defineDeploymentFamily("invalidRange", {
  fraudProofInvalidRange: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof invalid-range step-01",
    manifestPosition: 50,
    publicationPosition: 77,
    referenceRolePosition: 154,
    commands: [],
    validatorTitle: "fraud_proofs/invalid_range/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofInvalidRangeStep02: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof invalid-range step-02",
    manifestPosition: 414,
    publicationPosition: 78,
    referenceRolePosition: 155,
    commands: [],
    validatorTitle: "fraud_proofs/invalid_range/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
});
