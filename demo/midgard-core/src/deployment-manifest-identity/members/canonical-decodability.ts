import { defineDeploymentFamily } from "../members.types.js";

export const canonicalDecodabilityMembers = defineDeploymentFamily(
  "canonicalDecodability",
  {
    fraudProofCanonicalDecodability: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof canonical-decodability step-01",
      manifestPosition: 208,
      publicationPosition: 114,
      referenceRolePosition: 79,
      commands: [],
      validatorTitle: "fraud_proofs/canonical_decodability/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofCanonicalDecodabilityStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof canonical-decodability step-02",
      manifestPosition: 209,
      publicationPosition: 115,
      referenceRolePosition: 80,
      commands: [],
      validatorTitle: "fraud_proofs/canonical_decodability/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
  },
);
