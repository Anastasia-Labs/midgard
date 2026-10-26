import { defineDeploymentFamily } from "../members.types.js";

export const invalidSignatureMembers = defineDeploymentFamily(
  "invalidSignature",
  {
    fraudProofInvalidSignature: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof invalid-signature step-01",
      manifestPosition: 56,
      publicationPosition: 98,
      referenceRolePosition: 175,
      commands: [],
      validatorTitle: "fraud_proofs/invalid_signature/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofInvalidSignatureStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof invalid-signature step-02",
      manifestPosition: 509,
      publicationPosition: 99,
      referenceRolePosition: 176,
      commands: [],
      validatorTitle: "fraud_proofs/invalid_signature/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
  },
);
