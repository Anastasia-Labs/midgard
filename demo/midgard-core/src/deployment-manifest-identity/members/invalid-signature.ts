import { defineDeploymentFamily } from "../members.types.js";

export const invalidSignatureMembers = defineDeploymentFamily(
  "invalidSignature",
  {
    fraudProofInvalidSignature: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof invalid-signature step-01",
        token: "V1FpInvalidSignatureS01",
      },
      commands: [],
      validatorTitle: "fraud_proofs/invalid_signature/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofInvalidSignatureStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof invalid-signature step-02",
        token: "V1FpInvalidSignatureS02",
      },
      commands: [],
      validatorTitle: "fraud_proofs/invalid_signature/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
  },
);
