import { defineDeploymentFamily } from "../members.types.js";

export const daHashPreimageMembers = defineDeploymentFamily("daHashPreimage", {
  fraudProofDaHashPreimage: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof da-hash-preimage step-01",
    manifestPosition: 52,
    publicationPosition: 87,
    referenceRolePosition: 164,
    commands: [],
    validatorTitle: "fraud_proofs/da_hash_preimage/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofDaHashPreimageStep02: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof da-hash-preimage step-02",
    manifestPosition: 422,
    publicationPosition: 88,
    referenceRolePosition: 165,
    commands: [],
    validatorTitle: "fraud_proofs/da_hash_preimage/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
});
