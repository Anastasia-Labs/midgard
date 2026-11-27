import { defineDeploymentFamily } from "../members.types.js";

export const daHashPreimageMembers = defineDeploymentFamily("daHashPreimage", {
  fraudProofDaHashPreimage: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof da-hash-preimage step-01",
      token: "V1FpDaHashPreimageS01",
    },
    commands: [],
    validatorTitle: "fraud_proofs/da_hash_preimage/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofDaHashPreimageStep02: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof da-hash-preimage step-02",
      token: "V1FpDaHashPreimageS02",
    },
    commands: [],
    validatorTitle: "fraud_proofs/da_hash_preimage/step_02.main.spend",
    familyPaths: [["steps", "1"]],
  },
});
