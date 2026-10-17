import { defineDeploymentFamily } from "../members.types.js";

export const witnessScriptDecodingMembers = defineDeploymentFamily(
  "witnessScriptDecoding",
  {
    fraudProofWitnessScriptDecoding: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof witness-script-decoding step-01",
      manifestPosition: 259,
      publicationPosition: 165,
      referenceRolePosition: 400,
      commands: [],
      validatorTitle: "fraud_proofs/witness_script_decoding/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofWitnessScriptDecodingStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof witness-script-decoding step-02",
      manifestPosition: 260,
      publicationPosition: 166,
      referenceRolePosition: 401,
      commands: [],
      validatorTitle: "fraud_proofs/witness_script_decoding/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofWitnessScriptDecodingStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof witness-script-decoding step-03",
      manifestPosition: 261,
      publicationPosition: 167,
      referenceRolePosition: 402,
      commands: [],
      validatorTitle: "fraud_proofs/witness_script_decoding/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofWitnessScriptDecodingStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof witness-script-decoding step-04",
      manifestPosition: 262,
      publicationPosition: 168,
      referenceRolePosition: 403,
      commands: [],
      validatorTitle: "fraud_proofs/witness_script_decoding/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
