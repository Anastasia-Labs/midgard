import { defineDeploymentFamily } from "../members.types.js";

export const witnessScriptDecodingMembers = defineDeploymentFamily(
  "witnessScriptDecoding",
  {
    fraudProofWitnessScriptDecoding: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof witness-script-decoding step-01",
      manifestPosition: 266,
      publicationPosition: 172,
      referenceRolePosition: 405,
      commands: [],
      validatorTitle: "fraud_proofs/witness_script_decoding/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofWitnessScriptDecodingStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof witness-script-decoding step-02",
      manifestPosition: 267,
      publicationPosition: 173,
      referenceRolePosition: 406,
      commands: [],
      validatorTitle: "fraud_proofs/witness_script_decoding/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofWitnessScriptDecodingStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof witness-script-decoding step-03",
      manifestPosition: 268,
      publicationPosition: 174,
      referenceRolePosition: 407,
      commands: [],
      validatorTitle: "fraud_proofs/witness_script_decoding/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofWitnessScriptDecodingStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof witness-script-decoding step-04",
      manifestPosition: 269,
      publicationPosition: 175,
      referenceRolePosition: 408,
      commands: [],
      validatorTitle: "fraud_proofs/witness_script_decoding/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
