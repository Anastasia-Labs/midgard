import { defineDeploymentFamily } from "../members.types.js";

export const witnessScriptDecodingMembers = defineDeploymentFamily(
  "witnessScriptDecoding",
  {
    fraudProofWitnessScriptDecoding: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof witness-script-decoding step-01",
      manifestPosition: 260,
      publicationPosition: 166,
      referenceRolePosition: 396,
      commands: [],
      validatorTitle: "fraud_proofs/witness_script_decoding/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofWitnessScriptDecodingStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof witness-script-decoding step-02",
      manifestPosition: 261,
      publicationPosition: 167,
      referenceRolePosition: 397,
      commands: [],
      validatorTitle: "fraud_proofs/witness_script_decoding/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofWitnessScriptDecodingStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof witness-script-decoding step-03",
      manifestPosition: 262,
      publicationPosition: 168,
      referenceRolePosition: 398,
      commands: [],
      validatorTitle: "fraud_proofs/witness_script_decoding/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofWitnessScriptDecodingStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof witness-script-decoding step-04",
      manifestPosition: 263,
      publicationPosition: 169,
      referenceRolePosition: 399,
      commands: [],
      validatorTitle: "fraud_proofs/witness_script_decoding/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
