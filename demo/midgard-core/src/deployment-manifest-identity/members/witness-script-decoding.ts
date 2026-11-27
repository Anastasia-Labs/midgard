import { defineDeploymentFamily } from "../members.types.js";

export const witnessScriptDecodingMembers = defineDeploymentFamily(
  "witnessScriptDecoding",
  {
    fraudProofWitnessScriptDecoding: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof witness-script-decoding step-01",
        token: "V1FpWitnessDecodeS01",
      },
      commands: [],
      validatorTitle: "fraud_proofs/witness_script_decoding/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofWitnessScriptDecodingStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof witness-script-decoding step-02",
        token: "V1FpWitnessDecodeS02",
      },
      commands: [],
      validatorTitle: "fraud_proofs/witness_script_decoding/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofWitnessScriptDecodingStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof witness-script-decoding step-03",
        token: "V1FpWitnessDecodeS03",
      },
      commands: [],
      validatorTitle: "fraud_proofs/witness_script_decoding/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofWitnessScriptDecodingStep04: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof witness-script-decoding step-04",
        token: "V1FpWitnessDecodeS04",
      },
      commands: [],
      validatorTitle: "fraud_proofs/witness_script_decoding/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
