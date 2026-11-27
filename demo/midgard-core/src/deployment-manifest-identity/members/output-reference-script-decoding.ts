import { defineDeploymentFamily } from "../members.types.js";

export const outputReferenceScriptDecodingMembers = defineDeploymentFamily(
  "outputReferenceScriptDecoding",
  {
    fraudProofOutputReferenceScriptDecoding: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof output-reference-script-decoding step-01",
        token: "V1FpOutputRefDecodeS01",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/output_reference_script_decoding/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofOutputReferenceScriptDecodingStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof output-reference-script-decoding step-02",
        token: "V1FpOutputRefDecodeS02",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/output_reference_script_decoding/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofOutputReferenceScriptDecodingStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof output-reference-script-decoding step-03",
        token: "V1FpOutputRefDecodeS03",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/output_reference_script_decoding/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofOutputReferenceScriptDecodingStep04: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof output-reference-script-decoding step-04",
        token: "V1FpOutputRefDecodeS04",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/output_reference_script_decoding/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofOutputReferenceScriptDecodingStep05: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof output-reference-script-decoding step-05",
        token: "V1FpOutputRefDecodeS05",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/output_reference_script_decoding/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofOutputReferenceScriptDecodingStep06: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof output-reference-script-decoding step-06",
        token: "V1FpOutputRefDecodeS06",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/output_reference_script_decoding/step_06.main.spend",
      familyPaths: [["steps", "5"]],
    },
  },
);
