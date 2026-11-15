import { defineDeploymentFamily } from "../members.types.js";

export const outputReferenceScriptDecodingMembers = defineDeploymentFamily(
  "outputReferenceScriptDecoding",
  {
    fraudProofOutputReferenceScriptDecoding: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof output-reference-script-decoding step-01",
      commands: [],
      validatorTitle:
        "fraud_proofs/output_reference_script_decoding/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofOutputReferenceScriptDecodingStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof output-reference-script-decoding step-02",
      commands: [],
      validatorTitle:
        "fraud_proofs/output_reference_script_decoding/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofOutputReferenceScriptDecodingStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof output-reference-script-decoding step-03",
      commands: [],
      validatorTitle:
        "fraud_proofs/output_reference_script_decoding/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofOutputReferenceScriptDecodingStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof output-reference-script-decoding step-04",
      commands: [],
      validatorTitle:
        "fraud_proofs/output_reference_script_decoding/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofOutputReferenceScriptDecodingStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof output-reference-script-decoding step-05",
      commands: [],
      validatorTitle:
        "fraud_proofs/output_reference_script_decoding/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofOutputReferenceScriptDecodingStep06: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof output-reference-script-decoding step-06",
      commands: [],
      validatorTitle:
        "fraud_proofs/output_reference_script_decoding/step_06.main.spend",
      familyPaths: [["steps", "5"]],
    },
  },
);
