import { defineDeploymentFamily } from "../members.types.js";

export const outputReferenceScriptDecodingMembers = defineDeploymentFamily(
  "outputReferenceScriptDecoding",
  {
    fraudProofOutputReferenceScriptDecoding: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof output-reference-script-decoding step-01",
      manifestPosition: 316,
      publicationPosition: 222,
      referenceRolePosition: 446,
      commands: [],
      validatorTitle:
        "fraud_proofs/output_reference_script_decoding/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofOutputReferenceScriptDecodingStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof output-reference-script-decoding step-02",
      manifestPosition: 317,
      publicationPosition: 223,
      referenceRolePosition: 447,
      commands: [],
      validatorTitle:
        "fraud_proofs/output_reference_script_decoding/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofOutputReferenceScriptDecodingStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof output-reference-script-decoding step-03",
      manifestPosition: 318,
      publicationPosition: 224,
      referenceRolePosition: 448,
      commands: [],
      validatorTitle:
        "fraud_proofs/output_reference_script_decoding/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofOutputReferenceScriptDecodingStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof output-reference-script-decoding step-04",
      manifestPosition: 319,
      publicationPosition: 225,
      referenceRolePosition: 449,
      commands: [],
      validatorTitle:
        "fraud_proofs/output_reference_script_decoding/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofOutputReferenceScriptDecodingStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof output-reference-script-decoding step-05",
      manifestPosition: 320,
      publicationPosition: 226,
      referenceRolePosition: 450,
      commands: [],
      validatorTitle:
        "fraud_proofs/output_reference_script_decoding/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofOutputReferenceScriptDecodingStep06: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof output-reference-script-decoding step-06",
      manifestPosition: 321,
      publicationPosition: 227,
      referenceRolePosition: 451,
      commands: [],
      validatorTitle:
        "fraud_proofs/output_reference_script_decoding/step_06.main.spend",
      familyPaths: [["steps", "5"]],
    },
  },
);
