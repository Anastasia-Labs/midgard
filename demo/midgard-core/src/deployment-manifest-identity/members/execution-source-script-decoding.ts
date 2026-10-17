import { defineDeploymentFamily } from "../members.types.js";

export const executionSourceScriptDecodingMembers = defineDeploymentFamily(
  "executionSourceScriptDecoding",
  {
    fraudProofExecutionSourceScriptDecoding: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-source-script-decoding step-01",
      manifestPosition: 315,
      publicationPosition: 221,
      referenceRolePosition: 448,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_source_script_decoding/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofExecutionSourceScriptDecodingStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-source-script-decoding step-02",
      manifestPosition: 316,
      publicationPosition: 222,
      referenceRolePosition: 449,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_source_script_decoding/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofExecutionSourceScriptDecodingStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-source-script-decoding step-03",
      manifestPosition: 317,
      publicationPosition: 223,
      referenceRolePosition: 450,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_source_script_decoding/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofExecutionSourceScriptDecodingStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-source-script-decoding step-04",
      manifestPosition: 318,
      publicationPosition: 224,
      referenceRolePosition: 451,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_source_script_decoding/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofExecutionSourceScriptDecodingStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-source-script-decoding step-05",
      manifestPosition: 319,
      publicationPosition: 225,
      referenceRolePosition: 452,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_source_script_decoding/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
  },
);
