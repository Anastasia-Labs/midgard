import { defineDeploymentFamily } from "../members.types.js";

export const executionSourceScriptDecodingMembers = defineDeploymentFamily(
  "executionSourceScriptDecoding",
  {
    fraudProofExecutionSourceScriptDecoding: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-source-script-decoding step-01",
      manifestPosition: 322,
      publicationPosition: 228,
      referenceRolePosition: 452,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_source_script_decoding/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofExecutionSourceScriptDecodingStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-source-script-decoding step-02",
      manifestPosition: 323,
      publicationPosition: 229,
      referenceRolePosition: 453,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_source_script_decoding/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofExecutionSourceScriptDecodingStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-source-script-decoding step-03",
      manifestPosition: 324,
      publicationPosition: 230,
      referenceRolePosition: 454,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_source_script_decoding/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofExecutionSourceScriptDecodingStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-source-script-decoding step-04",
      manifestPosition: 325,
      publicationPosition: 231,
      referenceRolePosition: 455,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_source_script_decoding/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofExecutionSourceScriptDecodingStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-source-script-decoding step-05",
      manifestPosition: 326,
      publicationPosition: 232,
      referenceRolePosition: 456,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_source_script_decoding/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
  },
);
