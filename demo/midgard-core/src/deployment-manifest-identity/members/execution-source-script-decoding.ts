import { defineDeploymentFamily } from "../members.types.js";

export const executionSourceScriptDecodingMembers = defineDeploymentFamily(
  "executionSourceScriptDecoding",
  {
    fraudProofExecutionSourceScriptDecoding: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-source-script-decoding step-01",
      manifestPosition: 322,
      publicationPosition: 228,
      referenceRolePosition: 453,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofExecutionSourceScriptDecodingBindEvent: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-source-script-decoding bind-event",
      manifestPosition: 323,
      publicationPosition: 229,
      referenceRolePosition: 454,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofExecutionSourceScriptDecodingTerminal: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-source-script-decoding terminal",
      manifestPosition: 324,
      publicationPosition: 230,
      referenceRolePosition: 455,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_source_script_decoding/terminal.main.spend",
      familyPaths: [["steps", "2"]],
    },
  },
);
