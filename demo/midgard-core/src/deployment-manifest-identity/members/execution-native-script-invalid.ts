import { defineDeploymentFamily } from "../members.types.js";

export const executionNativeScriptInvalidMembers = defineDeploymentFamily(
  "executionNativeScriptInvalid",
  {
    fraudProofExecutionNativeScriptInvalid: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-01",
      manifestPosition: 339,
      publicationPosition: 258,
      referenceRolePosition: 488,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofExecutionNativeScriptInvalidStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-02",
      manifestPosition: 340,
      publicationPosition: 259,
      referenceRolePosition: 489,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofExecutionNativeScriptInvalidStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-03",
      manifestPosition: 341,
      publicationPosition: 260,
      referenceRolePosition: 490,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofExecutionNativeScriptInvalidStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-04",
      manifestPosition: 342,
      publicationPosition: 261,
      referenceRolePosition: 491,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofExecutionNativeScriptInvalidStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-05",
      manifestPosition: 343,
      publicationPosition: 262,
      referenceRolePosition: 492,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofExecutionNativeScriptInvalidStep06: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-06",
      manifestPosition: 344,
      publicationPosition: 263,
      referenceRolePosition: 493,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_06.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofExecutionNativeScriptInvalidAcceptedReconstructionInit: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid accepted-reconstruction-init",
      manifestPosition: 345,
      publicationPosition: 264,
      referenceRolePosition: 494,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_reconstruction_init.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofExecutionNativeScriptInvalidAcceptedSpendPrefix: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid accepted-spend-prefix",
      manifestPosition: 346,
      publicationPosition: 265,
      referenceRolePosition: 495,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_spend_prefix.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofExecutionNativeScriptInvalidAcceptedMintPrefix: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid accepted-mint-prefix",
      manifestPosition: 347,
      publicationPosition: 266,
      referenceRolePosition: 496,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_mint_prefix.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofExecutionNativeScriptInvalidAcceptedObserverPrefix: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid accepted-observer-prefix",
      manifestPosition: 348,
      publicationPosition: 267,
      referenceRolePosition: 497,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_observer_prefix.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofExecutionNativeScriptInvalidAcceptedReceivePrefix: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid accepted-receive-prefix",
      manifestPosition: 349,
      publicationPosition: 268,
      referenceRolePosition: 498,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_receive_prefix.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofExecutionNativeScriptInvalidAcceptedInlineSource: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid accepted-inline-source",
      manifestPosition: 350,
      publicationPosition: 269,
      referenceRolePosition: 499,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_inline_source.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofExecutionNativeScriptInvalidAcceptedReferenceSource: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid accepted-reference-source",
      manifestPosition: 351,
      publicationPosition: 270,
      referenceRolePosition: 500,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_reference_source.main.spend",
      familyPaths: [["steps", "12"]],
    },
  },
);
