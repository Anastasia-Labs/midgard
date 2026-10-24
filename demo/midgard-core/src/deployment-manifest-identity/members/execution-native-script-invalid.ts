import { defineDeploymentFamily } from "../members.types.js";

export const executionNativeScriptInvalidMembers = defineDeploymentFamily(
  "executionNativeScriptInvalid",
  {
    fraudProofExecutionNativeScriptInvalid: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-01",
      manifestPosition: 345,
      publicationPosition: 296,
      referenceRolePosition: 529,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofExecutionNativeScriptInvalidStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-02",
      manifestPosition: 346,
      publicationPosition: 297,
      referenceRolePosition: 530,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofExecutionNativeScriptInvalidStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-03",
      manifestPosition: 347,
      publicationPosition: 298,
      referenceRolePosition: 531,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofExecutionNativeScriptInvalidStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-04",
      manifestPosition: 348,
      publicationPosition: 299,
      referenceRolePosition: 532,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofExecutionNativeScriptInvalidStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-05",
      manifestPosition: 349,
      publicationPosition: 300,
      referenceRolePosition: 533,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofExecutionNativeScriptInvalidStep06: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-06",
      manifestPosition: 350,
      publicationPosition: 301,
      referenceRolePosition: 534,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_06.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofExecutionNativeScriptInvalidAcceptedReconstructionInit: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid accepted-reconstruction-init",
      manifestPosition: 351,
      publicationPosition: 302,
      referenceRolePosition: 535,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_reconstruction_init.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofExecutionNativeScriptInvalidAcceptedSpendPrefix: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid accepted-spend-prefix",
      manifestPosition: 352,
      publicationPosition: 303,
      referenceRolePosition: 536,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_spend_prefix.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofExecutionNativeScriptInvalidAcceptedMintPrefix: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid accepted-mint-prefix",
      manifestPosition: 353,
      publicationPosition: 304,
      referenceRolePosition: 537,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_mint_prefix.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofExecutionNativeScriptInvalidAcceptedObserverPrefix: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid accepted-observer-prefix",
      manifestPosition: 354,
      publicationPosition: 305,
      referenceRolePosition: 538,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_observer_prefix.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofExecutionNativeScriptInvalidAcceptedReceivePrefix: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid accepted-receive-prefix",
      manifestPosition: 355,
      publicationPosition: 306,
      referenceRolePosition: 539,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_receive_prefix.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofExecutionNativeScriptInvalidAcceptedInlineSource: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid accepted-inline-source",
      manifestPosition: 356,
      publicationPosition: 307,
      referenceRolePosition: 540,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_inline_source.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofExecutionNativeScriptInvalidAcceptedReferenceSource: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid accepted-reference-source",
      manifestPosition: 357,
      publicationPosition: 308,
      referenceRolePosition: 541,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_reference_source.main.spend",
      familyPaths: [["steps", "12"]],
    },
  },
);
