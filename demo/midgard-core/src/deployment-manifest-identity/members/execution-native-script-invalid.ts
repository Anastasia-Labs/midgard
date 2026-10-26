import { defineDeploymentFamily } from "../members.types.js";

export const executionNativeScriptInvalidMembers = defineDeploymentFamily(
  "executionNativeScriptInvalid",
  {
    fraudProofExecutionNativeScriptInvalid: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-01",
      manifestPosition: 345,
      publicationPosition: 327,
      referenceRolePosition: 560,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofExecutionNativeScriptInvalidStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-02",
      manifestPosition: 346,
      publicationPosition: 328,
      referenceRolePosition: 561,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofExecutionNativeScriptInvalidStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-03",
      manifestPosition: 347,
      publicationPosition: 329,
      referenceRolePosition: 562,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofExecutionNativeScriptInvalidStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-04",
      manifestPosition: 348,
      publicationPosition: 330,
      referenceRolePosition: 563,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofExecutionNativeScriptInvalidStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-05",
      manifestPosition: 349,
      publicationPosition: 331,
      referenceRolePosition: 564,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofExecutionNativeScriptInvalidStep06: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-06",
      manifestPosition: 350,
      publicationPosition: 332,
      referenceRolePosition: 565,
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
      publicationPosition: 333,
      referenceRolePosition: 566,
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
      publicationPosition: 334,
      referenceRolePosition: 567,
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
      publicationPosition: 335,
      referenceRolePosition: 568,
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
      publicationPosition: 336,
      referenceRolePosition: 569,
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
      publicationPosition: 337,
      referenceRolePosition: 570,
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
      publicationPosition: 338,
      referenceRolePosition: 571,
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
      publicationPosition: 339,
      referenceRolePosition: 572,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_reference_source.main.spend",
      familyPaths: [["steps", "12"]],
    },
  },
);
