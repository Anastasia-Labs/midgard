import { defineDeploymentFamily } from "../members.types.js";

export const executionNativeScriptInvalidMembers = defineDeploymentFamily(
  "executionNativeScriptInvalid",
  {
    fraudProofExecutionNativeScriptInvalid: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-01",
      manifestPosition: 361,
      publicationPosition: 343,
      referenceRolePosition: 576,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofExecutionNativeScriptInvalidStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-02",
      manifestPosition: 362,
      publicationPosition: 344,
      referenceRolePosition: 577,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofExecutionNativeScriptInvalidStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-03",
      manifestPosition: 363,
      publicationPosition: 345,
      referenceRolePosition: 578,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofExecutionNativeScriptInvalidStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-04",
      manifestPosition: 364,
      publicationPosition: 346,
      referenceRolePosition: 579,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofExecutionNativeScriptInvalidStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-05",
      manifestPosition: 365,
      publicationPosition: 347,
      referenceRolePosition: 580,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofExecutionNativeScriptInvalidStep06: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-06",
      manifestPosition: 366,
      publicationPosition: 348,
      referenceRolePosition: 581,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/step_06.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofExecutionNativeScriptInvalidAcceptedReconstructionInit: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid accepted-reconstruction-init",
      manifestPosition: 367,
      publicationPosition: 349,
      referenceRolePosition: 582,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_reconstruction_init.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofExecutionNativeScriptInvalidAcceptedSpendPrefix: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid accepted-spend-prefix",
      manifestPosition: 368,
      publicationPosition: 350,
      referenceRolePosition: 583,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_spend_prefix.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofExecutionNativeScriptInvalidAcceptedMintPrefix: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid accepted-mint-prefix",
      manifestPosition: 369,
      publicationPosition: 351,
      referenceRolePosition: 584,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_mint_prefix.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofExecutionNativeScriptInvalidAcceptedObserverPrefix: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid accepted-observer-prefix",
      manifestPosition: 370,
      publicationPosition: 352,
      referenceRolePosition: 585,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_observer_prefix.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofExecutionNativeScriptInvalidAcceptedReceivePrefix: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid accepted-receive-prefix",
      manifestPosition: 371,
      publicationPosition: 353,
      referenceRolePosition: 586,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_receive_prefix.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofExecutionNativeScriptInvalidAcceptedInlineSource: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid accepted-inline-source",
      manifestPosition: 372,
      publicationPosition: 354,
      referenceRolePosition: 587,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_inline_source.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofExecutionNativeScriptInvalidAcceptedReferenceSource: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid accepted-reference-source",
      manifestPosition: 373,
      publicationPosition: 355,
      referenceRolePosition: 588,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/accepted_reference_source.main.spend",
      familyPaths: [["steps", "12"]],
    },
  },
);
