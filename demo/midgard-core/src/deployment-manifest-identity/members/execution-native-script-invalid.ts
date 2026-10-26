import { defineDeploymentFamily } from "../members.types.js";

export const executionNativeScriptInvalidMembers = defineDeploymentFamily(
  "executionNativeScriptInvalid",
  {
    fraudProofExecutionNativeScriptInvalid: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-01",
      manifestPosition: 325,
      publicationPosition: 231,
      referenceRolePosition: 456,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofExecutionNativeScriptInvalidBindEvent: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid bind-event",
      manifestPosition: 326,
      publicationPosition: 232,
      referenceRolePosition: 457,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofExecutionNativeScriptInvalidBindPreState: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid bind-pre-state",
      manifestPosition: 361,
      publicationPosition: 343,
      referenceRolePosition: 576,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceStart: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid source-start",
      manifestPosition: 362,
      publicationPosition: 344,
      referenceRolePosition: 577,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_start.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceResolved: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid source-resolved",
      manifestPosition: 363,
      publicationPosition: 345,
      referenceRolePosition: 578,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceAuthenticate: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid source-authenticate",
      manifestPosition: 364,
      publicationPosition: 346,
      referenceRolePosition: 579,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceMap: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid source-map",
      manifestPosition: 365,
      publicationPosition: 347,
      referenceRolePosition: 580,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_map.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceInputFinish: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid source-input-finish",
      manifestPosition: 366,
      publicationPosition: 348,
      referenceRolePosition: 581,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid source-grammar",
      manifestPosition: 367,
      publicationPosition: 349,
      referenceRolePosition: 582,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceWitnesses: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid source-witnesses",
      manifestPosition: 368,
      publicationPosition: 350,
      referenceRolePosition: 583,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceFinish: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid source-finish",
      manifestPosition: 369,
      publicationPosition: 351,
      referenceRolePosition: 584,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_finish.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofExecutionNativeScriptInvalidFamilyEntry: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid family-entry",
      manifestPosition: 370,
      publicationPosition: 352,
      referenceRolePosition: 585,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/family_entry.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofExecutionNativeScriptInvalidPurposeSelect: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid purpose-select",
      manifestPosition: 371,
      publicationPosition: 353,
      referenceRolePosition: 586,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/purpose_select.main.spend",
      familyPaths: [["steps", "12"]],
    },
    fraudProofExecutionNativeScriptInvalidSpendSelect: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid spend-select",
      manifestPosition: 372,
      publicationPosition: 354,
      referenceRolePosition: 587,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/spend_select.main.spend",
      familyPaths: [["steps", "13"]],
    },
    fraudProofExecutionNativeScriptInvalidSpendScan: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid spend-scan",
      manifestPosition: 373,
      publicationPosition: 355,
      referenceRolePosition: 588,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/spend_scan.main.spend",
      familyPaths: [["steps", "14"]],
    },
    fraudProofExecutionNativeScriptInvalidMintGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid mint-grammar",
      manifestPosition: 661,
      publicationPosition: 656,
      referenceRolePosition: 656,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/mint_grammar.main.spend",
      familyPaths: [["steps", "15"]],
    },
    fraudProofExecutionNativeScriptInvalidMintScan: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid mint-scan",
      manifestPosition: 662,
      publicationPosition: 657,
      referenceRolePosition: 657,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/mint_scan.main.spend",
      familyPaths: [["steps", "16"]],
    },
    fraudProofExecutionNativeScriptInvalidReceiveStart: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid receive-start",
      manifestPosition: 663,
      publicationPosition: 658,
      referenceRolePosition: 658,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_start.main.spend",
      familyPaths: [["steps", "17"]],
    },
    fraudProofExecutionNativeScriptInvalidReceiveGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid receive-grammar",
      manifestPosition: 664,
      publicationPosition: 659,
      referenceRolePosition: 659,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_grammar.main.spend",
      familyPaths: [["steps", "18"]],
    },
    fraudProofExecutionNativeScriptInvalidReceiveOutputs: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid receive-outputs",
      manifestPosition: 665,
      publicationPosition: 660,
      referenceRolePosition: 660,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_outputs.main.spend",
      familyPaths: [["steps", "19"]],
    },
    fraudProofExecutionNativeScriptInvalidReceiveFinish: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid receive-finish",
      manifestPosition: 666,
      publicationPosition: 661,
      referenceRolePosition: 661,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_finish.main.spend",
      familyPaths: [["steps", "20"]],
    },
    fraudProofExecutionNativeScriptInvalidReceiveDeclaration: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid receive-declaration",
      manifestPosition: 667,
      publicationPosition: 662,
      referenceRolePosition: 662,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_declaration.main.spend",
      familyPaths: [["steps", "21"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceLookup: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid source-lookup",
      manifestPosition: 668,
      publicationPosition: 663,
      referenceRolePosition: 663,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/lookup.main.spend",
      familyPaths: [["steps", "22"]],
    },
    fraudProofExecutionNativeScriptInvalidSigners: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid signers",
      manifestPosition: 669,
      publicationPosition: 664,
      referenceRolePosition: 664,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/signers.main.spend",
      familyPaths: [["steps", "23"]],
    },
    fraudProofExecutionNativeScriptInvalidEvaluate: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid evaluate",
      manifestPosition: 670,
      publicationPosition: 665,
      referenceRolePosition: 665,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/evaluate.main.spend",
      familyPaths: [["steps", "24"]],
    },
    fraudProofExecutionNativeScriptInvalidSpendKey: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid spend-key",
      manifestPosition: 671,
      publicationPosition: 666,
      referenceRolePosition: 666,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/spend_key.main.spend",
      familyPaths: [["steps", "25"]],
    },
    fraudProofExecutionNativeScriptInvalidSpendRank: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid spend-rank",
      manifestPosition: 672,
      publicationPosition: 667,
      referenceRolePosition: 667,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/spend_rank.main.spend",
      familyPaths: [["steps", "26"]],
    },
    fraudProofExecutionNativeScriptInvalidTerminal: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid terminal",
      manifestPosition: 673,
      publicationPosition: 668,
      referenceRolePosition: 668,
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/terminal.main.spend",
      familyPaths: [["steps", "27"]],
    },
  },
);
