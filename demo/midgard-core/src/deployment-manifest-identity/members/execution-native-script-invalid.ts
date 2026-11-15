import { defineDeploymentFamily } from "../members.types.js";

export const executionNativeScriptInvalidMembers = defineDeploymentFamily(
  "executionNativeScriptInvalid",
  {
    fraudProofExecutionNativeScriptInvalid: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid step-01",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofExecutionNativeScriptInvalidBindEvent: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid bind-event",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofExecutionNativeScriptInvalidBindPreState: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid bind-pre-state",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceStart: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid source-start",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_start.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceResolved: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid source-resolved",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceAuthenticate: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid source-authenticate",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceMap: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid source-map",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_map.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceInputFinish: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid source-input-finish",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid source-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceWitnesses: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid source-witnesses",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceFinish: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid source-finish",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_finish.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofExecutionNativeScriptInvalidFamilyEntry: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid family-entry",
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/family_entry.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofExecutionNativeScriptInvalidPurposeSelect: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid purpose-select",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/purpose_select.main.spend",
      familyPaths: [["steps", "12"]],
    },
    fraudProofExecutionNativeScriptInvalidSpendSelect: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid spend-select",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/spend_select.main.spend",
      familyPaths: [["steps", "13"]],
    },
    fraudProofExecutionNativeScriptInvalidSpendScan: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid spend-scan",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/spend_scan.main.spend",
      familyPaths: [["steps", "14"]],
    },
    fraudProofExecutionNativeScriptInvalidMintGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid mint-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/mint_grammar.main.spend",
      familyPaths: [["steps", "15"]],
    },
    fraudProofExecutionNativeScriptInvalidMintScan: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid mint-scan",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/mint_scan.main.spend",
      familyPaths: [["steps", "16"]],
    },
    fraudProofExecutionNativeScriptInvalidReceiveStart: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid receive-start",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_start.main.spend",
      familyPaths: [["steps", "17"]],
    },
    fraudProofExecutionNativeScriptInvalidReceiveGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid receive-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_grammar.main.spend",
      familyPaths: [["steps", "18"]],
    },
    fraudProofExecutionNativeScriptInvalidReceiveOutputs: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid receive-outputs",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_outputs.main.spend",
      familyPaths: [["steps", "19"]],
    },
    fraudProofExecutionNativeScriptInvalidReceiveFinish: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid receive-finish",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_finish.main.spend",
      familyPaths: [["steps", "20"]],
    },
    fraudProofExecutionNativeScriptInvalidReceiveDeclaration: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid receive-declaration",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_declaration.main.spend",
      familyPaths: [["steps", "21"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceLookup: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid source-lookup",
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/lookup.main.spend",
      familyPaths: [["steps", "22"]],
    },
    fraudProofExecutionNativeScriptInvalidSigners: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid signers",
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/signers.main.spend",
      familyPaths: [["steps", "23"]],
    },
    fraudProofExecutionNativeScriptInvalidEvaluate: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid evaluate",
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/evaluate.main.spend",
      familyPaths: [["steps", "24"]],
    },
    fraudProofExecutionNativeScriptInvalidSpendKey: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid spend-key",
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/spend_key.main.spend",
      familyPaths: [["steps", "25"]],
    },
    fraudProofExecutionNativeScriptInvalidSpendRank: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof execution-native-script-invalid spend-rank",
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/spend_rank.main.spend",
      familyPaths: [["steps", "26"]],
    },
    fraudProofExecutionNativeScriptInvalidTerminal: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof execution-native-script-invalid terminal",
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/terminal.main.spend",
      familyPaths: [["steps", "27"]],
    },
  },
);
