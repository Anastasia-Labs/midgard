import { defineDeploymentFamily } from "../members.types.js";

export const executionNativeScriptInvalidMembers = defineDeploymentFamily(
  "executionNativeScriptInvalid",
  {
    fraudProofExecutionNativeScriptInvalid: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid step-01",
        token: "V1FpExecNativeInvalidS01",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofExecutionNativeScriptInvalidBindEvent: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid bind-event",
        token: "V1FpExecNativeInvBindEvent",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofExecutionNativeScriptInvalidBindPreState: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid bind-pre-state",
        token: "V1FpExecNativeInvBindPreState",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceStart: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid source-start",
        token: "V1FpExecNativeInvSrcStart",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_start.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceResolved: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid source-resolved",
        token: "V1FpExecNativeInvSrcResolved",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceAuthenticate: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid source-authenticate",
        token: "V1FpExecNativeInvSrcAuth",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceMap: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid source-map",
        token: "V1FpExecNativeInvSrcMap",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_map.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceInputFinish: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid source-input-finish",
        token: "V1FpExecNativeInvSrcInputFinish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid source-grammar",
        token: "V1FpExecNativeInvSrcGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceWitnesses: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid source-witnesses",
        token: "V1FpExecNativeInvSrcWitnesses",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceFinish: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid source-finish",
        token: "V1FpExecNativeInvSrcFinish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_finish.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofExecutionNativeScriptInvalidFamilyEntry: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid family-entry",
        token: "V1FpExecNativeInvEntry",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/family_entry.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofExecutionNativeScriptInvalidPurposeSelect: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid purpose-select",
        token: "V1FpExecNativeInvPurposeSelect",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/purpose_select.main.spend",
      familyPaths: [["steps", "12"]],
    },
    fraudProofExecutionNativeScriptInvalidSpendSelect: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid spend-select",
        token: "V1FpExecNativeInvSpendSelect",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/spend_select.main.spend",
      familyPaths: [["steps", "13"]],
    },
    fraudProofExecutionNativeScriptInvalidSpendScan: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid spend-scan",
        token: "V1FpExecNativeInvSpendScan",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/spend_scan.main.spend",
      familyPaths: [["steps", "14"]],
    },
    fraudProofExecutionNativeScriptInvalidMintGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid mint-grammar",
        token: "V1FpExecNativeInvMintGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/mint_grammar.main.spend",
      familyPaths: [["steps", "15"]],
    },
    fraudProofExecutionNativeScriptInvalidMintScan: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid mint-scan",
        token: "V1FpExecNativeInvMintScan",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/mint_scan.main.spend",
      familyPaths: [["steps", "16"]],
    },
    fraudProofExecutionNativeScriptInvalidReceiveStart: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid receive-start",
        token: "V1FpExecNativeInvRecvStart",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_start.main.spend",
      familyPaths: [["steps", "17"]],
    },
    fraudProofExecutionNativeScriptInvalidReceiveGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid receive-grammar",
        token: "V1FpExecNativeInvRecvGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_grammar.main.spend",
      familyPaths: [["steps", "18"]],
    },
    fraudProofExecutionNativeScriptInvalidReceiveOutputs: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid receive-outputs",
        token: "V1FpExecNativeInvRecvOutputs",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_outputs.main.spend",
      familyPaths: [["steps", "19"]],
    },
    fraudProofExecutionNativeScriptInvalidReceiveFinish: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid receive-finish",
        token: "V1FpExecNativeInvRecvFinish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_finish.main.spend",
      familyPaths: [["steps", "20"]],
    },
    fraudProofExecutionNativeScriptInvalidReceiveDeclaration: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid receive-declaration",
        token: "V1FpExecNativeInvRecvDecl",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_declaration.main.spend",
      familyPaths: [["steps", "21"]],
    },
    fraudProofExecutionNativeScriptInvalidSourceLookup: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid source-lookup",
        token: "V1FpExecNativeInvLookup",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/lookup.main.spend",
      familyPaths: [["steps", "22"]],
    },
    fraudProofExecutionNativeScriptInvalidSigners: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid signers",
        token: "V1FpExecNativeInvSigners",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/signers.main.spend",
      familyPaths: [["steps", "23"]],
    },
    fraudProofExecutionNativeScriptInvalidEvaluate: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid evaluate",
        token: "V1FpExecNativeInvEvaluate",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/evaluate.main.spend",
      familyPaths: [["steps", "24"]],
    },
    fraudProofExecutionNativeScriptInvalidSpendKey: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid spend-key",
        token: "V1FpExecNativeInvSpendKey",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/spend_key.main.spend",
      familyPaths: [["steps", "25"]],
    },
    fraudProofExecutionNativeScriptInvalidSpendRank: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid spend-rank",
        token: "V1FpExecNativeInvSpendRank",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/spend_rank.main.spend",
      familyPaths: [["steps", "26"]],
    },
    fraudProofExecutionNativeScriptInvalidTerminal: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof execution-native-script-invalid terminal",
        token: "V1FpExecNativeInvTerminal",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/execution_native_script_invalid/terminal.main.spend",
      familyPaths: [["steps", "27"]],
    },
  },
);
