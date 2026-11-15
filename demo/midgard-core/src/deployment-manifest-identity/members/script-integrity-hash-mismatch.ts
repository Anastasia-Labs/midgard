import { defineDeploymentFamily } from "../members.types.js";

export const scriptIntegrityHashMismatchMembers = defineDeploymentFamily(
  "scriptIntegrityHashMismatch",
  {
    fraudProofScriptIntegrityHashMismatch: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch step-01",
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofScriptIntegrityHashMismatchBindEvent: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch bind-event",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofScriptIntegrityHashMismatchBindPreState: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch bind-pre-state",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceStart: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch source-start",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_start.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceResolved: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch source-resolved",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceAuthenticate: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch source-authenticate",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceMap: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch source-map",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_map.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceInputFinish: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch source-input-finish",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch source-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceWitnesses: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch source-witnesses",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceFinish: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch source-finish",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_finish.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofScriptIntegrityHashMismatchFamilyEntry: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch family-entry",
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/family_entry.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesSpend: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-spend",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_spend.main.spend",
      familyPaths: [["steps", "12"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesSpendResolve: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-spend-resolve",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_spend_resolve.main.spend",
      familyPaths: [["steps", "13"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesMintGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-mint-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_mint_grammar.main.spend",
      familyPaths: [["steps", "14"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesMint: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-mint",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_mint.main.spend",
      familyPaths: [["steps", "15"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesObserveOpen: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-observe-open",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_observe_open.main.spend",
      familyPaths: [["steps", "16"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesObserve: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-observe",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_observe.main.spend",
      familyPaths: [["steps", "17"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesReceiveGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-receive-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_receive_grammar.main.spend",
      familyPaths: [["steps", "18"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesReceive: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-receive",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_receive.main.spend",
      familyPaths: [["steps", "19"]],
    },
    fraudProofScriptIntegrityHashMismatchTerminal: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch terminal",
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/terminal.main.spend",
      familyPaths: [["steps", "20"]],
    },
  },
);
