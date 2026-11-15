import { defineDeploymentFamily } from "../members.types.js";

export const receivePurposeLanguageMembers = defineDeploymentFamily(
  "receivePurposeLanguage",
  {
    fraudProofReceivePurposeLanguage: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language step-01",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofReceivePurposeLanguageBindEvent: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language bind-event",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofReceivePurposeLanguageBindPreState: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language bind-pre-state",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofReceivePurposeLanguageSourceStart: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language source-start",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_start.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofReceivePurposeLanguageSourceResolved: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language source-resolved",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofReceivePurposeLanguageSourceAuthenticate: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof receive-purpose-language source-authenticate",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofReceivePurposeLanguageSourceMap: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language source-map",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_map.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofReceivePurposeLanguageSourceInputFinish: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof receive-purpose-language source-input-finish",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofReceivePurposeLanguageSourceGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language source-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofReceivePurposeLanguageSourceWitnesses: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language source-witnesses",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofReceivePurposeLanguageSourceFinish: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language source-finish",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_finish.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofReceivePurposeLanguageFamilyEntry: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language family-entry",
      commands: [],
      validatorTitle:
        "fraud_proofs/receive_purpose_language/family_entry.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofReceivePurposeLanguageReceiveStart: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language receive-start",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_start.main.spend",
      familyPaths: [["steps", "12"]],
    },
    fraudProofReceivePurposeLanguageReceiveGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language receive-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_grammar.main.spend",
      familyPaths: [["steps", "13"]],
    },
    fraudProofReceivePurposeLanguageReceiveOutputs: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language receive-outputs",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_outputs.main.spend",
      familyPaths: [["steps", "14"]],
    },
    fraudProofReceivePurposeLanguageReceiveFinish: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language receive-finish",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_finish.main.spend",
      familyPaths: [["steps", "15"]],
    },
    fraudProofReceivePurposeLanguageReceiveDeclaration: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof receive-purpose-language receive-declaration",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_declaration.main.spend",
      familyPaths: [["steps", "16"]],
    },
    fraudProofReceivePurposeLanguageSourceLookup: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language source-lookup",
      commands: [],
      validatorTitle: "fraud_proofs/receive_purpose_language/lookup.main.spend",
      familyPaths: [["steps", "17"]],
    },
    fraudProofReceivePurposeLanguageClaimedRedeemerGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof receive-purpose-language claimed-redeemer-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claimed_redeemer_grammar.main.spend",
      familyPaths: [["steps", "18"]],
    },
    fraudProofReceivePurposeLanguageClaimedRedeemerScan: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof receive-purpose-language claimed-redeemer-scan",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claimed_redeemer_scan.main.spend",
      familyPaths: [["steps", "19"]],
    },
    fraudProofReceivePurposeLanguageTerminal: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language terminal",
      commands: [],
      validatorTitle:
        "fraud_proofs/receive_purpose_language/terminal.main.spend",
      familyPaths: [["steps", "20"]],
    },
  },
);
