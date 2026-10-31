import { defineDeploymentFamily } from "../members.types.js";

export const receivePurposeLanguageMembers = defineDeploymentFamily(
  "receivePurposeLanguage",
  {
    fraudProofReceivePurposeLanguage: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language step-01",
      manifestPosition: 327,
      publicationPosition: 233,
      referenceRolePosition: 466,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofReceivePurposeLanguageBindEvent: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language bind-event",
      manifestPosition: 328,
      publicationPosition: 234,
      referenceRolePosition: 467,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofReceivePurposeLanguageBindPreState: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language bind-pre-state",
      manifestPosition: 329,
      publicationPosition: 235,
      referenceRolePosition: 468,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofReceivePurposeLanguageSourceStart: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language source-start",
      manifestPosition: 661,
      publicationPosition: 656,
      referenceRolePosition: 656,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_start.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofReceivePurposeLanguageSourceResolved: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language source-resolved",
      manifestPosition: 662,
      publicationPosition: 657,
      referenceRolePosition: 657,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofReceivePurposeLanguageSourceAuthenticate: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof receive-purpose-language source-authenticate",
      manifestPosition: 663,
      publicationPosition: 658,
      referenceRolePosition: 658,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofReceivePurposeLanguageSourceMap: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language source-map",
      manifestPosition: 664,
      publicationPosition: 659,
      referenceRolePosition: 659,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_map.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofReceivePurposeLanguageSourceInputFinish: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof receive-purpose-language source-input-finish",
      manifestPosition: 665,
      publicationPosition: 660,
      referenceRolePosition: 660,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofReceivePurposeLanguageSourceGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language source-grammar",
      manifestPosition: 666,
      publicationPosition: 661,
      referenceRolePosition: 661,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofReceivePurposeLanguageSourceWitnesses: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language source-witnesses",
      manifestPosition: 667,
      publicationPosition: 662,
      referenceRolePosition: 662,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofReceivePurposeLanguageSourceFinish: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language source-finish",
      manifestPosition: 668,
      publicationPosition: 663,
      referenceRolePosition: 663,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_finish.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofReceivePurposeLanguageFamilyEntry: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language family-entry",
      manifestPosition: 669,
      publicationPosition: 664,
      referenceRolePosition: 664,
      commands: [],
      validatorTitle:
        "fraud_proofs/receive_purpose_language/family_entry.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofReceivePurposeLanguageReceiveStart: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language receive-start",
      manifestPosition: 670,
      publicationPosition: 665,
      referenceRolePosition: 665,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_start.main.spend",
      familyPaths: [["steps", "12"]],
    },
    fraudProofReceivePurposeLanguageReceiveGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language receive-grammar",
      manifestPosition: 671,
      publicationPosition: 666,
      referenceRolePosition: 666,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_grammar.main.spend",
      familyPaths: [["steps", "13"]],
    },
    fraudProofReceivePurposeLanguageReceiveOutputs: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language receive-outputs",
      manifestPosition: 672,
      publicationPosition: 667,
      referenceRolePosition: 667,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_outputs.main.spend",
      familyPaths: [["steps", "14"]],
    },
    fraudProofReceivePurposeLanguageReceiveFinish: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language receive-finish",
      manifestPosition: 673,
      publicationPosition: 668,
      referenceRolePosition: 668,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_finish.main.spend",
      familyPaths: [["steps", "15"]],
    },
    fraudProofReceivePurposeLanguageReceiveDeclaration: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof receive-purpose-language receive-declaration",
      manifestPosition: 674,
      publicationPosition: 669,
      referenceRolePosition: 669,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_declaration.main.spend",
      familyPaths: [["steps", "16"]],
    },
    fraudProofReceivePurposeLanguageSourceLookup: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language source-lookup",
      manifestPosition: 675,
      publicationPosition: 670,
      referenceRolePosition: 670,
      commands: [],
      validatorTitle: "fraud_proofs/receive_purpose_language/lookup.main.spend",
      familyPaths: [["steps", "17"]],
    },
    fraudProofReceivePurposeLanguageClaimedRedeemerGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof receive-purpose-language claimed-redeemer-grammar",
      manifestPosition: 676,
      publicationPosition: 671,
      referenceRolePosition: 671,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claimed_redeemer_grammar.main.spend",
      familyPaths: [["steps", "18"]],
    },
    fraudProofReceivePurposeLanguageClaimedRedeemerScan: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof receive-purpose-language claimed-redeemer-scan",
      manifestPosition: 677,
      publicationPosition: 672,
      referenceRolePosition: 672,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claimed_redeemer_scan.main.spend",
      familyPaths: [["steps", "19"]],
    },
    fraudProofReceivePurposeLanguageTerminal: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof receive-purpose-language terminal",
      manifestPosition: 678,
      publicationPosition: 673,
      referenceRolePosition: 673,
      commands: [],
      validatorTitle:
        "fraud_proofs/receive_purpose_language/terminal.main.spend",
      familyPaths: [["steps", "20"]],
    },
  },
);
