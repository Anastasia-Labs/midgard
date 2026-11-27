import { defineDeploymentFamily } from "../members.types.js";

export const receivePurposeLanguageMembers = defineDeploymentFamily(
  "receivePurposeLanguage",
  {
    fraudProofReceivePurposeLanguage: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof receive-purpose-language step-01",
        token: "V1FpRecvPurposeS01",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofReceivePurposeLanguageBindEvent: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof receive-purpose-language bind-event",
        token: "V1FpRecvPurposeBindEvent",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofReceivePurposeLanguageBindPreState: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof receive-purpose-language bind-pre-state",
        token: "V1FpRecvPurposeBindPreState",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofReceivePurposeLanguageSourceStart: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof receive-purpose-language source-start",
        token: "V1FpRecvPurposeSrcStart",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_start.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofReceivePurposeLanguageSourceResolved: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof receive-purpose-language source-resolved",
        token: "V1FpRecvPurposeSrcResolved",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofReceivePurposeLanguageSourceAuthenticate: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof receive-purpose-language source-authenticate",
        token: "V1FpRecvPurposeSrcAuth",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofReceivePurposeLanguageSourceMap: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof receive-purpose-language source-map",
        token: "V1FpRecvPurposeSrcMap",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_map.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofReceivePurposeLanguageSourceInputFinish: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof receive-purpose-language source-input-finish",
        token: "V1FpRecvPurposeSrcInputFinish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofReceivePurposeLanguageSourceGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof receive-purpose-language source-grammar",
        token: "V1FpRecvPurposeSrcGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofReceivePurposeLanguageSourceWitnesses: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof receive-purpose-language source-witnesses",
        token: "V1FpRecvPurposeSrcWitnesses",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofReceivePurposeLanguageSourceFinish: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof receive-purpose-language source-finish",
        token: "V1FpRecvPurposeSrcFinish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_finish.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofReceivePurposeLanguageFamilyEntry: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof receive-purpose-language family-entry",
        token: "V1FpRecvPurposeEntry",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/receive_purpose_language/family_entry.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofReceivePurposeLanguageReceiveStart: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof receive-purpose-language receive-start",
        token: "V1FpRecvPurposeRecvStart",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_start.main.spend",
      familyPaths: [["steps", "12"]],
    },
    fraudProofReceivePurposeLanguageReceiveGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof receive-purpose-language receive-grammar",
        token: "V1FpRecvPurposeRecvGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_grammar.main.spend",
      familyPaths: [["steps", "13"]],
    },
    fraudProofReceivePurposeLanguageReceiveOutputs: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof receive-purpose-language receive-outputs",
        token: "V1FpRecvPurposeRecvOutputs",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_outputs.main.spend",
      familyPaths: [["steps", "14"]],
    },
    fraudProofReceivePurposeLanguageReceiveFinish: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof receive-purpose-language receive-finish",
        token: "V1FpRecvPurposeRecvFinish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_finish.main.spend",
      familyPaths: [["steps", "15"]],
    },
    fraudProofReceivePurposeLanguageReceiveDeclaration: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof receive-purpose-language receive-declaration",
        token: "V1FpRecvPurposeRecvDecl",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_declaration.main.spend",
      familyPaths: [["steps", "16"]],
    },
    fraudProofReceivePurposeLanguageSourceLookup: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof receive-purpose-language source-lookup",
        token: "V1FpRecvPurposeSrcLookup",
      },
      commands: [],
      validatorTitle: "fraud_proofs/receive_purpose_language/lookup.main.spend",
      familyPaths: [["steps", "17"]],
    },
    fraudProofReceivePurposeLanguageClaimedRedeemerGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof receive-purpose-language claimed-redeemer-grammar",
        token: "V1FpRecvPurposeClaimedGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claimed_redeemer_grammar.main.spend",
      familyPaths: [["steps", "18"]],
    },
    fraudProofReceivePurposeLanguageClaimedRedeemerScan: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof receive-purpose-language claimed-redeemer-scan",
        token: "V1FpRecvPurposeClaimedScan",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claimed_redeemer_scan.main.spend",
      familyPaths: [["steps", "19"]],
    },
    fraudProofReceivePurposeLanguageTerminal: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof receive-purpose-language terminal",
        token: "V1FpRecvPurposeTerminal",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/receive_purpose_language/terminal.main.spend",
      familyPaths: [["steps", "20"]],
    },
  },
);
