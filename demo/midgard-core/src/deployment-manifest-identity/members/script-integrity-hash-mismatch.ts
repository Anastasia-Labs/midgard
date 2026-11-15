import { defineDeploymentFamily } from "../members.types.js";

export const scriptIntegrityHashMismatchMembers = defineDeploymentFamily(
  "scriptIntegrityHashMismatch",
  {
    fraudProofScriptIntegrityHashMismatch: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-mismatch step-01",
        token: "V1FpIntegrityMismatchS01",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofScriptIntegrityHashMismatchBindEvent: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-mismatch bind-event",
        token: "V1FpIntegrityBindEvent",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofScriptIntegrityHashMismatchBindPreState: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-mismatch bind-pre-state",
        token: "V1FpIntegrityBindPreState",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceStart: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-mismatch source-start",
        token: "V1FpIntegritySrcStart",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_start.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceResolved: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-mismatch source-resolved",
        token: "V1FpIntegritySrcResolved",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceAuthenticate: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-mismatch source-authenticate",
        token: "V1FpIntegritySrcAuth",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceMap: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-mismatch source-map",
        token: "V1FpIntegritySrcMap",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_map.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceInputFinish: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-mismatch source-input-finish",
        token: "V1FpIntegritySrcInputFinish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-mismatch source-grammar",
        token: "V1FpIntegritySrcGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceWitnesses: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-mismatch source-witnesses",
        token: "V1FpIntegritySrcWitnesses",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceFinish: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-mismatch source-finish",
        token: "V1FpIntegritySrcFinish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_finish.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofScriptIntegrityHashMismatchFamilyEntry: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-mismatch family-entry",
        token: "V1FpIntegrityEntry",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/family_entry.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesSpend: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-mismatch all-purposes-spend",
        token: "V1FpIntegrityApSpend",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_spend.main.spend",
      familyPaths: [["steps", "12"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesSpendResolve: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-mismatch all-purposes-spend-resolve",
        token: "V1FpIntegrityApSpendResolve",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_spend_resolve.main.spend",
      familyPaths: [["steps", "13"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesMintGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-mismatch all-purposes-mint-grammar",
        token: "V1FpIntegrityApMintGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_mint_grammar.main.spend",
      familyPaths: [["steps", "14"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesMint: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-mismatch all-purposes-mint",
        token: "V1FpIntegrityApMint",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_mint.main.spend",
      familyPaths: [["steps", "15"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesObserveOpen: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-mismatch all-purposes-observe-open",
        token: "V1FpIntegrityApObserveOpen",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_observe_open.main.spend",
      familyPaths: [["steps", "16"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesObserve: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-mismatch all-purposes-observe",
        token: "V1FpIntegrityApObserve",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_observe.main.spend",
      familyPaths: [["steps", "17"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesReceiveGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-mismatch all-purposes-receive-grammar",
        token: "V1FpIntegrityApRecvGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_receive_grammar.main.spend",
      familyPaths: [["steps", "18"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesReceive: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-mismatch all-purposes-receive",
        token: "V1FpIntegrityApRecv",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_receive.main.spend",
      familyPaths: [["steps", "19"]],
    },
    fraudProofScriptIntegrityHashMismatchTerminal: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof script-integrity-hash-mismatch terminal",
        token: "V1FpIntegrityTerminal",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/terminal.main.spend",
      familyPaths: [["steps", "20"]],
    },
  },
);
