import { defineDeploymentFamily } from "../members.types.js";

export const scriptIntegrityHashMismatchMembers = defineDeploymentFamily(
  "scriptIntegrityHashMismatch",
  {
    fraudProofScriptIntegrityHashMismatch: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch step-01",
      manifestPosition: 358,
      publicationPosition: 309,
      referenceRolePosition: 542,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofScriptIntegrityHashMismatchBindEvent: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch bind-event",
      manifestPosition: 359,
      publicationPosition: 310,
      referenceRolePosition: 543,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofScriptIntegrityHashMismatchBindPreState: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch bind-pre-state",
      manifestPosition: 360,
      publicationPosition: 311,
      referenceRolePosition: 544,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceStart: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch source-start",
      manifestPosition: 361,
      publicationPosition: 312,
      referenceRolePosition: 545,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_start.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceResolved: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch source-resolved",
      manifestPosition: 362,
      publicationPosition: 313,
      referenceRolePosition: 546,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceAuthenticate: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch source-authenticate",
      manifestPosition: 363,
      publicationPosition: 314,
      referenceRolePosition: 547,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceMap: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch source-map",
      manifestPosition: 364,
      publicationPosition: 315,
      referenceRolePosition: 548,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_map.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceInputFinish: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch source-input-finish",
      manifestPosition: 365,
      publicationPosition: 316,
      referenceRolePosition: 549,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch source-grammar",
      manifestPosition: 366,
      publicationPosition: 317,
      referenceRolePosition: 550,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceWitnesses: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch source-witnesses",
      manifestPosition: 367,
      publicationPosition: 318,
      referenceRolePosition: 551,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceFinish: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch source-finish",
      manifestPosition: 368,
      publicationPosition: 319,
      referenceRolePosition: 552,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_finish.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofScriptIntegrityHashMismatchFamilyEntry: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch family-entry",
      manifestPosition: 369,
      publicationPosition: 320,
      referenceRolePosition: 553,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/family_entry.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesSpend: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-spend",
      manifestPosition: 370,
      publicationPosition: 321,
      referenceRolePosition: 554,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_spend.main.spend",
      familyPaths: [["steps", "12"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesSpendResolve: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-spend-resolve",
      manifestPosition: 371,
      publicationPosition: 322,
      referenceRolePosition: 555,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_spend_resolve.main.spend",
      familyPaths: [["steps", "13"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesMintGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-mint-grammar",
      manifestPosition: 372,
      publicationPosition: 323,
      referenceRolePosition: 556,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_mint_grammar.main.spend",
      familyPaths: [["steps", "14"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesMint: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-mint",
      manifestPosition: 373,
      publicationPosition: 324,
      referenceRolePosition: 557,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_mint.main.spend",
      familyPaths: [["steps", "15"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesObserveOpen: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-observe-open",
      manifestPosition: 374,
      publicationPosition: 325,
      referenceRolePosition: 558,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_observe_open.main.spend",
      familyPaths: [["steps", "16"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesObserve: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-observe",
      manifestPosition: 375,
      publicationPosition: 326,
      referenceRolePosition: 559,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_observe.main.spend",
      familyPaths: [["steps", "17"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesReceiveGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-receive-grammar",
      manifestPosition: 376,
      publicationPosition: 327,
      referenceRolePosition: 560,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_receive_grammar.main.spend",
      familyPaths: [["steps", "18"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesReceive: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-receive",
      manifestPosition: 377,
      publicationPosition: 328,
      referenceRolePosition: 561,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_receive.main.spend",
      familyPaths: [["steps", "19"]],
    },
    fraudProofScriptIntegrityHashMismatchTerminal: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch terminal",
      manifestPosition: 378,
      publicationPosition: 329,
      referenceRolePosition: 562,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/terminal.main.spend",
      familyPaths: [["steps", "20"]],
    },
  },
);
