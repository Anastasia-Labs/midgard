import { defineDeploymentFamily } from "../members.types.js";

export const scriptIntegrityHashMismatchMembers = defineDeploymentFamily(
  "scriptIntegrityHashMismatch",
  {
    fraudProofScriptIntegrityHashMismatch: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch step-01",
      manifestPosition: 374,
      publicationPosition: 356,
      referenceRolePosition: 589,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofScriptIntegrityHashMismatchBindEvent: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch bind-event",
      manifestPosition: 375,
      publicationPosition: 357,
      referenceRolePosition: 590,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofScriptIntegrityHashMismatchBindPreState: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch bind-pre-state",
      manifestPosition: 376,
      publicationPosition: 358,
      referenceRolePosition: 591,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceStart: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch source-start",
      manifestPosition: 377,
      publicationPosition: 359,
      referenceRolePosition: 592,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_start.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceResolved: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch source-resolved",
      manifestPosition: 378,
      publicationPosition: 360,
      referenceRolePosition: 593,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceAuthenticate: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch source-authenticate",
      manifestPosition: 379,
      publicationPosition: 361,
      referenceRolePosition: 594,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceMap: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch source-map",
      manifestPosition: 380,
      publicationPosition: 362,
      referenceRolePosition: 595,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_map.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceInputFinish: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch source-input-finish",
      manifestPosition: 381,
      publicationPosition: 363,
      referenceRolePosition: 596,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch source-grammar",
      manifestPosition: 382,
      publicationPosition: 364,
      referenceRolePosition: 597,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceWitnesses: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch source-witnesses",
      manifestPosition: 383,
      publicationPosition: 365,
      referenceRolePosition: 598,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofScriptIntegrityHashMismatchSourceFinish: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch source-finish",
      manifestPosition: 384,
      publicationPosition: 366,
      referenceRolePosition: 599,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_finish.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofScriptIntegrityHashMismatchFamilyEntry: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch family-entry",
      manifestPosition: 385,
      publicationPosition: 367,
      referenceRolePosition: 600,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/family_entry.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesSpend: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-spend",
      manifestPosition: 386,
      publicationPosition: 368,
      referenceRolePosition: 601,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_spend.main.spend",
      familyPaths: [["steps", "12"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesSpendResolve: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-spend-resolve",
      manifestPosition: 387,
      publicationPosition: 369,
      referenceRolePosition: 602,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_spend_resolve.main.spend",
      familyPaths: [["steps", "13"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesMintGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-mint-grammar",
      manifestPosition: 388,
      publicationPosition: 370,
      referenceRolePosition: 603,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_mint_grammar.main.spend",
      familyPaths: [["steps", "14"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesMint: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-mint",
      manifestPosition: 389,
      publicationPosition: 371,
      referenceRolePosition: 604,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_mint.main.spend",
      familyPaths: [["steps", "15"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesObserveOpen: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-observe-open",
      manifestPosition: 390,
      publicationPosition: 372,
      referenceRolePosition: 605,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_observe_open.main.spend",
      familyPaths: [["steps", "16"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesObserve: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-observe",
      manifestPosition: 391,
      publicationPosition: 373,
      referenceRolePosition: 606,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_observe.main.spend",
      familyPaths: [["steps", "17"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesReceiveGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-receive-grammar",
      manifestPosition: 392,
      publicationPosition: 374,
      referenceRolePosition: 607,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_receive_grammar.main.spend",
      familyPaths: [["steps", "18"]],
    },
    fraudProofScriptIntegrityHashMismatchAllPurposesReceive: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof script-integrity-hash-mismatch all-purposes-receive",
      manifestPosition: 393,
      publicationPosition: 375,
      referenceRolePosition: 608,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_receive.main.spend",
      familyPaths: [["steps", "19"]],
    },
    fraudProofScriptIntegrityHashMismatchTerminal: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof script-integrity-hash-mismatch terminal",
      manifestPosition: 394,
      publicationPosition: 376,
      referenceRolePosition: 609,
      commands: [],
      validatorTitle:
        "fraud_proofs/script_integrity_hash_mismatch/terminal.main.spend",
      familyPaths: [["steps", "20"]],
    },
  },
);
