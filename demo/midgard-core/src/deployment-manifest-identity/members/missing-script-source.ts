import { defineDeploymentFamily } from "../members.types.js";

export const missingScriptSourceMembers = defineDeploymentFamily(
  "missingScriptSource",
  {
    fraudProofMissingScriptSource: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source step-01",
      manifestPosition: 385,
      publicationPosition: 242,
      referenceRolePosition: 475,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofMissingScriptSourceBindEvent: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source bind-event",
      manifestPosition: 386,
      publicationPosition: 243,
      referenceRolePosition: 476,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofMissingScriptSourceBindPreState: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source bind-pre-state",
      manifestPosition: 387,
      publicationPosition: 244,
      referenceRolePosition: 477,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofMissingScriptSourceSourceStart: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source source-start",
      manifestPosition: 388,
      publicationPosition: 245,
      referenceRolePosition: 478,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_start.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofMissingScriptSourceSourceResolved: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source source-resolved",
      manifestPosition: 389,
      publicationPosition: 246,
      referenceRolePosition: 479,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofMissingScriptSourceSourceAuthenticate: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source source-authenticate",
      manifestPosition: 390,
      publicationPosition: 247,
      referenceRolePosition: 480,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofMissingScriptSourceSourceMap: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source source-map",
      manifestPosition: 391,
      publicationPosition: 248,
      referenceRolePosition: 481,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_map.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofMissingScriptSourceSourceInputFinish: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source source-input-finish",
      manifestPosition: 392,
      publicationPosition: 249,
      referenceRolePosition: 482,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofMissingScriptSourceSourceGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source source-grammar",
      manifestPosition: 393,
      publicationPosition: 250,
      referenceRolePosition: 483,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofMissingScriptSourceSourceWitnesses: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source source-witnesses",
      manifestPosition: 394,
      publicationPosition: 251,
      referenceRolePosition: 484,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofMissingScriptSourceSourceFinish: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source source-finish",
      manifestPosition: 395,
      publicationPosition: 252,
      referenceRolePosition: 485,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_finish.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofMissingScriptSourceFamilyEntry: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source family-entry",
      manifestPosition: 396,
      publicationPosition: 253,
      referenceRolePosition: 486,
      commands: [],
      validatorTitle:
        "fraud_proofs/missing_script_source/family_entry.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofMissingScriptSourcePurposeSelect: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source purpose-select",
      manifestPosition: 397,
      publicationPosition: 254,
      referenceRolePosition: 487,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/purpose_select.main.spend",
      familyPaths: [["steps", "12"]],
    },
    fraudProofMissingScriptSourceSpendSelect: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source spend-select",
      manifestPosition: 398,
      publicationPosition: 255,
      referenceRolePosition: 488,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/spend_select.main.spend",
      familyPaths: [["steps", "13"]],
    },
    fraudProofMissingScriptSourceSpendScan: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source spend-scan",
      manifestPosition: 399,
      publicationPosition: 256,
      referenceRolePosition: 489,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/spend_scan.main.spend",
      familyPaths: [["steps", "14"]],
    },
    fraudProofMissingScriptSourceMintGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source mint-grammar",
      manifestPosition: 400,
      publicationPosition: 257,
      referenceRolePosition: 490,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/mint_grammar.main.spend",
      familyPaths: [["steps", "15"]],
    },
    fraudProofMissingScriptSourceMintScan: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source mint-scan",
      manifestPosition: 401,
      publicationPosition: 258,
      referenceRolePosition: 491,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/mint_scan.main.spend",
      familyPaths: [["steps", "16"]],
    },
    fraudProofMissingScriptSourceReceiveStart: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source receive-start",
      manifestPosition: 402,
      publicationPosition: 259,
      referenceRolePosition: 492,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_start.main.spend",
      familyPaths: [["steps", "17"]],
    },
    fraudProofMissingScriptSourceReceiveGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source receive-grammar",
      manifestPosition: 403,
      publicationPosition: 260,
      referenceRolePosition: 493,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_grammar.main.spend",
      familyPaths: [["steps", "18"]],
    },
    fraudProofMissingScriptSourceReceiveOutputs: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source receive-outputs",
      manifestPosition: 404,
      publicationPosition: 261,
      referenceRolePosition: 494,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_outputs.main.spend",
      familyPaths: [["steps", "19"]],
    },
    fraudProofMissingScriptSourceReceiveFinish: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source receive-finish",
      manifestPosition: 405,
      publicationPosition: 262,
      referenceRolePosition: 495,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_finish.main.spend",
      familyPaths: [["steps", "20"]],
    },
    fraudProofMissingScriptSourceReceiveDeclaration: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source receive-declaration",
      manifestPosition: 406,
      publicationPosition: 263,
      referenceRolePosition: 496,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_declaration.main.spend",
      familyPaths: [["steps", "21"]],
    },
    fraudProofMissingScriptSourceSourceLookup: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source source-lookup",
      manifestPosition: 407,
      publicationPosition: 264,
      referenceRolePosition: 497,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_lookup.main.spend",
      familyPaths: [["steps", "22"]],
    },
    fraudProofMissingScriptSourceObservedDecision: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source observed-decision",
      manifestPosition: 408,
      publicationPosition: 265,
      referenceRolePosition: 498,
      commands: [],
      validatorTitle:
        "fraud_proofs/missing_script_source/observed_decision.main.spend",
      familyPaths: [["steps", "23"]],
    },
    fraudProofMissingScriptSourceClaimStart: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source claim-start",
      manifestPosition: 409,
      publicationPosition: 266,
      referenceRolePosition: 499,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_start.main.spend",
      familyPaths: [["steps", "24"]],
    },
    fraudProofMissingScriptSourceClaimSpendStart: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source claim-spend-start",
      manifestPosition: 410,
      publicationPosition: 267,
      referenceRolePosition: 500,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_spend_start.main.spend",
      familyPaths: [["steps", "25"]],
    },
    fraudProofMissingScriptSourceClaimSpendScan: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source claim-spend-scan",
      manifestPosition: 411,
      publicationPosition: 268,
      referenceRolePosition: 501,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_spend_scan.main.spend",
      familyPaths: [["steps", "26"]],
    },
    fraudProofMissingScriptSourceClaimMintGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source claim-mint-grammar",
      manifestPosition: 412,
      publicationPosition: 269,
      referenceRolePosition: 502,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_mint_grammar.main.spend",
      familyPaths: [["steps", "27"]],
    },
    fraudProofMissingScriptSourceClaimMintScan: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source claim-mint-scan",
      manifestPosition: 413,
      publicationPosition: 270,
      referenceRolePosition: 503,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_mint_scan.main.spend",
      familyPaths: [["steps", "28"]],
    },
    fraudProofMissingScriptSourceClaimReceiveGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof missing-script-source claim-receive-grammar",
      manifestPosition: 414,
      publicationPosition: 271,
      referenceRolePosition: 504,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_receive_grammar.main.spend",
      familyPaths: [["steps", "29"]],
    },
    fraudProofMissingScriptSourceClaimReceiveOutputs: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof missing-script-source claim-receive-outputs",
      manifestPosition: 415,
      publicationPosition: 272,
      referenceRolePosition: 505,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_receive_outputs.main.spend",
      familyPaths: [["steps", "30"]],
    },
    fraudProofMissingScriptSourceClaimReceiveFinish: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof missing-script-source claim-receive-finish",
      manifestPosition: 416,
      publicationPosition: 273,
      referenceRolePosition: 506,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_receive_finish.main.spend",
      familyPaths: [["steps", "31"]],
    },
    fraudProofMissingScriptSourceClaimReceiveDeclaration: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof missing-script-source claim-receive-declaration",
      manifestPosition: 417,
      publicationPosition: 274,
      referenceRolePosition: 507,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_receive_declaration.main.spend",
      familyPaths: [["steps", "32"]],
    },
    fraudProofMissingScriptSourceClaimAdjudicate: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source claim-adjudicate",
      manifestPosition: 418,
      publicationPosition: 275,
      referenceRolePosition: 508,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_adjudicate.main.spend",
      familyPaths: [["steps", "33"]],
    },
    fraudProofMissingScriptSourceClaimedRedeemerGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof missing-script-source claimed-redeemer-grammar",
      manifestPosition: 419,
      publicationPosition: 276,
      referenceRolePosition: 509,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claimed_redeemer_grammar.main.spend",
      familyPaths: [["steps", "34"]],
    },
    fraudProofMissingScriptSourceClaimedRedeemerScan: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof missing-script-source claimed-redeemer-scan",
      manifestPosition: 420,
      publicationPosition: 277,
      referenceRolePosition: 510,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claimed_redeemer_scan.main.spend",
      familyPaths: [["steps", "35"]],
    },
    fraudProofMissingScriptSourceTerminal: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source terminal",
      manifestPosition: 421,
      publicationPosition: 278,
      referenceRolePosition: 511,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_terminal.main.spend",
      familyPaths: [["steps", "36"]],
    },
  },
);
