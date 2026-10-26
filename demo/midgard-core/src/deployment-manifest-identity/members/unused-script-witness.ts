import { defineDeploymentFamily } from "../members.types.js";

export const unusedScriptWitnessMembers = defineDeploymentFamily(
  "unusedScriptWitness",
  {
    fraudProofUnusedScriptWitness: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness step-01",
      manifestPosition: 330,
      publicationPosition: 236,
      referenceRolePosition: 469,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofUnusedScriptWitnessBindEvent: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness bind-event",
      manifestPosition: 331,
      publicationPosition: 237,
      referenceRolePosition: 470,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofUnusedScriptWitnessBindPreState: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness bind-pre-state",
      manifestPosition: 332,
      publicationPosition: 238,
      referenceRolePosition: 471,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofUnusedScriptWitnessSourceStart: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness source-start",
      manifestPosition: 333,
      publicationPosition: 239,
      referenceRolePosition: 472,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_start.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofUnusedScriptWitnessSourceResolved: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness source-resolved",
      manifestPosition: 334,
      publicationPosition: 240,
      referenceRolePosition: 473,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofUnusedScriptWitnessSourceAuthenticate: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness source-authenticate",
      manifestPosition: 335,
      publicationPosition: 241,
      referenceRolePosition: 474,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofUnusedScriptWitnessSourceMap: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness source-map",
      manifestPosition: 661,
      publicationPosition: 656,
      referenceRolePosition: 656,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_map.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofUnusedScriptWitnessSourceInputFinish: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness source-input-finish",
      manifestPosition: 662,
      publicationPosition: 657,
      referenceRolePosition: 657,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofUnusedScriptWitnessSourceGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness source-grammar",
      manifestPosition: 663,
      publicationPosition: 658,
      referenceRolePosition: 658,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofUnusedScriptWitnessSourceWitnesses: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness source-witnesses",
      manifestPosition: 664,
      publicationPosition: 659,
      referenceRolePosition: 659,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofUnusedScriptWitnessSourceFinish: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness source-finish",
      manifestPosition: 665,
      publicationPosition: 660,
      referenceRolePosition: 660,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_finish.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofUnusedScriptWitnessFamilyEntry: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness family-entry",
      manifestPosition: 666,
      publicationPosition: 661,
      referenceRolePosition: 661,
      commands: [],
      validatorTitle:
        "fraud_proofs/unused_script_witness/family_entry.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesSpend: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness all-purposes-spend",
      manifestPosition: 667,
      publicationPosition: 662,
      referenceRolePosition: 662,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_spend.main.spend",
      familyPaths: [["steps", "12"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesSpendResolve: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof unused-script-witness all-purposes-spend-resolve",
      manifestPosition: 668,
      publicationPosition: 663,
      referenceRolePosition: 663,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_spend_resolve.main.spend",
      familyPaths: [["steps", "13"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesMintGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof unused-script-witness all-purposes-mint-grammar",
      manifestPosition: 669,
      publicationPosition: 664,
      referenceRolePosition: 664,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_mint_grammar.main.spend",
      familyPaths: [["steps", "14"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesMint: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness all-purposes-mint",
      manifestPosition: 670,
      publicationPosition: 665,
      referenceRolePosition: 665,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_mint.main.spend",
      familyPaths: [["steps", "15"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesObserveOpen: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof unused-script-witness all-purposes-observe-open",
      manifestPosition: 671,
      publicationPosition: 666,
      referenceRolePosition: 666,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_observe_open.main.spend",
      familyPaths: [["steps", "16"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesObserve: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof unused-script-witness all-purposes-observe",
      manifestPosition: 672,
      publicationPosition: 667,
      referenceRolePosition: 667,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_observe.main.spend",
      familyPaths: [["steps", "17"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesReceiveGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof unused-script-witness all-purposes-receive-grammar",
      manifestPosition: 673,
      publicationPosition: 668,
      referenceRolePosition: 668,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_receive_grammar.main.spend",
      familyPaths: [["steps", "18"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesReceive: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof unused-script-witness all-purposes-receive",
      manifestPosition: 674,
      publicationPosition: 669,
      referenceRolePosition: 669,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_receive.main.spend",
      familyPaths: [["steps", "19"]],
    },
    fraudProofUnusedScriptWitnessWitnessEntry: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness witness-entry",
      manifestPosition: 675,
      publicationPosition: 670,
      referenceRolePosition: 670,
      commands: [],
      validatorTitle:
        "fraud_proofs/unused_script_witness/witness_entry.main.spend",
      familyPaths: [["steps", "20"]],
    },
    fraudProofUnusedScriptWitnessWitnessGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness witness-grammar",
      manifestPosition: 676,
      publicationPosition: 671,
      referenceRolePosition: 671,
      commands: [],
      validatorTitle:
        "fraud_proofs/unused_script_witness/witness_grammar.main.spend",
      familyPaths: [["steps", "21"]],
    },
    fraudProofUnusedScriptWitnessWitnessScan: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness witness-scan",
      manifestPosition: 677,
      publicationPosition: 672,
      referenceRolePosition: 672,
      commands: [],
      validatorTitle:
        "fraud_proofs/unused_script_witness/witness_scan.main.spend",
      familyPaths: [["steps", "22"]],
    },
    fraudProofUnusedScriptWitnessTerminal: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness terminal",
      manifestPosition: 678,
      publicationPosition: 673,
      referenceRolePosition: 673,
      commands: [],
      validatorTitle: "fraud_proofs/unused_script_witness/terminal.main.spend",
      familyPaths: [["steps", "23"]],
    },
  },
);
