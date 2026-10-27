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
      manifestPosition: 686,
      publicationPosition: 681,
      referenceRolePosition: 681,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_map.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofUnusedScriptWitnessSourceInputFinish: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness source-input-finish",
      manifestPosition: 687,
      publicationPosition: 682,
      referenceRolePosition: 682,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofUnusedScriptWitnessSourceGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness source-grammar",
      manifestPosition: 688,
      publicationPosition: 683,
      referenceRolePosition: 683,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofUnusedScriptWitnessSourceWitnesses: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness source-witnesses",
      manifestPosition: 689,
      publicationPosition: 684,
      referenceRolePosition: 684,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofUnusedScriptWitnessSourceFinish: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness source-finish",
      manifestPosition: 690,
      publicationPosition: 685,
      referenceRolePosition: 685,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_finish.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofUnusedScriptWitnessFamilyEntry: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness family-entry",
      manifestPosition: 691,
      publicationPosition: 686,
      referenceRolePosition: 686,
      commands: [],
      validatorTitle:
        "fraud_proofs/unused_script_witness/family_entry.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesSpend: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness all-purposes-spend",
      manifestPosition: 692,
      publicationPosition: 687,
      referenceRolePosition: 687,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_spend.main.spend",
      familyPaths: [["steps", "12"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesSpendResolve: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof unused-script-witness all-purposes-spend-resolve",
      manifestPosition: 693,
      publicationPosition: 688,
      referenceRolePosition: 688,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_spend_resolve.main.spend",
      familyPaths: [["steps", "13"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesMintGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof unused-script-witness all-purposes-mint-grammar",
      manifestPosition: 694,
      publicationPosition: 689,
      referenceRolePosition: 689,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_mint_grammar.main.spend",
      familyPaths: [["steps", "14"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesMint: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness all-purposes-mint",
      manifestPosition: 695,
      publicationPosition: 690,
      referenceRolePosition: 690,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_mint.main.spend",
      familyPaths: [["steps", "15"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesObserveOpen: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof unused-script-witness all-purposes-observe-open",
      manifestPosition: 696,
      publicationPosition: 691,
      referenceRolePosition: 691,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_observe_open.main.spend",
      familyPaths: [["steps", "16"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesObserve: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof unused-script-witness all-purposes-observe",
      manifestPosition: 697,
      publicationPosition: 692,
      referenceRolePosition: 692,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_observe.main.spend",
      familyPaths: [["steps", "17"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesReceiveGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof unused-script-witness all-purposes-receive-grammar",
      manifestPosition: 698,
      publicationPosition: 693,
      referenceRolePosition: 693,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_receive_grammar.main.spend",
      familyPaths: [["steps", "18"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesReceive: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof unused-script-witness all-purposes-receive",
      manifestPosition: 699,
      publicationPosition: 694,
      referenceRolePosition: 694,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_receive.main.spend",
      familyPaths: [["steps", "19"]],
    },
    fraudProofUnusedScriptWitnessWitnessEntry: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness witness-entry",
      manifestPosition: 700,
      publicationPosition: 695,
      referenceRolePosition: 695,
      commands: [],
      validatorTitle:
        "fraud_proofs/unused_script_witness/witness_entry.main.spend",
      familyPaths: [["steps", "20"]],
    },
    fraudProofUnusedScriptWitnessWitnessGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness witness-grammar",
      manifestPosition: 701,
      publicationPosition: 696,
      referenceRolePosition: 696,
      commands: [],
      validatorTitle:
        "fraud_proofs/unused_script_witness/witness_grammar.main.spend",
      familyPaths: [["steps", "21"]],
    },
    fraudProofUnusedScriptWitnessWitnessScan: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness witness-scan",
      manifestPosition: 702,
      publicationPosition: 697,
      referenceRolePosition: 697,
      commands: [],
      validatorTitle:
        "fraud_proofs/unused_script_witness/witness_scan.main.spend",
      familyPaths: [["steps", "22"]],
    },
    fraudProofUnusedScriptWitnessTerminal: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness terminal",
      manifestPosition: 703,
      publicationPosition: 698,
      referenceRolePosition: 698,
      commands: [],
      validatorTitle: "fraud_proofs/unused_script_witness/terminal.main.spend",
      familyPaths: [["steps", "23"]],
    },
  },
);
