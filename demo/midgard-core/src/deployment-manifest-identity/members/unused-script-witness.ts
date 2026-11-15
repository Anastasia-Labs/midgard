import { defineDeploymentFamily } from "../members.types.js";

export const unusedScriptWitnessMembers = defineDeploymentFamily(
  "unusedScriptWitness",
  {
    fraudProofUnusedScriptWitness: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness step-01",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofUnusedScriptWitnessBindEvent: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness bind-event",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofUnusedScriptWitnessBindPreState: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness bind-pre-state",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofUnusedScriptWitnessSourceStart: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness source-start",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_start.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofUnusedScriptWitnessSourceResolved: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness source-resolved",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofUnusedScriptWitnessSourceAuthenticate: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness source-authenticate",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofUnusedScriptWitnessSourceMap: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness source-map",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_map.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofUnusedScriptWitnessSourceInputFinish: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness source-input-finish",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofUnusedScriptWitnessSourceGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness source-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofUnusedScriptWitnessSourceWitnesses: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness source-witnesses",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofUnusedScriptWitnessSourceFinish: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness source-finish",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_finish.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofUnusedScriptWitnessFamilyEntry: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness family-entry",
      commands: [],
      validatorTitle:
        "fraud_proofs/unused_script_witness/family_entry.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesSpend: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness all-purposes-spend",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_spend.main.spend",
      familyPaths: [["steps", "12"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesSpendResolve: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof unused-script-witness all-purposes-spend-resolve",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_spend_resolve.main.spend",
      familyPaths: [["steps", "13"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesMintGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof unused-script-witness all-purposes-mint-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_mint_grammar.main.spend",
      familyPaths: [["steps", "14"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesMint: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness all-purposes-mint",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_mint.main.spend",
      familyPaths: [["steps", "15"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesObserveOpen: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof unused-script-witness all-purposes-observe-open",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_observe_open.main.spend",
      familyPaths: [["steps", "16"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesObserve: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof unused-script-witness all-purposes-observe",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_observe.main.spend",
      familyPaths: [["steps", "17"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesReceiveGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof unused-script-witness all-purposes-receive-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_receive_grammar.main.spend",
      familyPaths: [["steps", "18"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesReceive: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof unused-script-witness all-purposes-receive",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_receive.main.spend",
      familyPaths: [["steps", "19"]],
    },
    fraudProofUnusedScriptWitnessWitnessEntry: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness witness-entry",
      commands: [],
      validatorTitle:
        "fraud_proofs/unused_script_witness/witness_entry.main.spend",
      familyPaths: [["steps", "20"]],
    },
    fraudProofUnusedScriptWitnessWitnessGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness witness-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/unused_script_witness/witness_grammar.main.spend",
      familyPaths: [["steps", "21"]],
    },
    fraudProofUnusedScriptWitnessWitnessScan: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness witness-scan",
      commands: [],
      validatorTitle:
        "fraud_proofs/unused_script_witness/witness_scan.main.spend",
      familyPaths: [["steps", "22"]],
    },
    fraudProofUnusedScriptWitnessTerminal: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof unused-script-witness terminal",
      commands: [],
      validatorTitle: "fraud_proofs/unused_script_witness/terminal.main.spend",
      familyPaths: [["steps", "23"]],
    },
  },
);
