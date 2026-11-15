import { defineDeploymentFamily } from "../members.types.js";

export const unusedScriptWitnessMembers = defineDeploymentFamily(
  "unusedScriptWitness",
  {
    fraudProofUnusedScriptWitness: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness step-01",
        token: "V1FpUnusedScriptWitnessS01",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofUnusedScriptWitnessBindEvent: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness bind-event",
        token: "V1FpUnusedWitBindEvent",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofUnusedScriptWitnessBindPreState: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness bind-pre-state",
        token: "V1FpUnusedWitBindPreState",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofUnusedScriptWitnessSourceStart: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness source-start",
        token: "V1FpUnusedWitSrcStart",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_start.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofUnusedScriptWitnessSourceResolved: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness source-resolved",
        token: "V1FpUnusedWitSrcResolved",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofUnusedScriptWitnessSourceAuthenticate: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness source-authenticate",
        token: "V1FpUnusedWitSrcAuth",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofUnusedScriptWitnessSourceMap: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness source-map",
        token: "V1FpUnusedWitSrcMap",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_map.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofUnusedScriptWitnessSourceInputFinish: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness source-input-finish",
        token: "V1FpUnusedWitSrcInputFinish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofUnusedScriptWitnessSourceGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness source-grammar",
        token: "V1FpUnusedWitSrcGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofUnusedScriptWitnessSourceWitnesses: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness source-witnesses",
        token: "V1FpUnusedWitSrcWitnesses",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofUnusedScriptWitnessSourceFinish: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness source-finish",
        token: "V1FpUnusedWitSrcFinish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_finish.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofUnusedScriptWitnessFamilyEntry: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness family-entry",
        token: "V1FpUnusedWitEntry",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/unused_script_witness/family_entry.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesSpend: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness all-purposes-spend",
        token: "V1FpUnusedWitApSpend",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_spend.main.spend",
      familyPaths: [["steps", "12"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesSpendResolve: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness all-purposes-spend-resolve",
        token: "V1FpUnusedWitApSpendResolve",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_spend_resolve.main.spend",
      familyPaths: [["steps", "13"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesMintGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness all-purposes-mint-grammar",
        token: "V1FpUnusedWitApMintGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_mint_grammar.main.spend",
      familyPaths: [["steps", "14"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesMint: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness all-purposes-mint",
        token: "V1FpUnusedWitApMint",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_mint.main.spend",
      familyPaths: [["steps", "15"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesObserveOpen: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness all-purposes-observe-open",
        token: "V1FpUnusedWitApObserveOpen",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_observe_open.main.spend",
      familyPaths: [["steps", "16"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesObserve: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness all-purposes-observe",
        token: "V1FpUnusedWitApObserve",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_observe.main.spend",
      familyPaths: [["steps", "17"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesReceiveGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness all-purposes-receive-grammar",
        token: "V1FpUnusedWitApRecvGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_receive_grammar.main.spend",
      familyPaths: [["steps", "18"]],
    },
    fraudProofUnusedScriptWitnessAllPurposesReceive: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness all-purposes-receive",
        token: "V1FpUnusedWitApRecv",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/all_purposes_receive.main.spend",
      familyPaths: [["steps", "19"]],
    },
    fraudProofUnusedScriptWitnessWitnessEntry: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness witness-entry",
        token: "V1FpUnusedWitWitnessEntry",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/unused_script_witness/witness_entry.main.spend",
      familyPaths: [["steps", "20"]],
    },
    fraudProofUnusedScriptWitnessWitnessGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness witness-grammar",
        token: "V1FpUnusedWitWitnessGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/unused_script_witness/witness_grammar.main.spend",
      familyPaths: [["steps", "21"]],
    },
    fraudProofUnusedScriptWitnessWitnessScan: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness witness-scan",
        token: "V1FpUnusedWitWitnessScan",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/unused_script_witness/witness_scan.main.spend",
      familyPaths: [["steps", "22"]],
    },
    fraudProofUnusedScriptWitnessTerminal: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof unused-script-witness terminal",
        token: "V1FpUnusedWitTerminal",
      },
      commands: [],
      validatorTitle: "fraud_proofs/unused_script_witness/terminal.main.spend",
      familyPaths: [["steps", "23"]],
    },
  },
);
