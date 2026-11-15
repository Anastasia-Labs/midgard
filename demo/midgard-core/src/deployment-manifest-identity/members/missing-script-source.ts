import { defineDeploymentFamily } from "../members.types.js";

export const missingScriptSourceMembers = defineDeploymentFamily(
  "missingScriptSource",
  {
    fraudProofMissingScriptSource: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source step-01",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofMissingScriptSourceBindEvent: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source bind-event",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofMissingScriptSourceBindPreState: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source bind-pre-state",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofMissingScriptSourceSourceStart: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source source-start",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_start.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofMissingScriptSourceSourceResolved: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source source-resolved",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofMissingScriptSourceSourceAuthenticate: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source source-authenticate",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofMissingScriptSourceSourceMap: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source source-map",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_map.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofMissingScriptSourceSourceInputFinish: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source source-input-finish",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofMissingScriptSourceSourceGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source source-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofMissingScriptSourceSourceWitnesses: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source source-witnesses",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofMissingScriptSourceSourceFinish: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source source-finish",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_finish.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofMissingScriptSourceFamilyEntry: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source family-entry",
      commands: [],
      validatorTitle:
        "fraud_proofs/missing_script_source/family_entry.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofMissingScriptSourcePurposeSelect: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source purpose-select",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/purpose_select.main.spend",
      familyPaths: [["steps", "12"]],
    },
    fraudProofMissingScriptSourceSpendSelect: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source spend-select",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/spend_select.main.spend",
      familyPaths: [["steps", "13"]],
    },
    fraudProofMissingScriptSourceSpendScan: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source spend-scan",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/spend_scan.main.spend",
      familyPaths: [["steps", "14"]],
    },
    fraudProofMissingScriptSourceMintGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source mint-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/mint_grammar.main.spend",
      familyPaths: [["steps", "15"]],
    },
    fraudProofMissingScriptSourceMintScan: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source mint-scan",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/mint_scan.main.spend",
      familyPaths: [["steps", "16"]],
    },
    fraudProofMissingScriptSourceReceiveStart: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source receive-start",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_start.main.spend",
      familyPaths: [["steps", "17"]],
    },
    fraudProofMissingScriptSourceReceiveGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source receive-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_grammar.main.spend",
      familyPaths: [["steps", "18"]],
    },
    fraudProofMissingScriptSourceReceiveOutputs: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source receive-outputs",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_outputs.main.spend",
      familyPaths: [["steps", "19"]],
    },
    fraudProofMissingScriptSourceReceiveFinish: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source receive-finish",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_finish.main.spend",
      familyPaths: [["steps", "20"]],
    },
    fraudProofMissingScriptSourceReceiveDeclaration: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source receive-declaration",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_declaration.main.spend",
      familyPaths: [["steps", "21"]],
    },
    fraudProofMissingScriptSourceSourceLookup: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source source-lookup",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_lookup.main.spend",
      familyPaths: [["steps", "22"]],
    },
    fraudProofMissingScriptSourceObservedDecision: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source observed-decision",
      commands: [],
      validatorTitle:
        "fraud_proofs/missing_script_source/observed_decision.main.spend",
      familyPaths: [["steps", "23"]],
    },
    fraudProofMissingScriptSourceClaimStart: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source claim-start",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_start.main.spend",
      familyPaths: [["steps", "24"]],
    },
    fraudProofMissingScriptSourceClaimSpendStart: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source claim-spend-start",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_spend_start.main.spend",
      familyPaths: [["steps", "25"]],
    },
    fraudProofMissingScriptSourceClaimSpendScan: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source claim-spend-scan",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_spend_scan.main.spend",
      familyPaths: [["steps", "26"]],
    },
    fraudProofMissingScriptSourceClaimMintGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source claim-mint-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_mint_grammar.main.spend",
      familyPaths: [["steps", "27"]],
    },
    fraudProofMissingScriptSourceClaimMintScan: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source claim-mint-scan",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_mint_scan.main.spend",
      familyPaths: [["steps", "28"]],
    },
    fraudProofMissingScriptSourceClaimReceiveGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof missing-script-source claim-receive-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_receive_grammar.main.spend",
      familyPaths: [["steps", "29"]],
    },
    fraudProofMissingScriptSourceClaimReceiveOutputs: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof missing-script-source claim-receive-outputs",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_receive_outputs.main.spend",
      familyPaths: [["steps", "30"]],
    },
    fraudProofMissingScriptSourceClaimReceiveFinish: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof missing-script-source claim-receive-finish",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_receive_finish.main.spend",
      familyPaths: [["steps", "31"]],
    },
    fraudProofMissingScriptSourceClaimReceiveDeclaration: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof missing-script-source claim-receive-declaration",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_receive_declaration.main.spend",
      familyPaths: [["steps", "32"]],
    },
    fraudProofMissingScriptSourceClaimAdjudicate: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source claim-adjudicate",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_adjudicate.main.spend",
      familyPaths: [["steps", "33"]],
    },
    fraudProofMissingScriptSourceClaimedRedeemerGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof missing-script-source claimed-redeemer-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claimed_redeemer_grammar.main.spend",
      familyPaths: [["steps", "34"]],
    },
    fraudProofMissingScriptSourceClaimedRedeemerScan: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof missing-script-source claimed-redeemer-scan",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claimed_redeemer_scan.main.spend",
      familyPaths: [["steps", "35"]],
    },
    fraudProofMissingScriptSourceTerminal: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-script-source terminal",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_terminal.main.spend",
      familyPaths: [["steps", "36"]],
    },
  },
);
