import { defineDeploymentFamily } from "../members.types.js";

export const missingRedeemerMembers = defineDeploymentFamily(
  "missingRedeemer",
  {
    fraudProofMissingRedeemer: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer step-01",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofMissingRedeemerBindEvent: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer bind-event",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofMissingRedeemerBindPreState: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer bind-pre-state",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofMissingRedeemerSourceStart: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer source-start",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_start.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofMissingRedeemerSourceResolved: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer source-resolved",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofMissingRedeemerSourceAuthenticate: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer source-authenticate",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofMissingRedeemerSourceMap: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer source-map",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_map.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofMissingRedeemerSourceInputFinish: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer source-input-finish",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofMissingRedeemerSourceGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer source-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofMissingRedeemerSourceWitnesses: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer source-witnesses",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofMissingRedeemerSourceFinish: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer source-finish",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_finish.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofMissingRedeemerFamilyEntry: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer family-entry",
      commands: [],
      validatorTitle: "fraud_proofs/missing_redeemer/family_entry.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofMissingRedeemerPurposeSelect: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer purpose-select",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/purpose_select.main.spend",
      familyPaths: [["steps", "12"]],
    },
    fraudProofMissingRedeemerSpendSelect: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer spend-select",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/spend_select.main.spend",
      familyPaths: [["steps", "13"]],
    },
    fraudProofMissingRedeemerSpendScan: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer spend-scan",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/spend_scan.main.spend",
      familyPaths: [["steps", "14"]],
    },
    fraudProofMissingRedeemerMintGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer mint-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/mint_grammar.main.spend",
      familyPaths: [["steps", "15"]],
    },
    fraudProofMissingRedeemerMintScan: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer mint-scan",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/mint_scan.main.spend",
      familyPaths: [["steps", "16"]],
    },
    fraudProofMissingRedeemerReceiveStart: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer receive-start",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_start.main.spend",
      familyPaths: [["steps", "17"]],
    },
    fraudProofMissingRedeemerReceiveGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer receive-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_grammar.main.spend",
      familyPaths: [["steps", "18"]],
    },
    fraudProofMissingRedeemerReceiveOutputs: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer receive-outputs",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_outputs.main.spend",
      familyPaths: [["steps", "19"]],
    },
    fraudProofMissingRedeemerReceiveFinish: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer receive-finish",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_finish.main.spend",
      familyPaths: [["steps", "20"]],
    },
    fraudProofMissingRedeemerReceiveDeclaration: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer receive-declaration",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_declaration.main.spend",
      familyPaths: [["steps", "21"]],
    },
    fraudProofMissingRedeemerSourceLookup: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer source-lookup",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_lookup.main.spend",
      familyPaths: [["steps", "22"]],
    },
    fraudProofMissingRedeemerStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer step-02",
      commands: [],
      validatorTitle: "fraud_proofs/missing_redeemer/step_02.main.spend",
      familyPaths: [["steps", "23"]],
    },
    fraudProofMissingRedeemerStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer step-03",
      commands: [],
      validatorTitle: "fraud_proofs/missing_redeemer/step_03.main.spend",
      familyPaths: [["steps", "24"]],
    },
    fraudProofMissingRedeemerStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer step-04",
      commands: [],
      validatorTitle: "fraud_proofs/missing_redeemer/step_04.main.spend",
      familyPaths: [["steps", "25"]],
    },
    fraudProofMissingRedeemerClaimStart: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer claim-start",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_start.main.spend",
      familyPaths: [["steps", "26"]],
    },
    fraudProofMissingRedeemerClaimSpendStart: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer claim-spend-start",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_spend_start.main.spend",
      familyPaths: [["steps", "27"]],
    },
    fraudProofMissingRedeemerClaimSpendScan: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer claim-spend-scan",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_spend_scan.main.spend",
      familyPaths: [["steps", "28"]],
    },
    fraudProofMissingRedeemerClaimMintGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer claim-mint-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_mint_grammar.main.spend",
      familyPaths: [["steps", "29"]],
    },
    fraudProofMissingRedeemerClaimMintScan: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer claim-mint-scan",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_mint_scan.main.spend",
      familyPaths: [["steps", "30"]],
    },
    fraudProofMissingRedeemerClaimReceiveGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer claim-receive-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_receive_grammar.main.spend",
      familyPaths: [["steps", "31"]],
    },
    fraudProofMissingRedeemerClaimReceiveOutputs: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer claim-receive-outputs",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_receive_outputs.main.spend",
      familyPaths: [["steps", "32"]],
    },
    fraudProofMissingRedeemerClaimReceiveFinish: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer claim-receive-finish",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_receive_finish.main.spend",
      familyPaths: [["steps", "33"]],
    },
    fraudProofMissingRedeemerClaimReceiveDeclaration: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof missing-redeemer claim-receive-declaration",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_receive_declaration.main.spend",
      familyPaths: [["steps", "34"]],
    },
    fraudProofMissingRedeemerClaimAdjudicate: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer claim-adjudicate",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_adjudicate.main.spend",
      familyPaths: [["steps", "35"]],
    },
    fraudProofMissingRedeemerClaimedRedeemerGrammar: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer claimed-redeemer-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claimed_redeemer_grammar.main.spend",
      familyPaths: [["steps", "36"]],
    },
    fraudProofMissingRedeemerClaimedRedeemerScan: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer claimed-redeemer-scan",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claimed_redeemer_scan.main.spend",
      familyPaths: [["steps", "37"]],
    },
    fraudProofMissingRedeemerStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof missing-redeemer step-05",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_terminal.main.spend",
      familyPaths: [["steps", "38"]],
    },
  },
);
