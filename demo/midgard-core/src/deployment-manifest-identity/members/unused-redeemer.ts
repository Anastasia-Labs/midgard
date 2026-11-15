import { defineDeploymentFamily } from "../members.types.js";

export const unusedRedeemerMembers = defineDeploymentFamily("unusedRedeemer", {
  fraudProofUnusedRedeemer: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer step-01",
    commands: [],
    validatorTitle: "fraud_proofs/direct_script_derivation/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofUnusedRedeemerBindEvent: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer bind-event",
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/bind_event.main.spend",
    familyPaths: [["steps", "1"]],
  },
  fraudProofUnusedRedeemerBindPreState: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer bind-pre-state",
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
    familyPaths: [["steps", "2"]],
  },
  fraudProofUnusedRedeemerSourceStart: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer source-start",
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/source_start.main.spend",
    familyPaths: [["steps", "3"]],
  },
  fraudProofUnusedRedeemerSourceResolved: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer source-resolved",
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
    familyPaths: [["steps", "4"]],
  },
  fraudProofUnusedRedeemerSourceAuthenticate: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer source-authenticate",
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
    familyPaths: [["steps", "5"]],
  },
  fraudProofUnusedRedeemerSourceMap: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer source-map",
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/source_map.main.spend",
    familyPaths: [["steps", "6"]],
  },
  fraudProofUnusedRedeemerSourceInputFinish: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer source-input-finish",
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
    familyPaths: [["steps", "7"]],
  },
  fraudProofUnusedRedeemerSourceGrammar: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer source-grammar",
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
    familyPaths: [["steps", "8"]],
  },
  fraudProofUnusedRedeemerSourceWitnesses: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer source-witnesses",
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
    familyPaths: [["steps", "9"]],
  },
  fraudProofUnusedRedeemerSourceFinish: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer source-finish",
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/source_finish.main.spend",
    familyPaths: [["steps", "10"]],
  },
  fraudProofUnusedRedeemerFamilyEntry: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer family-entry",
    commands: [],
    validatorTitle: "fraud_proofs/unused_redeemer/family_entry.main.spend",
    familyPaths: [["steps", "11"]],
  },
  fraudProofUnusedRedeemerRedeemerGrammar: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer redeemer-grammar",
    commands: [],
    validatorTitle: "fraud_proofs/unused_redeemer/redeemer_grammar.main.spend",
    familyPaths: [["steps", "12"]],
  },
  fraudProofUnusedRedeemerRedeemerScan: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer redeemer-scan",
    commands: [],
    validatorTitle: "fraud_proofs/unused_redeemer/redeemer_scan.main.spend",
    familyPaths: [["steps", "13"]],
  },
  fraudProofUnusedRedeemerClaimStart: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer claim-start",
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_start.main.spend",
    familyPaths: [["steps", "14"]],
  },
  fraudProofUnusedRedeemerClaimSpendStart: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer claim-spend-start",
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_spend_start.main.spend",
    familyPaths: [["steps", "15"]],
  },
  fraudProofUnusedRedeemerClaimSpendScan: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer claim-spend-scan",
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_spend_scan.main.spend",
    familyPaths: [["steps", "16"]],
  },
  fraudProofUnusedRedeemerClaimMintGrammar: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer claim-mint-grammar",
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_mint_grammar.main.spend",
    familyPaths: [["steps", "17"]],
  },
  fraudProofUnusedRedeemerClaimMintScan: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer claim-mint-scan",
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_mint_scan.main.spend",
    familyPaths: [["steps", "18"]],
  },
  fraudProofUnusedRedeemerClaimReceiveGrammar: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer claim-receive-grammar",
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_receive_grammar.main.spend",
    familyPaths: [["steps", "19"]],
  },
  fraudProofUnusedRedeemerClaimReceiveOutputs: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer claim-receive-outputs",
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_receive_outputs.main.spend",
    familyPaths: [["steps", "20"]],
  },
  fraudProofUnusedRedeemerClaimReceiveFinish: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer claim-receive-finish",
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_receive_finish.main.spend",
    familyPaths: [["steps", "21"]],
  },
  fraudProofUnusedRedeemerClaimReceiveDeclaration: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer claim-receive-declaration",
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_receive_declaration.main.spend",
    familyPaths: [["steps", "22"]],
  },
  fraudProofUnusedRedeemerClaimAdjudicate: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer claim-adjudicate",
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_adjudicate.main.spend",
    familyPaths: [["steps", "23"]],
  },
  fraudProofUnusedRedeemerTerminal: {
    purpose: "spend",
    referenceRole: "V1 fraud-proof unused-redeemer terminal",
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_terminal.main.spend",
    familyPaths: [["steps", "24"]],
  },
});
