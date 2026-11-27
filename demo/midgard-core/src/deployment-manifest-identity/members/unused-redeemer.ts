import { defineDeploymentFamily } from "../members.types.js";

export const unusedRedeemerMembers = defineDeploymentFamily("unusedRedeemer", {
  fraudProofUnusedRedeemer: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer step-01",
      token: "V1FpUnusedRdmrS01",
    },
    commands: [],
    validatorTitle: "fraud_proofs/direct_script_derivation/step_01.main.spend",
    familyPaths: [["firstStep"], ["steps", "0"]],
  },
  fraudProofUnusedRedeemerBindEvent: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer bind-event",
      token: "V1FpUnusedRdmrBindEvent",
    },
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/bind_event.main.spend",
    familyPaths: [["steps", "1"]],
  },
  fraudProofUnusedRedeemerBindPreState: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer bind-pre-state",
      token: "V1FpUnusedRdmrBindPreState",
    },
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
    familyPaths: [["steps", "2"]],
  },
  fraudProofUnusedRedeemerSourceStart: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer source-start",
      token: "V1FpUnusedRdmrSrcStart",
    },
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/source_start.main.spend",
    familyPaths: [["steps", "3"]],
  },
  fraudProofUnusedRedeemerSourceResolved: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer source-resolved",
      token: "V1FpUnusedRdmrSrcResolved",
    },
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
    familyPaths: [["steps", "4"]],
  },
  fraudProofUnusedRedeemerSourceAuthenticate: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer source-authenticate",
      token: "V1FpUnusedRdmrSrcAuth",
    },
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
    familyPaths: [["steps", "5"]],
  },
  fraudProofUnusedRedeemerSourceMap: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer source-map",
      token: "V1FpUnusedRdmrSrcMap",
    },
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/source_map.main.spend",
    familyPaths: [["steps", "6"]],
  },
  fraudProofUnusedRedeemerSourceInputFinish: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer source-input-finish",
      token: "V1FpUnusedRdmrSrcInputFinish",
    },
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
    familyPaths: [["steps", "7"]],
  },
  fraudProofUnusedRedeemerSourceGrammar: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer source-grammar",
      token: "V1FpUnusedRdmrSrcGrammar",
    },
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
    familyPaths: [["steps", "8"]],
  },
  fraudProofUnusedRedeemerSourceWitnesses: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer source-witnesses",
      token: "V1FpUnusedRdmrSrcWitnesses",
    },
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
    familyPaths: [["steps", "9"]],
  },
  fraudProofUnusedRedeemerSourceFinish: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer source-finish",
      token: "V1FpUnusedRdmrSrcFinish",
    },
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/source_finish.main.spend",
    familyPaths: [["steps", "10"]],
  },
  fraudProofUnusedRedeemerFamilyEntry: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer family-entry",
      token: "V1FpUnusedRdmrEntry",
    },
    commands: [],
    validatorTitle: "fraud_proofs/unused_redeemer/family_entry.main.spend",
    familyPaths: [["steps", "11"]],
  },
  fraudProofUnusedRedeemerRedeemerGrammar: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer redeemer-grammar",
      token: "V1FpUnusedRdmrRdmrGrammar",
    },
    commands: [],
    validatorTitle: "fraud_proofs/unused_redeemer/redeemer_grammar.main.spend",
    familyPaths: [["steps", "12"]],
  },
  fraudProofUnusedRedeemerRedeemerScan: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer redeemer-scan",
      token: "V1FpUnusedRdmrRdmrScan",
    },
    commands: [],
    validatorTitle: "fraud_proofs/unused_redeemer/redeemer_scan.main.spend",
    familyPaths: [["steps", "13"]],
  },
  fraudProofUnusedRedeemerClaimStart: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer claim-start",
      token: "V1FpUnusedRdmrClaimStart",
    },
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_start.main.spend",
    familyPaths: [["steps", "14"]],
  },
  fraudProofUnusedRedeemerClaimSpendStart: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer claim-spend-start",
      token: "V1FpUnusedRdmrClaimSpendStart",
    },
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_spend_start.main.spend",
    familyPaths: [["steps", "15"]],
  },
  fraudProofUnusedRedeemerClaimSpendScan: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer claim-spend-scan",
      token: "V1FpUnusedRdmrClaimSpend",
    },
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_spend_scan.main.spend",
    familyPaths: [["steps", "16"]],
  },
  fraudProofUnusedRedeemerClaimMintGrammar: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer claim-mint-grammar",
      token: "V1FpUnusedRdmrClaimMintGrammar",
    },
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_mint_grammar.main.spend",
    familyPaths: [["steps", "17"]],
  },
  fraudProofUnusedRedeemerClaimMintScan: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer claim-mint-scan",
      token: "V1FpUnusedRdmrClaimMintScan",
    },
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_mint_scan.main.spend",
    familyPaths: [["steps", "18"]],
  },
  fraudProofUnusedRedeemerClaimReceiveGrammar: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer claim-receive-grammar",
      token: "V1FpUnusedRdmrClaimRecvGrammar",
    },
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_receive_grammar.main.spend",
    familyPaths: [["steps", "19"]],
  },
  fraudProofUnusedRedeemerClaimReceiveOutputs: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer claim-receive-outputs",
      token: "V1FpUnusedRdmrClaimRecvOutputs",
    },
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_receive_outputs.main.spend",
    familyPaths: [["steps", "20"]],
  },
  fraudProofUnusedRedeemerClaimReceiveFinish: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer claim-receive-finish",
      token: "V1FpUnusedRdmrClaimRecvFinish",
    },
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_receive_finish.main.spend",
    familyPaths: [["steps", "21"]],
  },
  fraudProofUnusedRedeemerClaimReceiveDeclaration: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer claim-receive-declaration",
      token: "V1FpUnusedRdmrClaimRecvDecl",
    },
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_receive_declaration.main.spend",
    familyPaths: [["steps", "22"]],
  },
  fraudProofUnusedRedeemerClaimAdjudicate: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer claim-adjudicate",
      token: "V1FpUnusedRdmrAdjudicate",
    },
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_adjudicate.main.spend",
    familyPaths: [["steps", "23"]],
  },
  fraudProofUnusedRedeemerTerminal: {
    purpose: "spend",
    reference: {
      role: "V1 fraud-proof unused-redeemer terminal",
      token: "V1FpUnusedRdmrTerminal",
    },
    commands: [],
    validatorTitle:
      "fraud_proofs/direct_script_derivation/claim_terminal.main.spend",
    familyPaths: [["steps", "24"]],
  },
});
