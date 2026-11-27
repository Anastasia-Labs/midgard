import { defineDeploymentFamily } from "../members.types.js";

export const missingRedeemerMembers = defineDeploymentFamily(
  "missingRedeemer",
  {
    fraudProofMissingRedeemer: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer step-01",
        token: "V1FpMissingRdmrS01",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofMissingRedeemerBindEvent: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer bind-event",
        token: "V1FpMissingRdmrBindEvent",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofMissingRedeemerBindPreState: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer bind-pre-state",
        token: "V1FpMissingRdmrBindPreState",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofMissingRedeemerSourceStart: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer source-start",
        token: "V1FpMissingRdmrSrcStart",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_start.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofMissingRedeemerSourceResolved: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer source-resolved",
        token: "V1FpMissingRdmrSrcResolved",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofMissingRedeemerSourceAuthenticate: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer source-authenticate",
        token: "V1FpMissingRdmrSrcAuth",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofMissingRedeemerSourceMap: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer source-map",
        token: "V1FpMissingRdmrSrcMap",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_map.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofMissingRedeemerSourceInputFinish: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer source-input-finish",
        token: "V1FpMissingRdmrSrcInputFinish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofMissingRedeemerSourceGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer source-grammar",
        token: "V1FpMissingRdmrSrcGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofMissingRedeemerSourceWitnesses: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer source-witnesses",
        token: "V1FpMissingRdmrSrcWitnesses",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofMissingRedeemerSourceFinish: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer source-finish",
        token: "V1FpMissingRdmrSrcFinish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_finish.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofMissingRedeemerFamilyEntry: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer family-entry",
        token: "V1FpMissingRdmrEntry",
      },
      commands: [],
      validatorTitle: "fraud_proofs/missing_redeemer/family_entry.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofMissingRedeemerPurposeSelect: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer purpose-select",
        token: "V1FpMissingRdmrPurposeSelect",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/purpose_select.main.spend",
      familyPaths: [["steps", "12"]],
    },
    fraudProofMissingRedeemerSpendSelect: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer spend-select",
        token: "V1FpMissingRdmrSpendSelect",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/spend_select.main.spend",
      familyPaths: [["steps", "13"]],
    },
    fraudProofMissingRedeemerSpendScan: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer spend-scan",
        token: "V1FpMissingRdmrSpendScan",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/spend_scan.main.spend",
      familyPaths: [["steps", "14"]],
    },
    fraudProofMissingRedeemerMintGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer mint-grammar",
        token: "V1FpMissingRdmrMintGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/mint_grammar.main.spend",
      familyPaths: [["steps", "15"]],
    },
    fraudProofMissingRedeemerMintScan: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer mint-scan",
        token: "V1FpMissingRdmrMintScan",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/mint_scan.main.spend",
      familyPaths: [["steps", "16"]],
    },
    fraudProofMissingRedeemerReceiveStart: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer receive-start",
        token: "V1FpMissingRdmrRecvStart",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_start.main.spend",
      familyPaths: [["steps", "17"]],
    },
    fraudProofMissingRedeemerReceiveGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer receive-grammar",
        token: "V1FpMissingRdmrRecvGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_grammar.main.spend",
      familyPaths: [["steps", "18"]],
    },
    fraudProofMissingRedeemerReceiveOutputs: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer receive-outputs",
        token: "V1FpMissingRdmrRecvOutputs",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_outputs.main.spend",
      familyPaths: [["steps", "19"]],
    },
    fraudProofMissingRedeemerReceiveFinish: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer receive-finish",
        token: "V1FpMissingRdmrRecvFinish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_finish.main.spend",
      familyPaths: [["steps", "20"]],
    },
    fraudProofMissingRedeemerReceiveDeclaration: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer receive-declaration",
        token: "V1FpMissingRdmrRecvDecl",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_declaration.main.spend",
      familyPaths: [["steps", "21"]],
    },
    fraudProofMissingRedeemerSourceLookup: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer source-lookup",
        token: "V1FpMissingRdmrSrcLookup",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_lookup.main.spend",
      familyPaths: [["steps", "22"]],
    },
    fraudProofMissingRedeemerStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer step-02",
        token: "V1FpMissingRdmrS02",
      },
      commands: [],
      validatorTitle: "fraud_proofs/missing_redeemer/step_02.main.spend",
      familyPaths: [["steps", "23"]],
    },
    fraudProofMissingRedeemerStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer step-03",
        token: "V1FpMissingRdmrS03",
      },
      commands: [],
      validatorTitle: "fraud_proofs/missing_redeemer/step_03.main.spend",
      familyPaths: [["steps", "24"]],
    },
    fraudProofMissingRedeemerStep04: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer step-04",
        token: "V1FpMissingRdmrS04",
      },
      commands: [],
      validatorTitle: "fraud_proofs/missing_redeemer/step_04.main.spend",
      familyPaths: [["steps", "25"]],
    },
    fraudProofMissingRedeemerClaimStart: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer claim-start",
        token: "V1FpMissingRdmrClaimStart",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_start.main.spend",
      familyPaths: [["steps", "26"]],
    },
    fraudProofMissingRedeemerClaimSpendStart: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer claim-spend-start",
        token: "V1FpMissingRdmrClaimSpendStart",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_spend_start.main.spend",
      familyPaths: [["steps", "27"]],
    },
    fraudProofMissingRedeemerClaimSpendScan: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer claim-spend-scan",
        token: "V1FpMissingRdmrClaimSpend",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_spend_scan.main.spend",
      familyPaths: [["steps", "28"]],
    },
    fraudProofMissingRedeemerClaimMintGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer claim-mint-grammar",
        token: "V1FpMissingRdmrClaimMintGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_mint_grammar.main.spend",
      familyPaths: [["steps", "29"]],
    },
    fraudProofMissingRedeemerClaimMintScan: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer claim-mint-scan",
        token: "V1FpMissingRdmrClaimMintScan",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_mint_scan.main.spend",
      familyPaths: [["steps", "30"]],
    },
    fraudProofMissingRedeemerClaimReceiveGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer claim-receive-grammar",
        token: "V1FpMissingRdmrClaimRecvGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_receive_grammar.main.spend",
      familyPaths: [["steps", "31"]],
    },
    fraudProofMissingRedeemerClaimReceiveOutputs: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer claim-receive-outputs",
        token: "V1FpMissingRdmrClaimRecvOutputs",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_receive_outputs.main.spend",
      familyPaths: [["steps", "32"]],
    },
    fraudProofMissingRedeemerClaimReceiveFinish: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer claim-receive-finish",
        token: "V1FpMissingRdmrClaimRecvFinish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_receive_finish.main.spend",
      familyPaths: [["steps", "33"]],
    },
    fraudProofMissingRedeemerClaimReceiveDeclaration: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer claim-receive-declaration",
        token: "V1FpMissingRdmrClaimRecvDecl",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_receive_declaration.main.spend",
      familyPaths: [["steps", "34"]],
    },
    fraudProofMissingRedeemerClaimAdjudicate: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer claim-adjudicate",
        token: "V1FpMissingRdmrAdjudicate",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_adjudicate.main.spend",
      familyPaths: [["steps", "35"]],
    },
    fraudProofMissingRedeemerClaimedRedeemerGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer claimed-redeemer-grammar",
        token: "V1FpMissingRdmrClaimedGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claimed_redeemer_grammar.main.spend",
      familyPaths: [["steps", "36"]],
    },
    fraudProofMissingRedeemerClaimedRedeemerScan: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer claimed-redeemer-scan",
        token: "V1FpMissingRdmrClaimedScan",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claimed_redeemer_scan.main.spend",
      familyPaths: [["steps", "37"]],
    },
    fraudProofMissingRedeemerStep05: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-redeemer step-05",
        token: "V1FpMissingRdmrS05",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_terminal.main.spend",
      familyPaths: [["steps", "38"]],
    },
  },
);
