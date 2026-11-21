import { defineDeploymentFamily } from "../members.types.js";

export const missingScriptSourceMembers = defineDeploymentFamily(
  "missingScriptSource",
  {
    fraudProofMissingScriptSource: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source step-01",
        token: "V1FpMissingSrcS01",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofMissingScriptSourceBindEvent: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source bind-event",
        token: "V1FpMissingSrcBindEvent",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofMissingScriptSourceBindPreState: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source bind-pre-state",
        token: "V1FpMissingSrcBindPreState",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofMissingScriptSourceSourceStart: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source source-start",
        token: "V1FpMissingSrcSrcStart",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_start.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofMissingScriptSourceSourceResolved: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source source-resolved",
        token: "V1FpMissingSrcSrcResolved",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_resolved.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofMissingScriptSourceSourceAuthenticate: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source source-authenticate",
        token: "V1FpMissingSrcSrcAuth",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_authenticate.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofMissingScriptSourceSourceMap: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source source-map",
        token: "V1FpMissingSrcSrcMap",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_map.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofMissingScriptSourceSourceInputFinish: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source source-input-finish",
        token: "V1FpMissingSrcSrcInputFinish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_input_finish.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofMissingScriptSourceSourceGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source source-grammar",
        token: "V1FpMissingSrcSrcGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_grammar.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofMissingScriptSourceSourceWitnesses: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source source-witnesses",
        token: "V1FpMissingSrcSrcWitnesses",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_witnesses.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofMissingScriptSourceSourceFinish: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source source-finish",
        token: "V1FpMissingSrcSrcFinish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_finish.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofMissingScriptSourceFamilyEntry: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source family-entry",
        token: "V1FpMissingSrcEntry",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/missing_script_source/family_entry.main.spend",
      familyPaths: [["steps", "11"]],
    },
    fraudProofMissingScriptSourcePurposeSelect: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source purpose-select",
        token: "V1FpMissingSrcPurposeSelect",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/purpose_select.main.spend",
      familyPaths: [["steps", "12"]],
    },
    fraudProofMissingScriptSourceSpendSelect: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source spend-select",
        token: "V1FpMissingSrcSpendSelect",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/spend_select.main.spend",
      familyPaths: [["steps", "13"]],
    },
    fraudProofMissingScriptSourceSpendScan: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source spend-scan",
        token: "V1FpMissingSrcSpendScan",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/spend_scan.main.spend",
      familyPaths: [["steps", "14"]],
    },
    fraudProofMissingScriptSourceMintGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source mint-grammar",
        token: "V1FpMissingSrcMintGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/mint_grammar.main.spend",
      familyPaths: [["steps", "15"]],
    },
    fraudProofMissingScriptSourceMintScan: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source mint-scan",
        token: "V1FpMissingSrcMintScan",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/mint_scan.main.spend",
      familyPaths: [["steps", "16"]],
    },
    fraudProofMissingScriptSourceReceiveStart: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source receive-start",
        token: "V1FpMissingSrcRecvStart",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_start.main.spend",
      familyPaths: [["steps", "17"]],
    },
    fraudProofMissingScriptSourceReceiveGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source receive-grammar",
        token: "V1FpMissingSrcRecvGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_grammar.main.spend",
      familyPaths: [["steps", "18"]],
    },
    fraudProofMissingScriptSourceReceiveOutputs: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source receive-outputs",
        token: "V1FpMissingSrcRecvOutputs",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_outputs.main.spend",
      familyPaths: [["steps", "19"]],
    },
    fraudProofMissingScriptSourceReceiveFinish: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source receive-finish",
        token: "V1FpMissingSrcRecvFinish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_finish.main.spend",
      familyPaths: [["steps", "20"]],
    },
    fraudProofMissingScriptSourceReceiveDeclaration: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source receive-declaration",
        token: "V1FpMissingSrcRecvDecl",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/receive_declaration.main.spend",
      familyPaths: [["steps", "21"]],
    },
    fraudProofMissingScriptSourceSourceLookup: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source source-lookup",
        token: "V1FpMissingSrcSrcLookup",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/source_lookup.main.spend",
      familyPaths: [["steps", "22"]],
    },
    fraudProofMissingScriptSourceObservedDecision: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source observed-decision",
        token: "V1FpMissingSrcObserved",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/missing_script_source/observed_decision.main.spend",
      familyPaths: [["steps", "23"]],
    },
    fraudProofMissingScriptSourceClaimStart: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source claim-start",
        token: "V1FpMissingSrcClaimStart",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_start.main.spend",
      familyPaths: [["steps", "24"]],
    },
    fraudProofMissingScriptSourceClaimSpendStart: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source claim-spend-start",
        token: "V1FpMissingSrcClaimSpendStart",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_spend_start.main.spend",
      familyPaths: [["steps", "25"]],
    },
    fraudProofMissingScriptSourceClaimSpendScan: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source claim-spend-scan",
        token: "V1FpMissingSrcClaimSpend",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_spend_scan.main.spend",
      familyPaths: [["steps", "26"]],
    },
    fraudProofMissingScriptSourceClaimMintGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source claim-mint-grammar",
        token: "V1FpMissingSrcClaimMintGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_mint_grammar.main.spend",
      familyPaths: [["steps", "27"]],
    },
    fraudProofMissingScriptSourceClaimMintScan: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source claim-mint-scan",
        token: "V1FpMissingSrcClaimMintScan",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_mint_scan.main.spend",
      familyPaths: [["steps", "28"]],
    },
    fraudProofMissingScriptSourceClaimReceiveGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source claim-receive-grammar",
        token: "V1FpMissingSrcClaimRecvGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_receive_grammar.main.spend",
      familyPaths: [["steps", "29"]],
    },
    fraudProofMissingScriptSourceClaimReceiveOutputs: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source claim-receive-outputs",
        token: "V1FpMissingSrcClaimRecvOutputs",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_receive_outputs.main.spend",
      familyPaths: [["steps", "30"]],
    },
    fraudProofMissingScriptSourceClaimReceiveFinish: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source claim-receive-finish",
        token: "V1FpMissingSrcClaimRecvFinish",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_receive_finish.main.spend",
      familyPaths: [["steps", "31"]],
    },
    fraudProofMissingScriptSourceClaimReceiveDeclaration: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source claim-receive-declaration",
        token: "V1FpMissingSrcClaimRecvDecl",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_receive_declaration.main.spend",
      familyPaths: [["steps", "32"]],
    },
    fraudProofMissingScriptSourceClaimAdjudicate: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source claim-adjudicate",
        token: "V1FpMissingSrcAdjudicate",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_adjudicate.main.spend",
      familyPaths: [["steps", "33"]],
    },
    fraudProofMissingScriptSourceClaimedRedeemerGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source claimed-redeemer-grammar",
        token: "V1FpMissingSrcClaimedGrammar",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claimed_redeemer_grammar.main.spend",
      familyPaths: [["steps", "34"]],
    },
    fraudProofMissingScriptSourceClaimedRedeemerScan: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source claimed-redeemer-scan",
        token: "V1FpMissingSrcClaimedScan",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claimed_redeemer_scan.main.spend",
      familyPaths: [["steps", "35"]],
    },
    fraudProofMissingScriptSourceTerminal: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof missing-script-source terminal",
        token: "V1FpMissingSrcTerminal",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/claim_terminal.main.spend",
      familyPaths: [["steps", "36"]],
    },
  },
);
