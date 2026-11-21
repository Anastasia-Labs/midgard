import { defineDeploymentFamily } from "../members.types.js";

// The first six roles keep the slots of the six steps they replace; the
// remaining six are appended after the last member, in step order.
export const distinctAssetAccumulationLimitMembers = defineDeploymentFamily(
  "distinctAssetAccumulationLimit",
  {
    fraudProofDistinctAssetAccumulationLimit: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof distinct-asset-accumulation-limit step-01",
        token: "V1FpDistinctAssetLimitS01",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofDistinctAssetAccumulationLimitBindEvent: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof distinct-asset-accumulation-limit bind-event",
        token: "V1FpDistinctAssetLimitBindEvent",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofDistinctAssetAccumulationLimitBindPreState: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof distinct-asset-accumulation-limit bind-pre-state",
        token: "V1FpDistinctAssetLimitBindPre",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofDistinctAssetAccumulationLimitFamilyEntry: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof distinct-asset-accumulation-limit family-entry",
        token: "V1FpDistinctAssetLimitEntry",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/family_entry.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofDistinctAssetAccumulationLimitInputs: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof distinct-asset-accumulation-limit inputs",
        token: "V1FpDistinctAssetLimitInputs",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/inputs.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofDistinctAssetAccumulationLimitInputsResolve: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof distinct-asset-accumulation-limit inputs-resolve",
        token: "V1FpDistinctAssetLimitInResolve",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/inputs_resolve.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofDistinctAssetAccumulationLimitInputRank: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof distinct-asset-accumulation-limit input-rank",
        token: "V1FpDistinctAssetLimitInRank",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/input_rank.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofDistinctAssetAccumulationLimitOutputsGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof distinct-asset-accumulation-limit outputs-grammar",
        token: "V1FpDistinctAssetLimitOutGram",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/outputs_grammar.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofDistinctAssetAccumulationLimitOutputs: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof distinct-asset-accumulation-limit outputs",
        token: "V1FpDistinctAssetLimitOutputs",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/outputs.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofDistinctAssetAccumulationLimitMintGrammar: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof distinct-asset-accumulation-limit mint-grammar",
        token: "V1FpDistinctAssetLimitMintGram",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/mint_grammar.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofDistinctAssetAccumulationLimitMint: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof distinct-asset-accumulation-limit mint",
        token: "V1FpDistinctAssetLimitMint",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/mint.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofDistinctAssetAccumulationLimitTerminal: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof distinct-asset-accumulation-limit terminal",
        token: "V1FpDistinctAssetLimitTerminal",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/terminal.main.spend",
      familyPaths: [["steps", "11"]],
    },
  },
);
