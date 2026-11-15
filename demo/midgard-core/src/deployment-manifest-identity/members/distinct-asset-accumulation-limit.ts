import { defineDeploymentFamily } from "../members.types.js";

// The first six roles keep the slots of the six steps they replace; the
// remaining six are appended after the last member, in step order.
export const distinctAssetAccumulationLimitMembers = defineDeploymentFamily(
  "distinctAssetAccumulationLimit",
  {
    fraudProofDistinctAssetAccumulationLimit: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-01",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofDistinctAssetAccumulationLimitBindEvent: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof distinct-asset-accumulation-limit bind-event",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofDistinctAssetAccumulationLimitBindPreState: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof distinct-asset-accumulation-limit bind-pre-state",
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofDistinctAssetAccumulationLimitFamilyEntry: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof distinct-asset-accumulation-limit family-entry",
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/family_entry.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofDistinctAssetAccumulationLimitInputs: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit inputs",
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/inputs.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofDistinctAssetAccumulationLimitInputsResolve: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof distinct-asset-accumulation-limit inputs-resolve",
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/inputs_resolve.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofDistinctAssetAccumulationLimitInputRank: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof distinct-asset-accumulation-limit input-rank",
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/input_rank.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofDistinctAssetAccumulationLimitOutputsGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof distinct-asset-accumulation-limit outputs-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/outputs_grammar.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofDistinctAssetAccumulationLimitOutputs: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit outputs",
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/outputs.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofDistinctAssetAccumulationLimitMintGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof distinct-asset-accumulation-limit mint-grammar",
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/mint_grammar.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofDistinctAssetAccumulationLimitMint: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit mint",
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/mint.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofDistinctAssetAccumulationLimitTerminal: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof distinct-asset-accumulation-limit terminal",
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/terminal.main.spend",
      familyPaths: [["steps", "11"]],
    },
  },
);
