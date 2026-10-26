import { defineDeploymentFamily } from "../members.types.js";

// The first six roles keep the slots of the six steps they replace; the
// remaining six are appended after the last member, in step order.
export const distinctAssetAccumulationLimitMembers = defineDeploymentFamily(
  "distinctAssetAccumulationLimit",
  {
    fraudProofDistinctAssetAccumulationLimit: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-01",
      manifestPosition: 395,
      publicationPosition: 377,
      referenceRolePosition: 610,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofDistinctAssetAccumulationLimitBindEvent: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof distinct-asset-accumulation-limit bind-event",
      manifestPosition: 396,
      publicationPosition: 378,
      referenceRolePosition: 611,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_event.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofDistinctAssetAccumulationLimitBindPreState: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof distinct-asset-accumulation-limit bind-pre-state",
      manifestPosition: 397,
      publicationPosition: 379,
      referenceRolePosition: 612,
      commands: [],
      validatorTitle:
        "fraud_proofs/direct_script_derivation/bind_pre_state.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofDistinctAssetAccumulationLimitFamilyEntry: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof distinct-asset-accumulation-limit family-entry",
      manifestPosition: 398,
      publicationPosition: 380,
      referenceRolePosition: 613,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/family_entry.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofDistinctAssetAccumulationLimitInputs: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit inputs",
      manifestPosition: 399,
      publicationPosition: 381,
      referenceRolePosition: 614,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/inputs.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofDistinctAssetAccumulationLimitInputsResolve: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof distinct-asset-accumulation-limit inputs-resolve",
      manifestPosition: 400,
      publicationPosition: 382,
      referenceRolePosition: 615,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/inputs_resolve.main.spend",
      familyPaths: [["steps", "5"]],
    },
    fraudProofDistinctAssetAccumulationLimitInputRank: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof distinct-asset-accumulation-limit input-rank",
      manifestPosition: 661,
      publicationPosition: 656,
      referenceRolePosition: 656,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/input_rank.main.spend",
      familyPaths: [["steps", "6"]],
    },
    fraudProofDistinctAssetAccumulationLimitOutputsGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof distinct-asset-accumulation-limit outputs-grammar",
      manifestPosition: 662,
      publicationPosition: 657,
      referenceRolePosition: 657,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/outputs_grammar.main.spend",
      familyPaths: [["steps", "7"]],
    },
    fraudProofDistinctAssetAccumulationLimitOutputs: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit outputs",
      manifestPosition: 663,
      publicationPosition: 658,
      referenceRolePosition: 658,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/outputs.main.spend",
      familyPaths: [["steps", "8"]],
    },
    fraudProofDistinctAssetAccumulationLimitMintGrammar: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof distinct-asset-accumulation-limit mint-grammar",
      manifestPosition: 664,
      publicationPosition: 659,
      referenceRolePosition: 659,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/mint_grammar.main.spend",
      familyPaths: [["steps", "9"]],
    },
    fraudProofDistinctAssetAccumulationLimitMint: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit mint",
      manifestPosition: 665,
      publicationPosition: 660,
      referenceRolePosition: 660,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/mint.main.spend",
      familyPaths: [["steps", "10"]],
    },
    fraudProofDistinctAssetAccumulationLimitTerminal: {
      purpose: "spend",
      referenceRole:
        "V1 fraud-proof distinct-asset-accumulation-limit terminal",
      manifestPosition: 666,
      publicationPosition: 661,
      referenceRolePosition: 661,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/terminal.main.spend",
      familyPaths: [["steps", "11"]],
    },
  },
);
