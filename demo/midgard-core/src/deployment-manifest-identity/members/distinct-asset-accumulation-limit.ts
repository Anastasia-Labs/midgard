import { defineDeploymentFamily } from "../members.types.js";

export const distinctAssetAccumulationLimitMembers = defineDeploymentFamily(
  "distinctAssetAccumulationLimit",
  {
    fraudProofDistinctAssetAccumulationLimit: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-01",
      manifestPosition: 363,
      publicationPosition: 282,
      referenceRolePosition: 515,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofDistinctAssetAccumulationLimitStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-02",
      manifestPosition: 364,
      publicationPosition: 283,
      referenceRolePosition: 516,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofDistinctAssetAccumulationLimitStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-03",
      manifestPosition: 365,
      publicationPosition: 284,
      referenceRolePosition: 517,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofDistinctAssetAccumulationLimitStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-04",
      manifestPosition: 366,
      publicationPosition: 285,
      referenceRolePosition: 518,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofDistinctAssetAccumulationLimitStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-05",
      manifestPosition: 367,
      publicationPosition: 286,
      referenceRolePosition: 519,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofDistinctAssetAccumulationLimitStep06: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-06",
      manifestPosition: 368,
      publicationPosition: 287,
      referenceRolePosition: 520,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_06.main.spend",
      familyPaths: [["steps", "5"]],
    },
  },
);
