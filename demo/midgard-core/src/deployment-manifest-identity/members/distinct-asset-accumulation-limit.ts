import { defineDeploymentFamily } from "../members.types.js";

export const distinctAssetAccumulationLimitMembers = defineDeploymentFamily(
  "distinctAssetAccumulationLimit",
  {
    fraudProofDistinctAssetAccumulationLimit: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-01",
      manifestPosition: 379,
      publicationPosition: 330,
      referenceRolePosition: 563,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofDistinctAssetAccumulationLimitStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-02",
      manifestPosition: 380,
      publicationPosition: 331,
      referenceRolePosition: 564,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofDistinctAssetAccumulationLimitStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-03",
      manifestPosition: 381,
      publicationPosition: 332,
      referenceRolePosition: 565,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofDistinctAssetAccumulationLimitStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-04",
      manifestPosition: 382,
      publicationPosition: 333,
      referenceRolePosition: 566,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofDistinctAssetAccumulationLimitStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-05",
      manifestPosition: 383,
      publicationPosition: 334,
      referenceRolePosition: 567,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofDistinctAssetAccumulationLimitStep06: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-06",
      manifestPosition: 384,
      publicationPosition: 335,
      referenceRolePosition: 568,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_06.main.spend",
      familyPaths: [["steps", "5"]],
    },
  },
);
