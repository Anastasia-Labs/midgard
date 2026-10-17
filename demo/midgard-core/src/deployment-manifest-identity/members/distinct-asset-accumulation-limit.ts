import { defineDeploymentFamily } from "../members.types.js";

export const distinctAssetAccumulationLimitMembers = defineDeploymentFamily(
  "distinctAssetAccumulationLimit",
  {
    fraudProofDistinctAssetAccumulationLimit: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-01",
      manifestPosition: 357,
      publicationPosition: 276,
      referenceRolePosition: 506,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofDistinctAssetAccumulationLimitStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-02",
      manifestPosition: 358,
      publicationPosition: 277,
      referenceRolePosition: 507,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofDistinctAssetAccumulationLimitStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-03",
      manifestPosition: 359,
      publicationPosition: 278,
      referenceRolePosition: 508,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofDistinctAssetAccumulationLimitStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-04",
      manifestPosition: 360,
      publicationPosition: 279,
      referenceRolePosition: 509,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofDistinctAssetAccumulationLimitStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-05",
      manifestPosition: 361,
      publicationPosition: 280,
      referenceRolePosition: 510,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofDistinctAssetAccumulationLimitStep06: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-06",
      manifestPosition: 362,
      publicationPosition: 281,
      referenceRolePosition: 511,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_06.main.spend",
      familyPaths: [["steps", "5"]],
    },
  },
);
