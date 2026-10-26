import { defineDeploymentFamily } from "../members.types.js";

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
        "fraud_proofs/distinct_asset_accumulation_limit/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofDistinctAssetAccumulationLimitStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-02",
      manifestPosition: 396,
      publicationPosition: 378,
      referenceRolePosition: 611,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofDistinctAssetAccumulationLimitStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-03",
      manifestPosition: 397,
      publicationPosition: 379,
      referenceRolePosition: 612,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofDistinctAssetAccumulationLimitStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-04",
      manifestPosition: 398,
      publicationPosition: 380,
      referenceRolePosition: 613,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
    fraudProofDistinctAssetAccumulationLimitStep05: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-05",
      manifestPosition: 399,
      publicationPosition: 381,
      referenceRolePosition: 614,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_05.main.spend",
      familyPaths: [["steps", "4"]],
    },
    fraudProofDistinctAssetAccumulationLimitStep06: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof distinct-asset-accumulation-limit step-06",
      manifestPosition: 400,
      publicationPosition: 382,
      referenceRolePosition: 615,
      commands: [],
      validatorTitle:
        "fraud_proofs/distinct_asset_accumulation_limit/step_06.main.spend",
      familyPaths: [["steps", "5"]],
    },
  },
);
