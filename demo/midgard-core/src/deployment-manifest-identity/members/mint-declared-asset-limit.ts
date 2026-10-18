import { defineDeploymentFamily } from "../members.types.js";

export const mintDeclaredAssetLimitMembers = defineDeploymentFamily(
  "mintDeclaredAssetLimit",
  {
    fraudProofMintDeclaredAssetLimit: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof mint-declared-asset-limit step-01",
      manifestPosition: 292,
      publicationPosition: 198,
      referenceRolePosition: 430,
      commands: [],
      validatorTitle:
        "fraud_proofs/mint_declared_asset_limit/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofMintDeclaredAssetLimitStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof mint-declared-asset-limit step-02",
      manifestPosition: 293,
      publicationPosition: 199,
      referenceRolePosition: 431,
      commands: [],
      validatorTitle:
        "fraud_proofs/mint_declared_asset_limit/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofMintDeclaredAssetLimitStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof mint-declared-asset-limit step-03",
      manifestPosition: 294,
      publicationPosition: 200,
      referenceRolePosition: 432,
      commands: [],
      validatorTitle:
        "fraud_proofs/mint_declared_asset_limit/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofMintDeclaredAssetLimitStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof mint-declared-asset-limit step-04",
      manifestPosition: 295,
      publicationPosition: 201,
      referenceRolePosition: 433,
      commands: [],
      validatorTitle:
        "fraud_proofs/mint_declared_asset_limit/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
