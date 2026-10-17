import { defineDeploymentFamily } from "../members.types.js";

export const mintDeclaredAssetLimitMembers = defineDeploymentFamily(
  "mintDeclaredAssetLimit",
  {
    fraudProofMintDeclaredAssetLimit: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof mint-declared-asset-limit step-01",
      manifestPosition: 286,
      publicationPosition: 192,
      referenceRolePosition: 422,
      commands: [],
      validatorTitle:
        "fraud_proofs/mint_declared_asset_limit/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofMintDeclaredAssetLimitStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof mint-declared-asset-limit step-02",
      manifestPosition: 287,
      publicationPosition: 193,
      referenceRolePosition: 423,
      commands: [],
      validatorTitle:
        "fraud_proofs/mint_declared_asset_limit/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofMintDeclaredAssetLimitStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof mint-declared-asset-limit step-03",
      manifestPosition: 288,
      publicationPosition: 194,
      referenceRolePosition: 424,
      commands: [],
      validatorTitle:
        "fraud_proofs/mint_declared_asset_limit/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofMintDeclaredAssetLimitStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof mint-declared-asset-limit step-04",
      manifestPosition: 289,
      publicationPosition: 195,
      referenceRolePosition: 425,
      commands: [],
      validatorTitle:
        "fraud_proofs/mint_declared_asset_limit/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
