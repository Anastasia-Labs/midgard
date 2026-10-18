import { defineDeploymentFamily } from "../members.types.js";

export const mintDeclaredAssetLimitMembers = defineDeploymentFamily(
  "mintDeclaredAssetLimit",
  {
    fraudProofMintDeclaredAssetLimit: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof mint-declared-asset-limit step-01",
      manifestPosition: 291,
      publicationPosition: 197,
      referenceRolePosition: 434,
      commands: [],
      validatorTitle:
        "fraud_proofs/mint_declared_asset_limit/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofMintDeclaredAssetLimitStep02: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof mint-declared-asset-limit step-02",
      manifestPosition: 292,
      publicationPosition: 198,
      referenceRolePosition: 435,
      commands: [],
      validatorTitle:
        "fraud_proofs/mint_declared_asset_limit/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofMintDeclaredAssetLimitStep03: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof mint-declared-asset-limit step-03",
      manifestPosition: 293,
      publicationPosition: 199,
      referenceRolePosition: 436,
      commands: [],
      validatorTitle:
        "fraud_proofs/mint_declared_asset_limit/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofMintDeclaredAssetLimitStep04: {
      purpose: "spend",
      referenceRole: "V1 fraud-proof mint-declared-asset-limit step-04",
      manifestPosition: 294,
      publicationPosition: 200,
      referenceRolePosition: 437,
      commands: [],
      validatorTitle:
        "fraud_proofs/mint_declared_asset_limit/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
