import { defineDeploymentFamily } from "../members.types.js";

export const mintDeclaredAssetLimitMembers = defineDeploymentFamily(
  "mintDeclaredAssetLimit",
  {
    fraudProofMintDeclaredAssetLimit: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof mint-declared-asset-limit step-01",
        token: "V1FpMintAssetLimitS01",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/mint_declared_asset_limit/step_01.main.spend",
      familyPaths: [["firstStep"], ["steps", "0"]],
    },
    fraudProofMintDeclaredAssetLimitStep02: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof mint-declared-asset-limit step-02",
        token: "V1FpMintAssetLimitS02",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/mint_declared_asset_limit/step_02.main.spend",
      familyPaths: [["steps", "1"]],
    },
    fraudProofMintDeclaredAssetLimitStep03: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof mint-declared-asset-limit step-03",
        token: "V1FpMintAssetLimitS03",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/mint_declared_asset_limit/step_03.main.spend",
      familyPaths: [["steps", "2"]],
    },
    fraudProofMintDeclaredAssetLimitStep04: {
      purpose: "spend",
      reference: {
        role: "V1 fraud-proof mint-declared-asset-limit step-04",
        token: "V1FpMintAssetLimitS04",
      },
      commands: [],
      validatorTitle:
        "fraud_proofs/mint_declared_asset_limit/step_04.main.spend",
      familyPaths: [["steps", "3"]],
    },
  },
);
