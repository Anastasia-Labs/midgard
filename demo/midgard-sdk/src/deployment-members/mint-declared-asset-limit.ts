import { spend } from "./recipes.types.js";

export const mintDeclaredAssetLimitRecipes = {
  fraudProofMintDeclaredAssetLimit: spend(
    (c) => c.fraudProofContracts.mintDeclaredAssetLimit.firstStep,
  ),
  fraudProofMintDeclaredAssetLimitStep02: spend(
    (c) => c.fraudProofContracts.mintDeclaredAssetLimit.steps[1],
  ),
  fraudProofMintDeclaredAssetLimitStep03: spend(
    (c) => c.fraudProofContracts.mintDeclaredAssetLimit.steps[2],
  ),
  fraudProofMintDeclaredAssetLimitStep04: spend(
    (c) => c.fraudProofContracts.mintDeclaredAssetLimit.steps[3],
  ),
} as const;
