import { spend } from "./recipes.types.js";

export const distinctAssetAccumulationLimitRecipes = {
  fraudProofDistinctAssetAccumulationLimit: spend(
    (c) => c.fraudProofContracts.distinctAssetAccumulationLimit.firstStep,
  ),
  fraudProofDistinctAssetAccumulationLimitStep02: spend(
    (c) => c.fraudProofContracts.distinctAssetAccumulationLimit.steps[1],
  ),
  fraudProofDistinctAssetAccumulationLimitStep03: spend(
    (c) => c.fraudProofContracts.distinctAssetAccumulationLimit.steps[2],
  ),
  fraudProofDistinctAssetAccumulationLimitStep04: spend(
    (c) => c.fraudProofContracts.distinctAssetAccumulationLimit.steps[3],
  ),
  fraudProofDistinctAssetAccumulationLimitStep05: spend(
    (c) => c.fraudProofContracts.distinctAssetAccumulationLimit.steps[4],
  ),
  fraudProofDistinctAssetAccumulationLimitStep06: spend(
    (c) => c.fraudProofContracts.distinctAssetAccumulationLimit.steps[5],
  ),
} as const;
