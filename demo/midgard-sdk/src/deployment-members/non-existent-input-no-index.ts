import { spend } from "./recipes.types.js";

export const nonExistentInputNoIndexRecipes = {
  fraudProofNonExistentInputNoIndex: spend(
    (c) => c.fraudProofContracts.nonExistentInputNoIndex.firstStep,
  ),
  fraudProofNonExistentInputNoIndexStep02: spend(
    (c) => c.fraudProofContracts.nonExistentInputNoIndex.steps[1],
  ),
  fraudProofNonExistentInputNoIndexStep03: spend(
    (c) => c.fraudProofContracts.nonExistentInputNoIndex.steps[2],
  ),
  fraudProofNonExistentInputNoIndexStep04: spend(
    (c) => c.fraudProofContracts.nonExistentInputNoIndex.steps[3],
  ),
} as const;
