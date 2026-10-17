import { spend } from "./recipes.types.js";

export const invalidRangeRecipes = {
  fraudProofInvalidRange: spend(
    (c) => c.fraudProofContracts.invalidRange.firstStep,
  ),
  fraudProofInvalidRangeStep02: spend(
    (c) => c.fraudProofContracts.invalidRange.steps[1],
  ),
} as const;
