import { spend } from "./recipes.types.js";

export const canonicalDecodabilityRecipes = {
  fraudProofCanonicalDecodability: spend(
    (c) => c.fraudProofContracts.canonicalDecodability.firstStep,
  ),
  fraudProofCanonicalDecodabilityStep02: spend(
    (c) => c.fraudProofContracts.canonicalDecodability.steps[1],
  ),
} as const;
