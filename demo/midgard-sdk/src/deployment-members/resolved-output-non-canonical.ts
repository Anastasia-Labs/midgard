import { spend } from "./recipes.types.js";

export const resolvedOutputNonCanonicalRecipes = {
  fraudProofResolvedOutputNonCanonical: spend(
    (c) => c.fraudProofContracts.resolvedOutputNonCanonical.firstStep,
  ),
  fraudProofResolvedOutputNonCanonicalStep02: spend(
    (c) => c.fraudProofContracts.resolvedOutputNonCanonical.steps[1],
  ),
  fraudProofResolvedOutputNonCanonicalStep03: spend(
    (c) => c.fraudProofContracts.resolvedOutputNonCanonical.steps[2],
  ),
  fraudProofResolvedOutputNonCanonicalStep04: spend(
    (c) => c.fraudProofContracts.resolvedOutputNonCanonical.steps[3],
  ),
  fraudProofResolvedOutputNonCanonicalStep04Datum: spend(
    (c) => c.fraudProofContracts.resolvedOutputNonCanonical.steps[4],
  ),
  fraudProofResolvedOutputNonCanonicalStep05: spend(
    (c) => c.fraudProofContracts.resolvedOutputNonCanonical.steps[5],
  ),
} as const;
