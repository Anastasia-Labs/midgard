import { spend } from "./recipes.types.js";

export const mintItemNonCanonicalRecipes = {
  fraudProofMintItemNonCanonical: spend(
    (c) => c.fraudProofContracts.mintItemNonCanonical.firstStep,
  ),
  fraudProofMintItemNonCanonicalStep02: spend(
    (c) => c.fraudProofContracts.mintItemNonCanonical.steps[1],
  ),
  fraudProofMintItemNonCanonicalStep03: spend(
    (c) => c.fraudProofContracts.mintItemNonCanonical.steps[2],
  ),
  fraudProofMintItemNonCanonicalStep04: spend(
    (c) => c.fraudProofContracts.mintItemNonCanonical.steps[3],
  ),
} as const;
