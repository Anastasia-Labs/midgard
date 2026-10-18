import { spend } from "./recipes.types.js";

export const invalidSignatureRecipes = {
  fraudProofInvalidSignature: spend(
    (c) => c.fraudProofContracts.invalidSignature.firstStep,
  ),
  fraudProofInvalidSignatureStep02: spend(
    (c) => c.fraudProofContracts.invalidSignature.steps[1],
  ),
} as const;
