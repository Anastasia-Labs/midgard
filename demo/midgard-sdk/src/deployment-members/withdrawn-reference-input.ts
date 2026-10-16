import { spend } from "./recipes.types.js";

export const withdrawnReferenceInputRecipes = {
  fraudProofWithdrawnReferenceInput: spend(
    (c) => c.fraudProofContracts.withdrawnReferenceInput.firstStep,
  ),
  fraudProofWithdrawnReferenceInputStep02: spend(
    (c) => c.fraudProofContracts.withdrawnReferenceInput.steps[1],
  ),
  fraudProofWithdrawnReferenceInputStep03: spend(
    (c) => c.fraudProofContracts.withdrawnReferenceInput.steps[2],
  ),
} as const;
