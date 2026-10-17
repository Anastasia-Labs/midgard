import { spend } from "./recipes.types.js";

export const noReferenceInputRecipes = {
  fraudProofNoReferenceInput: spend(
    (c) => c.fraudProofContracts.noReferenceInput.firstStep,
  ),
  fraudProofNoReferenceInputStep02: spend(
    (c) => c.fraudProofContracts.noReferenceInput.steps[1],
  ),
  fraudProofNoReferenceInputStep03: spend(
    (c) => c.fraudProofContracts.noReferenceInput.steps[2],
  ),
  fraudProofNoReferenceInputStep04: spend(
    (c) => c.fraudProofContracts.noReferenceInput.steps[3],
  ),
} as const;
