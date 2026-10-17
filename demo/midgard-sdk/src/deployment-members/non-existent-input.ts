import { spend } from "./recipes.types.js";

export const nonExistentInputRecipes = {
  fraudProofNonExistentInput: spend(
    (c) => c.fraudProofContracts.nonExistentInput.firstStep,
  ),
  fraudProofNonExistentInputStep02: spend(
    (c) => c.fraudProofContracts.nonExistentInput.steps[1],
  ),
  fraudProofNonExistentInputStep03: spend(
    (c) => c.fraudProofContracts.nonExistentInput.steps[2],
  ),
  fraudProofNonExistentInputStep04: spend(
    (c) => c.fraudProofContracts.nonExistentInput.steps[3],
  ),
} as const;
