import { spend } from "./recipes.types.js";

export const inputSetUniquenessRecipes = {
  fraudProofInputSetUniqueness: spend(
    (c) => c.fraudProofContracts.inputSetUniqueness.firstStep,
  ),
  fraudProofInputSetUniquenessStep02: spend(
    (c) => c.fraudProofContracts.inputSetUniqueness.steps[1],
  ),
  fraudProofInputSetUniquenessStep03: spend(
    (c) => c.fraudProofContracts.inputSetUniqueness.steps[2],
  ),
  fraudProofInputSetUniquenessStep04: spend(
    (c) => c.fraudProofContracts.inputSetUniqueness.steps[3],
  ),
} as const;
