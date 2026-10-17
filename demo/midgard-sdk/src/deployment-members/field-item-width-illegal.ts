import { spend } from "./recipes.types.js";

export const fieldItemWidthIllegalRecipes = {
  fraudProofFieldItemWidthIllegal: spend(
    (c) => c.fraudProofContracts.fieldItemWidthIllegal.firstStep,
  ),
  fraudProofFieldItemWidthIllegalStep02: spend(
    (c) => c.fraudProofContracts.fieldItemWidthIllegal.steps[1],
  ),
  fraudProofFieldItemWidthIllegalStep03: spend(
    (c) => c.fraudProofContracts.fieldItemWidthIllegal.steps[2],
  ),
} as const;
