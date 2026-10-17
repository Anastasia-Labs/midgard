import { spend } from "./recipes.types.js";

export const daHashPreimageRecipes = {
  fraudProofDaHashPreimage: spend(
    (c) => c.fraudProofContracts.daHashPreimage.firstStep,
  ),
  fraudProofDaHashPreimageStep02: spend(
    (c) => c.fraudProofContracts.daHashPreimage.steps[1],
  ),
} as const;
