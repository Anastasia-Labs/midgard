import { spend } from "./recipes.types.js";

export const zeroInputRecipes = {
  fraudProofZeroInput: spend((c) => c.fraudProofContracts.zeroInput.firstStep),
  fraudProofZeroInputStep02: spend(
    (c) => c.fraudProofContracts.zeroInput.steps[1],
  ),
} as const;
