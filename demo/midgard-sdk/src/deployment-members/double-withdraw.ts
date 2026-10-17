import { spend } from "./recipes.types.js";

export const doubleWithdrawRecipes = {
  fraudProofDoubleWithdraw: spend(
    (c) => c.fraudProofContracts.doubleWithdraw.firstStep,
  ),
  fraudProofDoubleWithdrawStep02: spend(
    (c) => c.fraudProofContracts.doubleWithdraw.steps[1],
  ),
} as const;
