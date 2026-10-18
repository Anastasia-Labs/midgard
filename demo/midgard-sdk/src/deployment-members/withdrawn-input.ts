import { spend } from "./recipes.types.js";

export const withdrawnInputRecipes = {
  fraudProofWithdrawnInput: spend(
    (c) => c.fraudProofContracts.withdrawnInput.firstStep,
  ),
  fraudProofWithdrawnInputStep02: spend(
    (c) => c.fraudProofContracts.withdrawnInput.steps[1],
  ),
  fraudProofWithdrawnInputStep03: spend(
    (c) => c.fraudProofContracts.withdrawnInput.steps[2],
  ),
} as const;
