import { spend } from "./recipes.types.js";

export const doubleSpendRecipes = {
  fraudProofDoubleSpend: spend(
    (c) => c.fraudProofContracts.doubleSpend.firstStep,
  ),
  fraudProofDoubleSpendStep02: spend(
    (c) => c.fraudProofContracts.doubleSpend.steps[1],
  ),
  fraudProofDoubleSpendStep03: spend(
    (c) => c.fraudProofContracts.doubleSpend.steps[2],
  ),
  fraudProofDoubleSpendStep04: spend(
    (c) => c.fraudProofContracts.doubleSpend.steps[3],
  ),
} as const;
