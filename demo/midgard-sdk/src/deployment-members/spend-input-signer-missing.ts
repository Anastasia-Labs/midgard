import { spend } from "./recipes.types.js";

export const spendInputSignerMissingRecipes = {
  fraudProofSpendInputSignerMissing: spend(
    (c) => c.fraudProofContracts.spendInputSignerMissing.firstStep,
  ),
  fraudProofSpendInputSignerMissingStep02: spend(
    (c) => c.fraudProofContracts.spendInputSignerMissing.steps[1],
  ),
  fraudProofSpendInputSignerMissingStep03: spend(
    (c) => c.fraudProofContracts.spendInputSignerMissing.steps[2],
  ),
  fraudProofSpendInputSignerMissingStep04: spend(
    (c) => c.fraudProofContracts.spendInputSignerMissing.steps[3],
  ),
  fraudProofSpendInputSignerMissingStep05: spend(
    (c) => c.fraudProofContracts.spendInputSignerMissing.steps[4],
  ),
} as const;
