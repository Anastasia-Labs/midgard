import { spend } from "./recipes.types.js";

export const withdrawalMistagRecipes = {
  fraudProofWithdrawalMistag: spend(
    (c) => c.fraudProofContracts.withdrawalMistag.firstStep,
  ),
  fraudProofWithdrawalMistagStep02: spend(
    (c) => c.fraudProofContracts.withdrawalMistag.steps[1],
  ),
  fraudProofWithdrawalMistagStep03: spend(
    (c) => c.fraudProofContracts.withdrawalMistag.steps[2],
  ),
  fraudProofWithdrawalMistagStep04: spend(
    (c) => c.fraudProofContracts.withdrawalMistag.steps[3],
  ),
  fraudProofWithdrawalMistagStep05: spend(
    (c) => c.fraudProofContracts.withdrawalMistag.steps[4],
  ),
} as const;
