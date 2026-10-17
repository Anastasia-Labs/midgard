import { spend } from "./recipes.types.js";

export const fabricatedWithdrawalRecipes = {
  fraudProofFabricatedWithdrawal: spend(
    (c) => c.fraudProofContracts.fabricatedWithdrawal.firstStep,
  ),
  fraudProofFabricatedWithdrawalStep02: spend(
    (c) => c.fraudProofContracts.fabricatedWithdrawal.steps[1],
  ),
  fraudProofFabricatedWithdrawalStep03: spend(
    (c) => c.fraudProofContracts.fabricatedWithdrawal.steps[2],
  ),
  fraudProofFabricatedWithdrawalStep04: spend(
    (c) => c.fraudProofContracts.fabricatedWithdrawal.steps[3],
  ),
} as const;
