import { spend } from "./recipes.types.js";

export const fabricatedDepositRecipes = {
  fraudProofFabricatedDeposit: spend(
    (c) => c.fraudProofContracts.fabricatedDeposit.firstStep,
  ),
  fraudProofFabricatedDepositStep02: spend(
    (c) => c.fraudProofContracts.fabricatedDeposit.steps[1],
  ),
  fraudProofFabricatedDepositStep03: spend(
    (c) => c.fraudProofContracts.fabricatedDeposit.steps[2],
  ),
  fraudProofFabricatedDepositStep04: spend(
    (c) => c.fraudProofContracts.fabricatedDeposit.steps[3],
  ),
} as const;
