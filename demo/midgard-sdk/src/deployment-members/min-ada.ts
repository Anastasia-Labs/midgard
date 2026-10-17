import { spend, withdraw } from "./recipes.types.js";

export const minAdaRecipes = {
  fraudProofMinAda: spend((c) => c.fraudProofContracts.minAda.firstStep),
  fraudProofMinAdaStep02: spend((c) => c.fraudProofContracts.minAda.steps[1]),
  fraudProofMinAdaStep03: spend((c) => c.fraudProofContracts.minAda.steps[2]),
  fraudProofMinAdaStep04: spend((c) => c.fraudProofContracts.minAda.steps[3]),
  fraudProofMinAdaStep02TxWithdraw: withdraw(
    (c) => c.fraudProofContracts.minAda.yields.tx,
  ),
} as const;
