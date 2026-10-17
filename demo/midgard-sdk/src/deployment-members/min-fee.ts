import { spend } from "./recipes.types.js";

export const minFeeRecipes = {
  fraudProofMinFee: spend((c) => c.fraudProofContracts.minFee.firstStep),
  fraudProofMinFeeStep02: spend((c) => c.fraudProofContracts.minFee.steps[1]),
} as const;
