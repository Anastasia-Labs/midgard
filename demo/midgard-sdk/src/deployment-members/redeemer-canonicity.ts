import { spend } from "./recipes.types.js";

export const redeemerCanonicityRecipes = {
  fraudProofRedeemerCanonicity: spend(
    (c) => c.fraudProofContracts.redeemerCanonicity.firstStep,
  ),
  fraudProofRedeemerCanonicityStep02: spend(
    (c) => c.fraudProofContracts.redeemerCanonicity.steps[1],
  ),
  fraudProofRedeemerCanonicityStep02Data: spend(
    (c) => c.fraudProofContracts.redeemerCanonicity.steps[2],
  ),
  fraudProofRedeemerCanonicityStep03: spend(
    (c) => c.fraudProofContracts.redeemerCanonicity.steps[3],
  ),
} as const;
