import { spend } from "./recipes.types.js";

export const unusedRedeemerRecipes = {
  fraudProofUnusedRedeemer: spend(
    (c) => c.fraudProofContracts.unusedRedeemer.firstStep,
  ),
  fraudProofUnusedRedeemerStep02: spend(
    (c) => c.fraudProofContracts.unusedRedeemer.steps[1],
  ),
  fraudProofUnusedRedeemerStep02a: spend(
    (c) => c.fraudProofContracts.unusedRedeemer.steps[2],
  ),
  fraudProofUnusedRedeemerStep02b: spend(
    (c) => c.fraudProofContracts.unusedRedeemer.steps[3],
  ),
  fraudProofUnusedRedeemerStep02c: spend(
    (c) => c.fraudProofContracts.unusedRedeemer.steps[4],
  ),
  fraudProofUnusedRedeemerStep03: spend(
    (c) => c.fraudProofContracts.unusedRedeemer.steps[5],
  ),
  fraudProofUnusedRedeemerStep04: spend(
    (c) => c.fraudProofContracts.unusedRedeemer.steps[6],
  ),
  fraudProofUnusedRedeemerStep05: spend(
    (c) => c.fraudProofContracts.unusedRedeemer.steps[7],
  ),
  fraudProofUnusedRedeemerStep06: spend(
    (c) => c.fraudProofContracts.unusedRedeemer.steps[8],
  ),
} as const;
