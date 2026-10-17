import { spend } from "./recipes.types.js";

export const missingRedeemerRecipes = {
  fraudProofMissingRedeemer: spend(
    (c) => c.fraudProofContracts.missingRedeemer.firstStep,
  ),
  fraudProofMissingRedeemerStep02: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[1],
  ),
  fraudProofMissingRedeemerStep02a: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[2],
  ),
  fraudProofMissingRedeemerStep02b: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[3],
  ),
  fraudProofMissingRedeemerStep03: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[4],
  ),
  fraudProofMissingRedeemerStep04: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[5],
  ),
  fraudProofMissingRedeemerStep05: spend(
    (c) => c.fraudProofContracts.missingRedeemer.steps[6],
  ),
} as const;
