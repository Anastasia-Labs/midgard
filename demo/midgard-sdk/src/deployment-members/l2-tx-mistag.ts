import { spend } from "./recipes.types.js";

export const l2TxMistagRecipes = {
  fraudProofL2TxMistag: spend(
    (c) => c.fraudProofContracts.l2TxMistag.firstStep,
  ),
  fraudProofL2TxMistagStep02: spend(
    (c) => c.fraudProofContracts.l2TxMistag.steps[1],
  ),
} as const;
