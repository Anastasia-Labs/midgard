import { spend } from "./recipes.types.js";

export const receivePurposeLanguageRecipes = {
  fraudProofReceivePurposeLanguage: spend(
    (c) => c.fraudProofContracts.receivePurposeLanguage.firstStep,
  ),
  fraudProofReceivePurposeLanguageStep02: spend(
    (c) => c.fraudProofContracts.receivePurposeLanguage.steps[1],
  ),
  fraudProofReceivePurposeLanguageStep03: spend(
    (c) => c.fraudProofContracts.receivePurposeLanguage.steps[2],
  ),
} as const;
