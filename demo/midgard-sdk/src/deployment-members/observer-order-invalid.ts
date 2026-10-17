import { spend } from "./recipes.types.js";

export const observerOrderInvalidRecipes = {
  fraudProofObserverOrderInvalid: spend(
    (c) => c.fraudProofContracts.observerOrderInvalid.firstStep,
  ),
  fraudProofObserverOrderInvalidStep02: spend(
    (c) => c.fraudProofContracts.observerOrderInvalid.steps[1],
  ),
  fraudProofObserverOrderInvalidStep03: spend(
    (c) => c.fraudProofContracts.observerOrderInvalid.steps[2],
  ),
  fraudProofObserverOrderInvalidStep04: spend(
    (c) => c.fraudProofContracts.observerOrderInvalid.steps[3],
  ),
} as const;
