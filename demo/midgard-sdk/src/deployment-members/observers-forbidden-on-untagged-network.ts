import { spend } from "./recipes.types.js";

export const observersForbiddenOnUntaggedNetworkRecipes = {
  fraudProofObserversForbiddenOnUntaggedNetwork: spend(
    (c) => c.fraudProofContracts.observersForbiddenOnUntaggedNetwork.firstStep,
  ),
  fraudProofObserversForbiddenOnUntaggedNetworkStep02: spend(
    (c) => c.fraudProofContracts.observersForbiddenOnUntaggedNetwork.steps[1],
  ),
} as const;
