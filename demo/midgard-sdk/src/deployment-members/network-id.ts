import { spend } from "./recipes.types.js";

export const networkIdRecipes = {
  fraudProofNetworkId: spend((c) => c.fraudProofContracts.networkId.firstStep),
  fraudProofNetworkIdStep02: spend(
    (c) => c.fraudProofContracts.networkId.steps[1],
  ),
  fraudProofNetworkIdForcedStep: spend(
    (c) => c.fraudProofContracts.networkId.forcedStep,
  ),
  fraudProofNetworkIdForcedScan: spend(
    (c) => c.fraudProofContracts.networkId.forcedScan,
  ),
} as const;
