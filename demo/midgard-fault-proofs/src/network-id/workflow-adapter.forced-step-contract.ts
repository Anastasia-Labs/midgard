import type { UTxO } from "@lucid-evolution/lucid";

import type { NetworkIdWorkflowAdapterConfig } from "./workflow-adapter.create-network-id-raw-l1-observation-port.js";
export const createNetworkIdForcedStepContract = (
  config: NetworkIdWorkflowAdapterConfig,
) => {
  const requireForcedStep = () => {
    const forcedStep = config.contracts.forcedStep;
    if (forcedStep === undefined) {
      throw new Error(
        "network-id forced direction requires the deployed forced step",
      );
    }
    return forcedStep;
  };

  const requireForcedStepReferenceScript = (): UTxO => {
    const referenceScript = config.forcedStepReferenceScript;
    if (referenceScript === undefined) {
      throw new Error(
        "network-id forced direction requires the published fraudProofNetworkIdForcedStep reference script",
      );
    }
    return referenceScript;
  };

  return { requireForcedStep, requireForcedStepReferenceScript };
};
