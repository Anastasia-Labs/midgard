/** Reference-script-only Q35 computation-thread initialization. */
import {
  submitCanonicalDecodabilityInit,
  type SubmitCanonicalDecodabilityInitResult,
} from "../canonical-decodability/submit-canonical-decodability-init.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts-v1.js";
import type { NetworkIdContracts } from "./contracts-v1.js";
import type { NetworkIdCatalogueCategory } from "./submit-common-v1.js";

type BaseParams = Parameters<typeof submitCanonicalDecodabilityInit>[0];

export type SubmitNetworkIdInitParams = Omit<
  BaseParams,
  "contracts" | "category" | "witnessReferenceScripts"
> & {
  readonly contracts: NetworkIdContracts;
  readonly category: NetworkIdCatalogueCategory;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
};

export type SubmitNetworkIdInitResult = SubmitCanonicalDecodabilityInitResult;

/**
 * Init is family-invariant: catalogue membership authenticates Q35's exact
 * first-step hash before the computation-thread token is created.
 */
export const submitNetworkIdInit = async (
  params: SubmitNetworkIdInitParams,
): Promise<SubmitNetworkIdInitResult> => {
  if (params.witnessReferenceScripts == null) {
    throw new Error(
      "network-id init requires published computation-thread and PHAS reference scripts",
    );
  }
  return await submitCanonicalDecodabilityInit(params);
};
