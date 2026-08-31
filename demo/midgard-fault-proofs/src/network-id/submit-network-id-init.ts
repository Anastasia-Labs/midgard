/** Reference-script-only Q35 computation-thread initialization. */
import {
  submitCanonicalDecodabilityInit,
  type SubmitCanonicalDecodabilityInitResult,
} from "../canonical-decodability/submit-canonical-decodability-init.js";
import type { FaultProofWitnessReferenceScriptsV1 } from "../witness-reference-scripts-v1.js";
import type { NetworkIdContractsV1 } from "./contracts-v1.js";
import type { NetworkIdCatalogueCategoryV1 } from "./submit-common-v1.js";

type BaseParams = Parameters<typeof submitCanonicalDecodabilityInit>[0];

export type SubmitNetworkIdInitParams = Omit<
  BaseParams,
  "contracts" | "category" | "witnessReferenceScripts"
> & {
  readonly contracts: NetworkIdContractsV1;
  readonly category: NetworkIdCatalogueCategoryV1;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScriptsV1;
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
