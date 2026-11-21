/** Reference-script-only Q35 computation-thread initialization. */
import {
  buildCanonicalDecodabilityInit,
  type CanonicalDecodabilityInitBuild,
} from "../canonical-decodability/submit-canonical-decodability-init.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { NetworkIdContracts } from "./contracts.js";
import type { NetworkIdCatalogueCategory } from "./submit-common.js";

type BaseParams = Parameters<typeof buildCanonicalDecodabilityInit>[0];

export type SubmitNetworkIdInitParams = Omit<
  BaseParams,
  "contracts" | "category" | "witnessReferenceScripts"
> & {
  readonly contracts: NetworkIdContracts;
  readonly category: NetworkIdCatalogueCategory;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
};

export type NetworkIdInitBuild = CanonicalDecodabilityInitBuild;

/**
 * Init is family-invariant: catalogue membership authenticates Q35's exact
 * first-step hash before the computation-thread token is created.
 */
export const buildNetworkIdInit = async (
  params: SubmitNetworkIdInitParams,
): Promise<NetworkIdInitBuild> => {
  if (params.witnessReferenceScripts == null) {
    throw new Error(
      "network-id init requires published computation-thread and PHAS reference scripts",
    );
  }
  return await buildCanonicalDecodabilityInit(params);
};
