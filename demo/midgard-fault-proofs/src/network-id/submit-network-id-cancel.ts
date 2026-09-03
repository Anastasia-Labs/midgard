/** Explicit Q35 prover cancellation from either published step. */
import {
  submitCanonicalDecodabilityCancel,
  type SubmitCanonicalDecodabilityCancelResult,
} from "../canonical-decodability/submit-canonical-decodability-cancel.js";
import type { FaultProofWitnessReferenceScripts } from "../witness-reference-scripts.js";
import type { NetworkIdContracts } from "./contracts.js";

type BaseParams = Parameters<typeof submitCanonicalDecodabilityCancel>[0];

export type SubmitNetworkIdCancelParams = Omit<
  BaseParams,
  "contracts" | "witnessReferenceScripts"
> & {
  readonly contracts: NetworkIdContracts;
  readonly witnessReferenceScripts: FaultProofWitnessReferenceScripts;
};
export type SubmitNetworkIdCancelResult =
  SubmitCanonicalDecodabilityCancelResult;

export const submitNetworkIdCancel = async (
  params: SubmitNetworkIdCancelParams,
): Promise<SubmitNetworkIdCancelResult> => {
  if (params.witnessReferenceScripts == null) {
    throw new Error(
      "network-id cancellation requires published step and computation-thread reference scripts",
    );
  }
  return await submitCanonicalDecodabilityCancel(params);
};
