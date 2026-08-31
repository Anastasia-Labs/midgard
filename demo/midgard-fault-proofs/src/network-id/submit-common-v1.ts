/** Family-local aliases over the invariant two-step thread plumbing. */
import type { UTxO } from "@lucid-evolution/lucid";

import {
  requireCanonicalDecodabilityReferenceScriptV1,
  requireCanonicalDecodabilityStepStateV1,
  requireCanonicalDecodabilityThreadUtxoV1,
} from "../canonical-decodability/submit-common-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { NetworkIdContractsV1 } from "./contracts-v1.js";

export type NetworkIdCatalogueCategoryV1 = {
  readonly categoryId: string;
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};

export const networkIdSubmitError = (message: string): Error =>
  new Error(`network-id: ${message}`);

export const networkIdStepLabelV1 = (stepIndex: 0 | 1): string =>
  `network-id step 0${(stepIndex + 1).toString()}`;

export const requireNetworkIdThreadUtxoV1 = async (params: {
  readonly lucid: Parameters<
    typeof requireCanonicalDecodabilityThreadUtxoV1
  >[0]["lucid"];
  readonly contracts: NetworkIdContractsV1;
  readonly categoryId: string;
  readonly stepIndex: 0 | 1;
  readonly threadOutRef: string;
}) => await requireCanonicalDecodabilityThreadUtxoV1(params);

export const requireNetworkIdReferenceScriptV1 = (params: {
  readonly utxo: UTxO;
  readonly expectedScriptHash: string;
  readonly stepIndex: 0 | 1;
}): UTxO => requireCanonicalDecodabilityReferenceScriptV1(params);

export const requireNetworkIdStepStateV1 = <State>(params: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
  readonly schema: { fraud_prover: string; data: State | null };
  readonly stepIndex: 0 | 1;
}): State => requireCanonicalDecodabilityStepStateV1(params);
