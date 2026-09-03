/** Family-local aliases over the invariant two-step thread plumbing. */
import type { UTxO } from "@lucid-evolution/lucid";

import {
  requireCanonicalDecodabilityReferenceScript,
  requireCanonicalDecodabilityStepState,
  requireCanonicalDecodabilityThreadUtxo,
} from "../canonical-decodability/submit-common-v1.js";
import type { ResolvedProverSigner } from "../runtime.js";
import type { NetworkIdContracts } from "./contracts-v1.js";

export type NetworkIdCatalogueCategory = {
  readonly categoryId: string;
  readonly scriptHash: string;
  readonly membershipProofCbor: string;
};

export const networkIdSubmitError = (message: string): Error =>
  new Error(`network-id: ${message}`);

export const networkIdStepLabel = (stepIndex: 0 | 1): string =>
  `network-id step 0${(stepIndex + 1).toString()}`;

export const requireNetworkIdThreadUtxo = async (params: {
  readonly lucid: Parameters<
    typeof requireCanonicalDecodabilityThreadUtxo
  >[0]["lucid"];
  readonly contracts: NetworkIdContracts;
  readonly categoryId: string;
  readonly stepIndex: 0 | 1;
  readonly threadOutRef: string;
}) => await requireCanonicalDecodabilityThreadUtxo(params);

export const requireNetworkIdReferenceScript = (params: {
  readonly utxo: UTxO;
  readonly expectedScriptHash: string;
  readonly stepIndex: 0 | 1;
}): UTxO => requireCanonicalDecodabilityReferenceScript(params);

export const requireNetworkIdStepState = <State>(params: {
  readonly threadUtxo: UTxO;
  readonly signer: ResolvedProverSigner;
  readonly schema: { fraud_prover: string; data: State | null };
  readonly stepIndex: 0 | 1;
}): State => requireCanonicalDecodabilityStepState(params);
