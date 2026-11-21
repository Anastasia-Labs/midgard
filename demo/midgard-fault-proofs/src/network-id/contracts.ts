import type { MintingPolicy, Script } from "@lucid-evolution/lucid";

export const NETWORK_ID_CATEGORY_LABEL = "network-id";

export type NetworkIdStepContract = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/**
 * Family-local deployment shape. Both steps are reference-script-only in
 * production; transaction builders must supply the complete L1 reference
 * input set before resolving any positional field-opening indices.
 */
export type NetworkIdContracts = {
  readonly steps: readonly [NetworkIdStepContract, NetworkIdStepContract];
  readonly forcedStep?: NetworkIdStepContract;
  /**
   * Resumable forced outputs scan. Present on exactly the deployments that
   * carry the forced door: the door writes this validator's `Ready` state and
   * only this validator writes step 02's `ForcedNetworkIdMismatch` state.
   */
  readonly forcedScan?: NetworkIdStepContract;
  readonly expectedNetworkId: 0n | 1n;
  readonly computationThread: {
    readonly policyId: string;
    readonly mintingScript: Script;
  };
  readonly fraudProof: {
    readonly policyId: string;
    readonly mintingScript: Script;
    readonly spendingScriptAddress: string;
  };
  readonly hubOraclePolicyId: string;
  readonly stateQueuePolicyId: string;
  readonly fieldPreimageCertificatePolicyId: string;
  readonly fieldPreimageCertificateMintingScript: MintingPolicy;
};
