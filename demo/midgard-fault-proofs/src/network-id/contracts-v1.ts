import type { MintingPolicy, Script } from "@lucid-evolution/lucid";

export const NETWORK_ID_CATEGORY_LABEL = "network-id";

/** Human-readable family label; the canonical SDK catalogue key is networkId. */
export const NETWORK_ID_BLUEPRINT_TITLES_V1 = {
  step01: "fraud_proofs/network_id/step_01.main.spend",
  step02: "fraud_proofs/network_id/step_02.main.spend",
} as const;

export type NetworkIdStepContractV1 = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/**
 * Family-local deployment shape. Both steps are reference-script-only in
 * production; transaction builders must supply the complete L1 reference
 * input set before resolving any positional field-opening indices.
 */
export type NetworkIdContractsV1 = {
  readonly steps: readonly [NetworkIdStepContractV1, NetworkIdStepContractV1];
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
