import type { Script } from "@lucid-evolution/lucid";

export const MIN_ADA_CATEGORY_LABEL = "min-ada";

export type MinAdaStepContract = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

export type MinAdaContracts = {
  readonly steps: readonly [
    MinAdaStepContract,
    MinAdaStepContract,
    MinAdaStepContract,
    MinAdaStepContract,
  ];
  readonly yields: {
    readonly tx: {
      readonly withdrawalScriptCBOR: string;
      readonly withdrawalScript: Script;
      readonly withdrawalScriptHash: string;
    };
  };
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
  readonly referenceScriptAuthPolicyId: string;
};
