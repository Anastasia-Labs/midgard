import type { Script } from "@lucid-evolution/lucid";

export const MIN_ADA_CATEGORY_LABEL = "min-ada";

export const MIN_ADA_BLUEPRINT_TITLES_V1 = {
  step01: "fraud_proofs/min_ada/step_01.main.spend",
  step02: "fraud_proofs/min_ada/step_02.main.spend",
  step03: "fraud_proofs/min_ada/step_03.main.spend",
  step04: "fraud_proofs/min_ada/step_04.main.spend",
  step05: "fraud_proofs/min_ada/step_05.main.spend",
} as const;

export type MinAdaStepContractV1 = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

export type MinAdaContractsV1 = {
  readonly steps: readonly [
    MinAdaStepContractV1,
    MinAdaStepContractV1,
    MinAdaStepContractV1,
    MinAdaStepContractV1,
    MinAdaStepContractV1,
  ];
  readonly yields: {
    readonly tx: {
      readonly withdrawalScriptCBOR: string;
      readonly withdrawalScript: Script;
      readonly withdrawalScriptHash: string;
    };
    readonly utxo: {
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
