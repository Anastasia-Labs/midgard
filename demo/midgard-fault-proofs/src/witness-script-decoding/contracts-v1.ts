import type { Script } from "@lucid-evolution/lucid";

export const WITNESS_SCRIPT_DECODING_CATEGORY_LABEL = "witness-script-decoding";

export const WITNESS_SCRIPT_DECODING_BLUEPRINT_TITLES_V1 = {
  step01: "fraud_proofs/witness_script_decoding/step_01.main.spend",
  step02: "fraud_proofs/witness_script_decoding/step_02.main.spend",
  step03: "fraud_proofs/witness_script_decoding/step_03.main.spend",
  step04: "fraud_proofs/witness_script_decoding/step_04.main.spend",
} as const;

export type WitnessScriptDecodingStepContractV1 = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/** Manifest-bound contracts consumed by every concrete family submitter. */
export type WitnessScriptDecodingContractsV1 = {
  readonly steps: readonly [
    WitnessScriptDecodingStepContractV1,
    WitnessScriptDecodingStepContractV1,
    WitnessScriptDecodingStepContractV1,
    WitnessScriptDecodingStepContractV1,
  ];
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
  readonly fieldPreimageCertificateMintingScript: Script;
};
