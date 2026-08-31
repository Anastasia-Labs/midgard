import type { Script } from "@lucid-evolution/lucid";

export const NATIVE_SCRIPT_INVALID_CATEGORY_LABEL = "native-script-invalid";

export const NATIVE_SCRIPT_INVALID_BLUEPRINT_TITLES_V1 = {
  step01: "fraud_proofs/native_script_invalid/step_01.main.spend",
  step02: "fraud_proofs/native_script_invalid/step_02.main.spend",
  step03: "fraud_proofs/native_script_invalid/step_03.main.spend",
  step04: "fraud_proofs/native_script_invalid/step_04.main.spend",
  step05: "fraud_proofs/native_script_invalid/step_05.main.spend",
} as const;

export type NativeScriptInvalidStepContractV1 = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

export type NativeScriptInvalidContractsV1 = {
  readonly steps: readonly [
    NativeScriptInvalidStepContractV1,
    NativeScriptInvalidStepContractV1,
    NativeScriptInvalidStepContractV1,
    NativeScriptInvalidStepContractV1,
    NativeScriptInvalidStepContractV1,
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
};
