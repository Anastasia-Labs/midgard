import type { Script } from "@lucid-evolution/lucid";

export const NATIVE_SCRIPT_INVALID_CATEGORY_LABEL = "native-script-invalid";

export type NativeScriptInvalidStepContract = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

export type NativeScriptInvalidContracts = {
  readonly steps: readonly [
    NativeScriptInvalidStepContract,
    NativeScriptInvalidStepContract,
    NativeScriptInvalidStepContract,
    NativeScriptInvalidStepContract,
    NativeScriptInvalidStepContract,
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
