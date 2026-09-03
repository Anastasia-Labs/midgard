import type { Script } from "@lucid-evolution/lucid";

export const MISSING_NATIVE_SCRIPT_UTXO_CATEGORY_LABEL =
  "missing-native-script-utxo";

export const MISSING_NATIVE_SCRIPT_UTXO_BLUEPRINT_TITLES = {
  step01: "fraud_proofs/missing_native_script_utxo/step_01.main.spend",
  step02: "fraud_proofs/missing_native_script_utxo/step_02.main.spend",
  step03: "fraud_proofs/missing_native_script_utxo/step_03.main.spend",
  step04: "fraud_proofs/missing_native_script_utxo/step_04.main.spend",
  step05: "fraud_proofs/missing_native_script_utxo/step_05.main.spend",
  step06: "fraud_proofs/missing_native_script_utxo/step_06.main.spend",
  step07: "fraud_proofs/missing_native_script_utxo/step_07.main.spend",
} as const;

export type MissingNativeScriptUtxoStepContract = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

export type MissingNativeScriptUtxoContracts = {
  readonly steps: readonly [
    MissingNativeScriptUtxoStepContract,
    MissingNativeScriptUtxoStepContract,
    MissingNativeScriptUtxoStepContract,
    MissingNativeScriptUtxoStepContract,
    MissingNativeScriptUtxoStepContract,
    MissingNativeScriptUtxoStepContract,
    MissingNativeScriptUtxoStepContract,
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
