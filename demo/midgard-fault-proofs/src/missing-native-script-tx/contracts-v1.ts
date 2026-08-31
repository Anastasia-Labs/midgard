/**
 * Explicit, pre-registration contract record for `missing-native-script-tx`.
 *
 * Blueprint parameter order (applied backwards, step 08 first):
 *
 * - step_01: `[step_02_validator_script_hash, computation_thread_token_policy_id, hub_oracle]`
 * - step_02: `[step_03_validator_script_hash, computation_thread_token_policy_id, field_preimage_certificate_policy_id]`
 * - step_03: `[step_04_validator_script_hash, computation_thread_token_policy_id, hub_oracle]`
 * - step_04: `[step_05_validator_script_hash, computation_thread_token_policy_id, field_preimage_certificate_policy_id]`
 * - step_05: `[step_06_validator_script_hash, computation_thread_token_policy_id]`
 * - step_06: `[step_07_validator_script_hash, computation_thread_token_policy_id, fraud_proof_token_policy_id, fraud_proof_token_address, field_preimage_certificate_policy_id]`
 * - step_07: `[step_08_validator_script_hash, computation_thread_token_policy_id, field_preimage_certificate_policy_id]`
 * - step_08: `[computation_thread_token_policy_id, fraud_proof_token_policy_id, fraud_proof_token_address, field_preimage_certificate_policy_id]`
 *
 * There is deliberately no category id here. Registration owns the production
 * id; emulator wiring supplies the reserved test id explicitly.
 */
import type { Script } from "@lucid-evolution/lucid";

import type { FaultProofClaimRegistryContractV1 } from "../claim-registry-transaction-v1.js";

export const MISSING_NATIVE_SCRIPT_TX_CATEGORY_LABEL =
  "missing-native-script-tx";

export const MISSING_NATIVE_SCRIPT_TX_BLUEPRINT_TITLES_V1 = {
  step01: "fraud_proofs/missing_native_script_tx/step_01.main.spend",
  step02: "fraud_proofs/missing_native_script_tx/step_02.main.spend",
  step03: "fraud_proofs/missing_native_script_tx/step_03.main.spend",
  step04: "fraud_proofs/missing_native_script_tx/step_04.main.spend",
  step05: "fraud_proofs/missing_native_script_tx/step_05.main.spend",
  step06: "fraud_proofs/missing_native_script_tx/step_06.main.spend",
  step07: "fraud_proofs/missing_native_script_tx/step_07.main.spend",
  step08: "fraud_proofs/missing_native_script_tx/step_08.main.spend",
} as const;

export type MissingNativeScriptTxStepContractV1 = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

export type MissingNativeScriptTxContractsV1 = {
  readonly steps: readonly [
    MissingNativeScriptTxStepContractV1,
    MissingNativeScriptTxStepContractV1,
    MissingNativeScriptTxStepContractV1,
    MissingNativeScriptTxStepContractV1,
    MissingNativeScriptTxStepContractV1,
    MissingNativeScriptTxStepContractV1,
    MissingNativeScriptTxStepContractV1,
    MissingNativeScriptTxStepContractV1,
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
  /**
   * The applied `claim_registry.spend` validator. Every arm of
   * `computation_thread.mint` requires the claim-registry input in the same
   * transaction, so each submitter resolves its mutation from here.
   */
  readonly claimRegistry: FaultProofClaimRegistryContractV1;
  readonly stateQueuePolicyId: string;
  readonly fieldPreimageCertificatePolicyId: string;
};
