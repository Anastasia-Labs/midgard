import type { Script } from "@lucid-evolution/lucid";

export const WITHDRAWN_REFERENCE_INPUT_CATEGORY_LABEL =
  "withdrawn-reference-input";

/** Blueprint titles for the three deployed validators. */
export const WITHDRAWN_REFERENCE_INPUT_BLUEPRINT_TITLES = {
  step01: "fraud_proofs/withdrawn_reference_input/step_01.main.spend",
  step02: "fraud_proofs/withdrawn_reference_input/step_02.main.spend",
  step03: "fraud_proofs/withdrawn_reference_input/step_03.main.spend",
} as const;

export type WithdrawnReferenceInputStepContract = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/**
 * Explicit pre-registration deployment record.
 *
 * Blueprint parameter order is fixed as follows:
 *
 * - step 01: `[step_02_hash, computation_thread_policy_id, hub_oracle]`
 * - step 02: `[step_03_hash, computation_thread_policy_id,
 *   field_preimage_certificate_policy_id]`
 * - step 03: `[fraud_proof_policy_id, fraud_proof_token_address,
 *   computation_thread_policy_id]`
 *
 * There is deliberately no category id: catalogue registration owns it.
 */
export type WithdrawnReferenceInputContracts = {
  readonly steps: readonly [
    WithdrawnReferenceInputStepContract,
    WithdrawnReferenceInputStepContract,
    WithdrawnReferenceInputStepContract,
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
