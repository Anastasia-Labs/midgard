/**
 * `native-script-decoding` deployed-contract record (design §4.1/§8.2, Q2/Q3).
 *
 * Like the two Q39/Q40 families, this family predates its catalogue
 * registration: the `SupportedFaultProofCategoryName` entry, the SDK
 * `contracts.ts` chain builder and the category-id constant are parent-owned
 * surfaces that land with registration (design §10 Q2). Until then, submitters
 * take this explicit already-resolved record instead of going through
 * `resolveFaultProofDeploymentContracts`, and the emulator harness assembles it
 * from the same parameterized chain whose step-01 hash the tests register as an
 * extra catalogue category.
 *
 * Blueprint-declared parameter order per step (the #609 arity guard checks
 * count, not order, so the order is pinned here and asserted by the envelope
 * suite via distinct-hash checks):
 *
 * - step_01: `[step_02_validator_script_hash, computation_thread_token_policy_id, hub_oracle]`
 * - step_02: `[step_03_open_subject_validator_script_hash, computation_thread_token_policy_id]`
 * - step_03_open_subject: `[step_03_bind_descriptor_validator_script_hash, step_04_validator_script_hash, computation_thread_token_policy_id, field_preimage_certificate_policy_id]`
 * - step_03_bind_descriptor: `[step_03_advance_or_close_validator_script_hash, step_04_validator_script_hash, computation_thread_token_policy_id]`
 * - step_03_advance_or_close: `[step_04_validator_script_hash, computation_thread_token_policy_id]`
 * - step_04: `[computation_thread_token_policy_id, fraud_proof_token_policy_id, fraud_proof_token_address]`
 *
 * Note the family's own order differs from fabricated-deposit's on both ends:
 * step-02 here has no hub-oracle parameter (the header re-check rides the
 * state-queue reference input instead), step-03 adds the field-preimage
 * certificate policy, and step-04 leads with the computation-thread policy.
 */
import type { Script } from "@lucid-evolution/lucid";

/** Human-readable family label used in every local failure message. */
export const NATIVE_SCRIPT_DECODING_CATEGORY_LABEL = "native-script-decoding";

/** Blueprint titles of the six parameterized step validators. */
export const NATIVE_SCRIPT_DECODING_BLUEPRINT_TITLES_V1 = {
  step01: "fraud_proofs/native_script_decoding/step_01.main.spend",
  step02: "fraud_proofs/native_script_decoding/step_02.main.spend",
  step03OpenSubject:
    "fraud_proofs/native_script_decoding/step_03_open_subject.main.spend",
  step03BindDescriptor:
    "fraud_proofs/native_script_decoding/step_03_bind_descriptor.main.spend",
  step03AdvanceOrClose:
    "fraud_proofs/native_script_decoding/step_03_advance_or_close.main.spend",
  step04: "fraud_proofs/native_script_decoding/step_04.main.spend",
} as const;

/** One deployed step of the `native-script-decoding` chain. */
export type NativeScriptDecodingStepContractV1 = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/**
 * The already-resolved contracts a `native-script-decoding` submission needs.
 *
 * There is deliberately no `categoryId` field yet: the id is assigned at
 * catalogue registration (design §10 Q2; `0000000d` expected but not promised),
 * so callers that need one — thread-token asset names, catalogue lookups — take
 * it separately from the deployment they are actually talking to.
 */
export type NativeScriptDecodingContractsV1 = {
  /** Steps 01, 02, the three step-03 validators, and 04, in order. */
  readonly steps: readonly [
    NativeScriptDecodingStepContractV1,
    NativeScriptDecodingStepContractV1,
    NativeScriptDecodingStepContractV1,
    NativeScriptDecodingStepContractV1,
    NativeScriptDecodingStepContractV1,
    NativeScriptDecodingStepContractV1,
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
  /**
   * Policy id OpenSubject was parameterized with for §8.6 field-preimage
   * certificates. In the emulator harness this is the always-succeeds stand-in
   * (#579 ruling A); in production it is the real certificate policy.
   */
  readonly fieldPreimageCertificatePolicyId: string;
};
