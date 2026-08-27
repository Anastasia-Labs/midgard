/**
 * `missing-signature` deployed-contract record (offchain plan §4.2, D1/D2).
 *
 * Like the decoding family, this family predates its catalogue registration:
 * the `SupportedFaultProofCategoryName` entry, the SDK `contracts.ts` chain
 * builder and the category-id constant are parent-owned surfaces that land
 * with registration (plan §2.2, D1). Until then, submitters take this
 * explicit already-resolved record instead of going through
 * `resolveFaultProofDeploymentContracts`, and the emulator harness assembles
 * it from the same parameterized chain whose step-01 hash the tests register
 * as an extra catalogue category.
 *
 * Blueprint-declared parameter order per step (the #609 arity guard checks
 * count, not order, so the order is pinned here — plan §1's table):
 *
 * - step_01: `[step_02_validator_script_hash, computation_thread_token_policy_id, hub_oracle]`
 * - step_02: `[step_03_validator_script_hash, computation_thread_token_policy_id, field_preimage_certificate_policy_id]`
 * - step_03: `[step_04_validator_script_hash, computation_thread_token_policy_id]`
 * - step_04: `[fraud_proof_token_policy_id, fraud_proof_token_address, computation_thread_token_policy_id, field_preimage_certificate_policy_id]`
 *
 * Note against the decoding family's table: **two** steps here take the
 * field-preimage certificate policy (02 and 04 — each opens a different
 * field through the §8.8 door), and step-04 leads with the fraud-proof pair.
 */
import type { Script } from "@lucid-evolution/lucid";

/** Human-readable family label used in every local failure message. */
export const MISSING_SIGNATURE_CATEGORY_LABEL = "missing-signature";

/** Blueprint titles of the four parameterized step validators. */
export const MISSING_SIGNATURE_BLUEPRINT_TITLES_V1 = {
  step01: "fraud_proofs/missing_signature/step_01.main.spend",
  step02: "fraud_proofs/missing_signature/step_02.main.spend",
  step03: "fraud_proofs/missing_signature/step_03.main.spend",
  step04: "fraud_proofs/missing_signature/step_04.main.spend",
} as const;

/** One deployed step of the `missing-signature` chain. */
export type MissingSignatureStepContractV1 = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/**
 * The already-resolved contracts a `missing-signature` submission needs.
 *
 * There is deliberately no `categoryId` field yet: the id is assigned at
 * catalogue registration (plan D1; `0000000e` expected but not promised), so
 * callers that need one — thread-token asset names, catalogue lookups — take
 * it separately from the deployment they are actually talking to.
 */
export type MissingSignatureContractsV1 = {
  /** Steps 01..04, in order. */
  readonly steps: readonly [
    MissingSignatureStepContractV1,
    MissingSignatureStepContractV1,
    MissingSignatureStepContractV1,
    MissingSignatureStepContractV1,
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
   * Policy id the step-02/step-04 chain was parameterized with for §8.6
   * field-preimage certificates. In the emulator harness this is the
   * always-succeeds stand-in (#579 ruling A); in production it is the real
   * certificate policy.
   */
  readonly fieldPreimageCertificatePolicyId: string;
};
