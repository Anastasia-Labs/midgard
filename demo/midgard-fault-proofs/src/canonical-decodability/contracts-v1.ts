/**
 * `canonical-decodability` deployed-contract record (offchain plan §4.2).
 *
 * The family predates its catalogue registration, like `native-script-decoding`
 * and the two Q39/Q40 families: the `SupportedFaultProofCategoryName` entry,
 * the SDK `contracts.ts` chain builder and the category-id constant are
 * parent-owned surfaces that land with registration (plan §2). Until then,
 * submitters take this explicit already-resolved record instead of going
 * through `resolveFaultProofDeploymentContracts`, and the emulator harness
 * assembles it from the same parameterized chain whose step-01 hash the tests
 * register as an extra catalogue category.
 *
 * Blueprint-declared parameter order per step (the #609 arity guard checks
 * count, not order, so the order is pinned here):
 *
 * - step_01: `[step_02_validator_script_hash, computation_thread_token_policy_id, hub_oracle, field_preimage_certificate_policy_id]`
 * - step_02: `[fraud_proof_token_policy_id, fraud_proof_token_address, computation_thread_token_policy_id]`
 */
import type { Script } from "@lucid-evolution/lucid";

/** Human-readable family label used in every local failure message. */
export const CANONICAL_DECODABILITY_CATEGORY_LABEL = "canonical-decodability";

/** Blueprint titles of the two parameterized step validators. */
export const CANONICAL_DECODABILITY_BLUEPRINT_TITLES_V1 = {
  step01: "fraud_proofs/canonical_decodability/step_01.main.spend",
  step02: "fraud_proofs/canonical_decodability/step_02.main.spend",
} as const;

/** One deployed step of the `canonical-decodability` chain. */
export type CanonicalDecodabilityStepContractV1 = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/**
 * The already-resolved contracts a `canonical-decodability` submission needs.
 *
 * There is deliberately no `categoryId` field yet: the id is assigned at
 * catalogue registration (plan §2; the emulator reserves `00000011`), so
 * callers that need one — thread-token asset names, catalogue lookups — take
 * it separately from the deployment they are actually talking to.
 */
export type CanonicalDecodabilityContractsV1 = {
  /** Steps 01..02, in order. */
  readonly steps: readonly [
    CanonicalDecodabilityStepContractV1,
    CanonicalDecodabilityStepContractV1,
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
   * Policy id the step-01 chain was parameterized with for §8.6 field-preimage
   * certificates (the tier-3 `Certified` field carriage). In the emulator
   * harness this is the always-succeeds stand-in (#579 ruling A); in
   * production it is the real certificate policy.
   */
  readonly fieldPreimageCertificatePolicyId: string;
};
