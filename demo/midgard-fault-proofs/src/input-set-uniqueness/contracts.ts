/**
 * `input-set-uniqueness` deployed-contract record.
 *
 * Like native-script-decoding, this family predates its catalogue
 * registration: the `SupportedFaultProofCategoryName` entry, the SDK
 * `contracts.ts` chain builder and the category-id constant are parent-owned
 * surfaces that land with registration. Until then, submitters take this
 * explicit already-resolved record instead of going through
 * `resolveFaultProofDeploymentContracts`, and the emulator harness assembles it
 * from the same parameterized chain whose step-01 hash the tests register as an
 * extra catalogue category.
 *
 * Blueprint-declared parameter order per step (the #609 arity guard checks
 * count, not order, so the order is pinned here):
 *
 * - step_01: `[step_02_validator_script_hash, step_03_validator_script_hash, computation_thread_token_policy_id, hub_oracle]`
 * - step_02: `[fraud_proof_token_policy_id, fraud_proof_token_address, computation_thread_token_policy_id, field_preimage_certificate_policy_id]`
 * - step_03: `[step_04_hash, computation_thread_token_policy_id, field_preimage_certificate_policy_id]`
 * - step_04: `[fraud_proof_token_policy_id, fraud_proof_token_address, computation_thread_token_policy_id, field_preimage_certificate_policy_id]`
 *
 * Step-02 both concludes (opens fields 0/1 through the §8.8 door) and
 * finalizes (burns the thread, mints the permanent fraud-proof token), so the
 * chain is two steps: shorter than input-no-idx's four because the conviction
 * predicate is byte equality of two door-authenticated items, with no
 * per-field commitment reproduction to spread across steps.
 */
import type { Script } from "@lucid-evolution/lucid";

/** Human-readable family label used in every local failure message. */
export const INPUT_SET_UNIQUENESS_CATEGORY_LABEL = "input-set-uniqueness";

/** Blueprint titles of the four parameterized step validators. */
export const INPUT_SET_UNIQUENESS_BLUEPRINT_TITLES = {
  step01: "fraud_proofs/input_set_uniqueness/step_01.main.spend",
  step02: "fraud_proofs/input_set_uniqueness/step_02.main.spend",
  step03: "fraud_proofs/input_set_uniqueness/step_03.main.spend",
  step04: "fraud_proofs/input_set_uniqueness/step_04.main.spend",
} as const;

/** One deployed step of the `input-set-uniqueness` chain. */
export type InputSetUniquenessStepContract = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/**
 * The already-resolved contracts an `input-set-uniqueness` submission needs.
 *
 * There is deliberately no `categoryId` field yet: the id is assigned at
 * catalogue registration (`0000001a` reserved but not promised), so callers
 * that need one — thread-token asset names, catalogue lookups — take it
 * separately from the deployment they are actually talking to.
 */
export type InputSetUniquenessContracts = {
  /** Steps 01..04, in order. Accepted proofs use 01→02; forced use 01→03→04*. */
  readonly steps: readonly [
    InputSetUniquenessStepContract,
    InputSetUniquenessStepContract,
    InputSetUniquenessStepContract,
    InputSetUniquenessStepContract,
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
   * Policy id the step-02 chain was parameterized with for §8.6 field-preimage
   * certificates. In the emulator harness this is the always-succeeds stand-in
   * (#579 ruling A); in production it is the real certificate policy. The
   * family's openings are tiny (§5.3 fixed-stride out-ref lists), so tier-1
   * inline carriage is always used and the certificate door stays closed.
   */
  readonly fieldPreimageCertificatePolicyId: string;
};
