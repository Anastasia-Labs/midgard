/**
 * `mint-authorization` deployed-contract record.
 *
 * Like the `native-script-decoding` family, this family predates its
 * catalogue registration: the `SupportedFaultProofCategoryName` entry, the
 * SDK `contracts.ts` chain builder and the category-id constant are
 * parent-owned surfaces that land with registration. Until then, submitters
 * take this explicit already-resolved record instead of going through
 * `resolveFaultProofDeploymentContracts`, and the emulator harness assembles
 * it from the same parameterized chain whose step-01 hash the tests register
 * as an extra catalogue category (the reserved test id is `0000001b`).
 *
 * Blueprint-declared parameter order per step (the #609 arity guard checks
 * count, not order, so the order is pinned here):
 *
 * - step_01: `[step_02_validator_script_hash, computation_thread_token_policy_id, hub_oracle]`
 * - step_02: `[step_03_validator_script_hash, computation_thread_token_policy_id, field_preimage_certificate_policy_id]`
 * - step_03: `[step_04_validator_script_hash, step_05_validator_script_hash, computation_thread_token_policy_id, field_preimage_certificate_policy_id]`
 * - step_04: `[step_05_validator_script_hash, computation_thread_token_policy_id, field_preimage_certificate_policy_id]`
 * - step_05: `[fraud_proof_token_policy_id, fraud_proof_token_address, computation_thread_token_policy_id]`
 *
 * Step-03 is the only step in the deployed families so far that takes TWO
 * downstream step hashes: its direction-A arm continues into step-04's
 * reference-input scan while its direction-B arm closes straight to step-05.
 */
import type { Script } from "@lucid-evolution/lucid";

/** Human-readable family label used in every local failure message. */
export const MINT_AUTHORIZATION_CATEGORY_LABEL = "mint-authorization";

/** Blueprint titles of the five parameterized step validators. */
export const MINT_AUTHORIZATION_BLUEPRINT_TITLES_V1 = {
  step01: "fraud_proofs/mint_authorization/step_01.main.spend",
  step02: "fraud_proofs/mint_authorization/step_02.main.spend",
  step03: "fraud_proofs/mint_authorization/step_03.main.spend",
  step04: "fraud_proofs/mint_authorization/step_04.main.spend",
  step05: "fraud_proofs/mint_authorization/step_05.main.spend",
} as const;

/** One deployed step of the `mint-authorization` chain. */
export type MintAuthorizationStepContractV1 = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/**
 * The already-resolved contracts a `mint-authorization` submission needs.
 *
 * There is deliberately no `categoryId` field: the id is assigned at
 * catalogue registration, so callers that need one — thread-token asset
 * names, catalogue lookups — take it separately from the deployment they are
 * actually talking to.
 */
export type MintAuthorizationContractsV1 = {
  /** Steps 01..05, in order. */
  readonly steps: readonly [
    MintAuthorizationStepContractV1,
    MintAuthorizationStepContractV1,
    MintAuthorizationStepContractV1,
    MintAuthorizationStepContractV1,
    MintAuthorizationStepContractV1,
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
   * Policy id steps 02–04 were parameterized with for §8.6 field-preimage
   * certificates. In the emulator harness this is the always-succeeds
   * stand-in (#579 ruling A); in production it is the real certificate
   * policy.
   */
  readonly fieldPreimageCertificatePolicyId: string;
};
