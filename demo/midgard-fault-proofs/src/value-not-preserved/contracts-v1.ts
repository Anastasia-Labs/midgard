/**
 * `value-not-preserved` deployed-contract record (offchain plan §2/§4).
 *
 * The family predates its catalogue registration, exactly like
 * `native-script-decoding`: the `SupportedFaultProofCategoryName` entry, the
 * SDK chain builder and the category-id constant are parent-owned surfaces
 * that land at registration. Until then, submitters take this explicit
 * already-resolved record instead of going through
 * `resolveFaultProofDeploymentContracts`, and the emulator harness assembles
 * it from the same parameterized chain whose step-01 hash the tests register
 * as an extra catalogue category.
 *
 * Blueprint-declared parameter order per step (pinned here; the arity guard
 * checks count, not order):
 *
 * - step_01: `[step_02_validator_script_hash, computation_thread_token_policy_id, hub_oracle]`
 * - step_02: `[step_03_validator_script_hash, computation_thread_token_policy_id, field_preimage_certificate_policy_id]`
 * - step_03: `[step_04_validator_script_hash, computation_thread_token_policy_id, field_preimage_certificate_policy_id]`
 * - step_04: `[fraud_proof_token_policy_id, fraud_proof_token_address, computation_thread_token_policy_id]`
 *
 * Note the family's own order differs from native-script-decoding's on both
 * ends: steps 02 and 03 both carry the §8.6 field-preimage certificate policy
 * (each opens a committed field through the §8.8 door), and step-04 leads
 * with the fraud-proof pair.
 */
import type { Script } from "@lucid-evolution/lucid";

import type { FaultProofClaimRegistryContractV1 } from "../claim-registry-transaction-v1.js";

/** Human-readable family label used in every local failure message. */
export const VALUE_NOT_PRESERVED_CATEGORY_LABEL = "value-not-preserved";

/** Blueprint titles of the four parameterized step validators. */
export const VALUE_NOT_PRESERVED_BLUEPRINT_TITLES_V1 = {
  step01: "fraud_proofs/value_not_preserved/step_01.main.spend",
  step02: "fraud_proofs/value_not_preserved/step_02.main.spend",
  step03: "fraud_proofs/value_not_preserved/step_03.main.spend",
  step04: "fraud_proofs/value_not_preserved/step_04.main.spend",
} as const;

/** One deployed step of the `value-not-preserved` chain. */
export type ValueNotPreservedStepContractV1 = {
  readonly spendingScript: Script;
  readonly spendingScriptHash: string;
  readonly spendingScriptAddress: string;
};

/**
 * The already-resolved contracts a `value-not-preserved` submission needs.
 *
 * There is deliberately no `categoryId` field: the id is assigned at
 * catalogue registration (`00000019` reserved but not promised), so callers
 * that need one — thread-token asset names, catalogue lookups — take it
 * separately from the deployment they are actually talking to.
 */
export type ValueNotPreservedContractsV1 = {
  /** Steps 01..04, in order. */
  readonly steps: readonly [
    ValueNotPreservedStepContractV1,
    ValueNotPreservedStepContractV1,
    ValueNotPreservedStepContractV1,
    ValueNotPreservedStepContractV1,
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
  /**
   * Policy id steps 02/03 were parameterized with for §8.6 field-preimage
   * certificates. In the emulator harness this is the always-succeeds
   * stand-in; in production it is the real certificate policy. The step-03
   * submitter selects §8.4 tiers purely from preimage size, so a field over
   * the tier-2 window would name its §8.6 manifest by this policy id.
   */
  readonly fieldPreimageCertificatePolicyId: string;
};
