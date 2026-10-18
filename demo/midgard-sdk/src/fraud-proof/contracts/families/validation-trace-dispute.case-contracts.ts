import { type OutRefLike } from "@al-ft/midgard-core/out-ref";
import { MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS } from "@al-ft/midgard-core/validation-dispute";
import { type Network } from "@lucid-evolution/lucid";

import {
  type AuthenticatedValidator,
  type MintingValidator,
  type WithdrawalValidator,
} from "../../../common.js";
import {
  applyBlueprintParams,
  type FaultProofBlueprint,
  makeAuthenticatedValidator,
  makeMintingPolicy,
} from "../blueprint.js";
import {
  buildValidationThreadExpiry,
  outRefData,
} from "./validation-trace-dispute.thread-expiry.js";
import { VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES } from "./validation-trace-dispute.validation-trace-dispute-fault-proof-titles.js";

export type ValidationCaseContracts = {
  readonly caseConfig: MintingValidator;
  /** Its minting policy is the thread token policy from Defend on. */
  readonly case: AuthenticatedValidator;
  readonly threadExpiry: WithdrawalValidator;
  /** Its minting policy is the lost-defense receipt policy. */
  readonly award: AuthenticatedValidator;
};

/**
 * The case and the scripts its identity fixes. The configuration policy is a
 * one-shot on the deployment nonce; the case reads its other hand-off hashes
 * from the configuration datum, so none of them parameterises it. The thread
 * expiry and the award (whose mint is the lost-defense receipt policy the
 * operator directories take) depend only on the case policy and the hub
 * oracle, so a deployment can derive the receipt policy before it builds the
 * rest of the validation-trace chain.
 */
export const buildValidationCaseContracts = ({
  blueprint,
  network,
  hubOraclePolicyId,
  computationThreadPolicyId,
  disputeBondLovelace,
  caseConfigInitializationNonce,
}: {
  readonly blueprint: FaultProofBlueprint;
  readonly network: Network;
  readonly hubOraclePolicyId: string;
  readonly computationThreadPolicyId: string;
  readonly disputeBondLovelace: bigint;
  readonly caseConfigInitializationNonce: OutRefLike;
}): ValidationCaseContracts => {
  const titles = VALIDATION_TRACE_DISPUTE_FAULT_PROOF_TITLES;
  const caseConfig = makeMintingPolicy(
    applyBlueprintParams(blueprint, titles.caseConfig, [
      outRefData(caseConfigInitializationNonce),
    ]),
  );
  const caseParams = [
    hubOraclePolicyId,
    caseConfig.policyId,
    computationThreadPolicyId,
    disputeBondLovelace,
  ];
  const caseValidator = makeAuthenticatedValidator(
    network,
    applyBlueprintParams(blueprint, titles.caseMint, caseParams),
    applyBlueprintParams(blueprint, titles.caseSpend, caseParams),
  );
  const threadExpiry = buildValidationThreadExpiry(
    blueprint,
    caseValidator.policyId,
    MIDGARD_VALIDATION_DISPUTE_MAX_DURATION_MS,
  );
  const awardParams = [
    caseValidator.policyId,
    hubOraclePolicyId,
    threadExpiry.withdrawalScriptHash,
  ];
  const award = makeAuthenticatedValidator(
    network,
    applyBlueprintParams(blueprint, titles.awardMint, awardParams),
    applyBlueprintParams(blueprint, titles.award, awardParams),
  );
  return { caseConfig, case: caseValidator, threadExpiry, award };
};
