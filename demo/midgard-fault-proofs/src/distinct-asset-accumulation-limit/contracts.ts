/** Resolved `distinctAssetAccumulationLimit` deployment the submitters and
 * prover read. */
import * as SDK from "@al-ft/midgard-sdk";
import type { Script } from "@lucid-evolution/lucid";

export const DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_LABEL =
  "distinct-asset-accumulation-limit";

export type DistinctAssetAccumulationLimitContracts = {
  /** Every role in `SDK.DISTINCT_ASSET_ACCUMULATION_LIMIT_ROLE_ORDER`, step-01
   * first. */
  readonly steps: readonly [SDK.SpendingValidator, ...SDK.SpendingValidator[]];
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

/** Named roles; fails closed when the chain is not the 12-role graph. */
export const distinctAssetAccumulationLimitContractRoles = (
  contracts: DistinctAssetAccumulationLimitContracts,
): SDK.DistinctAssetAccumulationLimitRoles =>
  SDK.distinctAssetAccumulationLimitRoles({
    firstStep: contracts.steps[0],
    steps: contracts.steps,
  });

export type DistinctAssetAccumulationLimitRole =
  SDK.DistinctAssetAccumulationLimitRole;
