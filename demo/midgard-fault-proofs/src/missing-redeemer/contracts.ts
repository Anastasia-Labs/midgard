/** Resolved `missingRedeemer` deployment the submitters and prover read. */
import * as SDK from "@al-ft/midgard-sdk";
import type { Script } from "@lucid-evolution/lucid";

export const MISSING_REDEEMER_CATEGORY_LABEL = "missing-redeemer";

export type MissingRedeemerContracts = {
  /** Every role in `SDK.MISSING_REDEEMER_ROLE_ORDER`, step-01 first. */
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

/** Named roles; fails closed when the chain is not the 37-role graph. */
export const missingRedeemerContractRoles = (
  contracts: MissingRedeemerContracts,
): SDK.MissingRedeemerRoles =>
  SDK.missingRedeemerRoles({
    firstStep: contracts.steps[0],
    steps: contracts.steps,
  });

export type MissingRedeemerRole = SDK.MissingRedeemerRole;
