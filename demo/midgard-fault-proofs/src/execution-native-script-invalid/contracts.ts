/** Resolved `executionNativeScriptInvalid` deployment the submitters and prover
 * read. */
import * as SDK from "@al-ft/midgard-sdk";
import type { Script } from "@lucid-evolution/lucid";

export const EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL =
  "execution-native-script-invalid";

export type ExecutionNativeScriptInvalidContracts = {
  /** Every role in `SDK.EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_ORDER`, step-01 first. */
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

/** Named roles; fails closed when the chain is not the 28-role graph. */
export const executionNativeScriptInvalidContractRoles = (
  contracts: ExecutionNativeScriptInvalidContracts,
): SDK.ExecutionNativeScriptInvalidRoles =>
  SDK.executionNativeScriptInvalidRoles({
    firstStep: contracts.steps[0],
    steps: contracts.steps,
  });

export type ExecutionNativeScriptInvalidRole =
  SDK.ExecutionNativeScriptInvalidRole;
