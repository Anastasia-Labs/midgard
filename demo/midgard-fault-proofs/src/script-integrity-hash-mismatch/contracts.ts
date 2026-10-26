/** Resolved `scriptIntegrityHashMismatch` deployment the submitters and
 * prover read. */
import * as SDK from "@al-ft/midgard-sdk";
import type { Script } from "@lucid-evolution/lucid";

export const SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_LABEL =
  "script-integrity-hash-mismatch";

export type ScriptIntegrityHashMismatchContracts = {
  /** Every role in `SDK.SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_ORDER`, step-01
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

/** Named roles; fails closed when the chain is not the 21-role graph. */
export const scriptIntegrityHashMismatchContractRoles = (
  contracts: ScriptIntegrityHashMismatchContracts,
): SDK.ScriptIntegrityHashMismatchRoles =>
  SDK.scriptIntegrityHashMismatchRoles({
    firstStep: contracts.steps[0],
    steps: contracts.steps,
  });

export type ScriptIntegrityHashMismatchRole =
  SDK.ScriptIntegrityHashMismatchRole;
