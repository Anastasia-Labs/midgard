/** Resolved `unusedScriptWitness` deployment the submitters and prover
 * read. */
import * as SDK from "@al-ft/midgard-sdk";
import type { Script } from "@lucid-evolution/lucid";

export const UNUSED_SCRIPT_WITNESS_CATEGORY_LABEL = "unused-script-witness";

export type UnusedScriptWitnessContracts = {
  /** Every role in `SDK.UNUSED_SCRIPT_WITNESS_ROLE_ORDER`, step-01 first. */
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

/** Named roles; fails closed when the chain is not the 24-role graph. */
export const unusedScriptWitnessContractRoles = (
  contracts: UnusedScriptWitnessContracts,
): SDK.UnusedScriptWitnessRoles =>
  SDK.unusedScriptWitnessRoles({
    firstStep: contracts.steps[0],
    steps: contracts.steps,
  });

export type UnusedScriptWitnessRole = SDK.UnusedScriptWitnessRole;
