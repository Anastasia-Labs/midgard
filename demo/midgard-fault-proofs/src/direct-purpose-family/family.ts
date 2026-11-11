/** A direct purpose family: a fault-proof category whose thread binds the
 * leaf's source and walks the shared direct derivation custody. Every direct
 * family names its roles, and the kit plans and submits its custody; a family
 * whose terminal decides conviction from its final state alone also names
 * its verdict, and the kit submits its terminal too. */
import * as SDK from "@al-ft/midgard-sdk";
import type { Script } from "@lucid-evolution/lucid";

/** Every role name a direct walk family can bind. */
export type DirectPurposeRole =
  | SDK.MissingRedeemerRole
  | SDK.MissingScriptSourceRole
  | SDK.UnusedRedeemerRole
  | SDK.ReceivePurposeLanguageRole
  | SDK.ExecutionSourceScriptDecodingRole
  | SDK.DistinctAssetAccumulationLimitRole
  | SDK.ExecutionNativeScriptInvalidRole
  | SDK.ScriptIntegrityHashMismatchRole
  | SDK.UnusedScriptWitnessRole;

/** Resolved deployment of one direct purpose family. */
export type DirectPurposeContracts = {
  /** Every role in the family's role order, step-01 first. */
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

/** The terminal's final-state datum, the off-chain twin of its verdict and
 * the redeemer that spends it. */
export type DirectPurposeVerdict<State> = {
  readonly schema: { fraud_prover: string; data: State | null };
  /** Method syntax: a family of any final state is a family. */
  convicts(state: State): boolean;
  /** Encodes the terminal's spend redeemer from the indices every claim
   * terminal binds; the shared claim finalize when absent. */
  readonly finalize?: (indices: SDK.DirectClaimFinalizeArgs) => string;
};

/** What every direct family shares: its roles and the custody the kit
 * submits through them. */
export type DirectPurposeCustodyFamily = {
  /** Category label, used in every error and transaction label. */
  readonly label: string;
  readonly roleOrder: readonly DirectPurposeRole[];
  /** Named roles; fails closed when the chain is not the family's graph. */
  readonly roles: (
    contracts: DirectPurposeContracts,
  ) => Readonly<Partial<Record<DirectPurposeRole, SDK.SpendingValidator>>>;
  /** The step-01 spend redeemer's schema; the shared direct step-01 when
   * absent. */
  readonly step01Redeemer?: typeof SDK.DirectStep01Redeemer;
};

/** A family whose terminal decides conviction from its final state alone:
 * the shared custody and the verdict the kit's terminal reads. */
export type DirectPurposeFamily<State = SDK.DirectClaimFinalState> =
  DirectPurposeCustodyFamily & {
    readonly verdict: DirectPurposeVerdict<State>;
  };

/** The shared claim terminal's verdict. */
export const DIRECT_CLAIM_VERDICT: DirectPurposeVerdict<SDK.DirectClaimFinalState> =
  {
    schema: SDK.DirectClaimFinalDatum,
    convicts: SDK.directClaimConvicts,
  };

/** The family's validator at `role`; a role outside its graph fails. */
export const directPurposeRoleValidator = (
  family: DirectPurposeCustodyFamily,
  contracts: DirectPurposeContracts,
  role: DirectPurposeRole,
): SDK.SpendingValidator => {
  const validator = family.roles(contracts)[role];
  if (validator === undefined)
    throw new Error(`${family.label}: ${role} is not a role of the family`);
  return validator;
};

/** The step index of `role` in the family's role order. */
export const directPurposeRoleIndex = (
  family: DirectPurposeCustodyFamily,
  role: DirectPurposeRole,
): number => {
  const index = family.roleOrder.indexOf(role);
  if (index < 0)
    throw new Error(`${family.label}: ${role} is not a role of the family`);
  return index;
};
