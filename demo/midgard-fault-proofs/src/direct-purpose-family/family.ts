/** A direct purpose family: a fault-proof category whose thread binds the
 * leaf's source, walks the shared direct derivation custody and ends at a
 * terminal that decides conviction from its final state alone. The family
 * names its roles and its verdict; the kit plans and submits the custody. */
import * as SDK from "@al-ft/midgard-sdk";
import type { Script } from "@lucid-evolution/lucid";

/** Every role name a direct purpose family can bind. */
export type DirectPurposeRole =
  | SDK.MissingRedeemerRole
  | SDK.MissingScriptSourceRole;

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

/** The terminal's final-state datum and the off-chain twin of its verdict. */
export type DirectPurposeVerdict<State> = {
  readonly schema: { fraud_prover: string; data: State | null };
  /** Method syntax: a family of any final state is a family. */
  convicts(state: State): boolean;
};

export type DirectPurposeFamily<State = SDK.DirectClaimFinalState> = {
  /** Category label, used in every error and transaction label. */
  readonly label: string;
  readonly roleOrder: readonly DirectPurposeRole[];
  /** Named roles; fails closed when the chain is not the family's graph. */
  readonly roles: (
    contracts: DirectPurposeContracts,
  ) => Readonly<Partial<Record<DirectPurposeRole, SDK.SpendingValidator>>>;
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
  family: DirectPurposeFamily<unknown>,
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
  family: DirectPurposeFamily<unknown>,
  role: DirectPurposeRole,
): number => {
  const index = family.roleOrder.indexOf(role);
  if (index < 0)
    throw new Error(`${family.label}: ${role} is not a role of the family`);
  return index;
};
