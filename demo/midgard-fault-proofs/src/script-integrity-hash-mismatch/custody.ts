/** One custody transition of the `scriptIntegrityHashMismatch` thread: the
 * role it spends, the role it pays, the exact next datum, the fields it opens
 * and the redeemer built against Lucid's completed layout. */
import * as SDK from "@al-ft/midgard-sdk";
import type { RedeemerContext, UTxO } from "@lucid-evolution/lucid";

import type { DirectPurposeTransition } from "../direct-purpose-family/custody.js";
import { createDirectFieldPlan } from "../direct-script-derivation/index.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import {
  SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_LABEL,
  type ScriptIntegrityHashMismatchRole,
} from "./contracts.js";

export type ScriptIntegrityHashMismatchRedeemerArgs = {
  readonly indices: {
    readonly input_index: bigint;
    readonly output_index: bigint;
  };
  readonly open: (field: number) => SDK.FieldOpening;
  readonly ctx: RedeemerContext;
  readonly thread: UTxO;
  readonly unit: string;
  readonly nextAddress: string;
  readonly membershipRewardAddress: string;
};

export type ScriptIntegrityHashMismatchTransition = {
  readonly label: string;
  readonly role: ScriptIntegrityHashMismatchRole;
  readonly next: ScriptIntegrityHashMismatchRole;
  readonly datum: string;
  readonly fields: readonly number[];
  /** A PHAS membership withdrawal rides the transaction. */
  readonly membership?: { readonly redeemer: string };
  readonly redeemer: (args: ScriptIntegrityHashMismatchRedeemerArgs) => string;
};

export type ScriptIntegrityHashMismatchFieldPlan = ReturnType<
  typeof createDirectFieldPlan
>;

/** Every field a thread transition opens: the input, output, observer, mint
 * and witness fields. */
export const SCRIPT_INTEGRITY_HASH_MISMATCH_FIELDS: readonly number[] = [
  0, 1, 2, 3, 5, 6,
];

/** Every field the thread opens, planned once from the bound source. */
export const scriptIntegrityHashMismatchFieldPlans = (
  material: DirectScriptMaterial,
  prover: string,
): ((field: number) => ScriptIntegrityHashMismatchFieldPlan) => {
  const plans = new Map(
    SCRIPT_INTEGRITY_HASH_MISMATCH_FIELDS.map((field) => [
      field,
      createDirectFieldPlan(material, field, prover),
    ]),
  );
  return (field) => {
    const plan = plans.get(field);
    if (plan === undefined)
      throw new Error(
        `${SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_LABEL}: field ${field.toString()} unplanned`,
      );
    return plan;
  };
};

const isRole = (role: string): role is ScriptIntegrityHashMismatchRole =>
  (SDK.SCRIPT_INTEGRITY_HASH_MISMATCH_ROLE_ORDER as readonly string[]).includes(
    role,
  );

/** The shared bind and source custody transitions, which carry the same role
 * names in both families. */
export const sharedSourceTransition = (
  transition: DirectPurposeTransition,
): ScriptIntegrityHashMismatchTransition => {
  const { role, next } = transition;
  if (!isRole(role) || !isRole(next))
    throw new Error(
      `${SCRIPT_INTEGRITY_HASH_MISMATCH_CATEGORY_LABEL}: shared transition ${transition.label} names a role outside this family`,
    );
  return { ...transition, role, next };
};
