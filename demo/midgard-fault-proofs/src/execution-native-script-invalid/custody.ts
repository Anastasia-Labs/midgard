/** One custody transition of the `executionNativeScriptInvalid` thread: the role it
 * spends, the role it pays, the exact next datum, the fields it opens and the
 * redeemer built against Lucid's completed layout. */
import * as SDK from "@al-ft/midgard-sdk";
import type { RedeemerContext, UTxO } from "@lucid-evolution/lucid";

import type { DirectPurposeTransition } from "../direct-purpose-family/custody.js";
import { createDirectFieldPlan } from "../direct-script-derivation/index.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import {
  EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL,
  type ExecutionNativeScriptInvalidRole,
} from "./contracts.js";

export type ExecutionNativeScriptInvalidRedeemerArgs = {
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

export type ExecutionNativeScriptInvalidTransition = {
  readonly label: string;
  readonly role: ExecutionNativeScriptInvalidRole;
  readonly next: ExecutionNativeScriptInvalidRole;
  readonly datum: string;
  readonly fields: readonly number[];
  /** A PHAS membership withdrawal rides the transaction. */
  readonly membership?: { readonly redeemer: string };
  readonly redeemer: (args: ExecutionNativeScriptInvalidRedeemerArgs) => string;
};

export type ExecutionNativeScriptInvalidFieldPlan = ReturnType<
  typeof createDirectFieldPlan
>;

/** Every field a thread transition opens: the input, output, observer,
 * mint, script-witness, address-witness and redeemer fields. */
export const EXECUTION_NATIVE_SCRIPT_INVALID_FIELDS: readonly number[] = [
  0, 1, 2, 3, 5, 6, 7, 8,
];

/** Every field the thread opens, planned once from the bound source. */
export const executionNativeScriptInvalidFieldPlans = (
  material: DirectScriptMaterial,
  prover: string,
): ((field: number) => ExecutionNativeScriptInvalidFieldPlan) => {
  const plans = new Map(
    EXECUTION_NATIVE_SCRIPT_INVALID_FIELDS.map((field) => [
      field,
      createDirectFieldPlan(material, field, prover),
    ]),
  );
  return (field) => {
    const plan = plans.get(field);
    if (plan === undefined)
      throw new Error(
        `${EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL}: field ${field.toString()} unplanned`,
      );
    return plan;
  };
};

export const isExecutionNativeScriptInvalidRole = (
  role: string,
): role is ExecutionNativeScriptInvalidRole =>
  (
    SDK.EXECUTION_NATIVE_SCRIPT_INVALID_ROLE_ORDER as readonly string[]
  ).includes(role);

/** The shared bind and source custody transitions, which carry the same role
 * names in both families. */
export const executionNativeScriptInvalidSharedTransition = (
  transition: DirectPurposeTransition,
): ExecutionNativeScriptInvalidTransition => {
  const { role, next } = transition;
  if (
    !isExecutionNativeScriptInvalidRole(role) ||
    !isExecutionNativeScriptInvalidRole(next)
  )
    throw new Error(
      `${EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL}: shared transition ${transition.label} names a role outside this family`,
    );
  return {
    label: transition.label,
    role,
    next,
    datum: transition.datum,
    fields: transition.fields,
    ...(transition.membership === undefined
      ? {}
      : { membership: transition.membership }),
    redeemer: transition.redeemer,
  };
};
