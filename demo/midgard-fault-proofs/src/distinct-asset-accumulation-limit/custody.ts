/** One custody transition of the `distinctAssetAccumulationLimit` thread: the
 * role it spends, the role it pays, the exact next datum, the fields it opens
 * and the redeemer built against Lucid's completed layout. */
import type * as SDK from "@al-ft/midgard-sdk";
import type { RedeemerContext, UTxO } from "@lucid-evolution/lucid";

import type { DirectPurposeTransition } from "../direct-purpose-family/custody.js";
import { createDirectFieldPlan } from "../direct-script-derivation/index.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import {
  DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_LABEL,
  type DistinctAssetAccumulationLimitRole,
} from "./contracts.js";

export type DistinctAssetAccumulationLimitRedeemerArgs = {
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

export type DistinctAssetAccumulationLimitTransition = {
  readonly label: string;
  readonly role: DistinctAssetAccumulationLimitRole;
  readonly next: DistinctAssetAccumulationLimitRole;
  readonly datum: string;
  readonly fields: readonly number[];
  /** A PHAS membership withdrawal rides the transaction. */
  readonly membership?: { readonly redeemer: string };
  readonly redeemer: (
    args: DistinctAssetAccumulationLimitRedeemerArgs,
  ) => string;
};

export type DistinctAssetAccumulationLimitFieldPlan = ReturnType<
  typeof createDirectFieldPlan
>;

/** Every field a thread transition opens: the spent inputs, the reference
 * inputs (to rank a crossing input), the outputs and the mint. */
export const DISTINCT_ASSET_ACCUMULATION_LIMIT_FIELDS: readonly number[] = [
  0, 1, 2, 5,
];

/** Every field the thread opens, planned once from the bound source. */
export const distinctAssetAccumulationLimitFieldPlans = (
  material: DirectScriptMaterial,
  prover: string,
): ((field: number) => DistinctAssetAccumulationLimitFieldPlan) => {
  const plans = new Map(
    DISTINCT_ASSET_ACCUMULATION_LIMIT_FIELDS.map((field) => [
      field,
      createDirectFieldPlan(material, field, prover),
    ]),
  );
  return (field) => {
    const plan = plans.get(field);
    if (plan === undefined)
      throw new Error(
        `${DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_LABEL}: field ${field.toString()} unplanned`,
      );
    return plan;
  };
};

/** A shared bind transition. The bound pre-state is paid to this family's
 * entry, where the shared graph would pay its source walk. */
export const sharedBindTransition = (
  transition: DirectPurposeTransition,
): DistinctAssetAccumulationLimitTransition => {
  const { role, next } = transition;
  if (role === "bindEvent" && next === "bindPreState")
    return { ...transition, role, next };
  if (role === "bindPreState")
    return { ...transition, role, next: "familyEntry" };
  throw new Error(
    `${DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_LABEL}: shared transition ${transition.label} is not a bind step`,
  );
};
