/** The fields the `distinctAssetAccumulationLimit` thread opens, and the
 * shared bind steps rerouted to its entry. Its custody transitions are the
 * kit's `DirectPurposeTransition`. */
import type {
  DirectPurposeFieldPlan,
  DirectPurposeTransition,
} from "../direct-purpose-family/custody.js";
import { createDirectFieldPlan } from "../direct-script-derivation/index.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import { DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_LABEL } from "./contracts.js";

/** Every field a thread transition opens: the spent inputs, the reference
 * inputs (to rank a crossing input), the outputs and the mint. */
export const DISTINCT_ASSET_ACCUMULATION_LIMIT_FIELDS: readonly number[] = [
  0, 1, 2, 5,
];

/** Every field the thread opens, planned once from the bound source. */
export const distinctAssetAccumulationLimitFieldPlans = (
  material: DirectScriptMaterial,
  prover: string,
): ((field: number) => DirectPurposeFieldPlan) => {
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
): DirectPurposeTransition => {
  const { role, next } = transition;
  if (role === "bindEvent" && next === "bindPreState") return transition;
  if (role === "bindPreState") return { ...transition, next: "familyEntry" };
  throw new Error(
    `${DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY_LABEL}: shared transition ${transition.label} is not a bind step`,
  );
};
