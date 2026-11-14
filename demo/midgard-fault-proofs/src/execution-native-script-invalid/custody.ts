/** The fields the `executionNativeScriptInvalid` thread opens. Its custody
 * transitions are the kit's `DirectPurposeTransition`. */
import type { DirectPurposeFieldPlan } from "../direct-purpose-family/custody.js";
import { createDirectFieldPlan } from "../direct-script-derivation/index.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import { EXECUTION_NATIVE_SCRIPT_INVALID_CATEGORY_LABEL } from "./contracts.js";

/** Every field a thread transition opens: the input, output, observer,
 * mint, script-witness, address-witness and redeemer fields. */
export const EXECUTION_NATIVE_SCRIPT_INVALID_FIELDS: readonly number[] = [
  0, 1, 2, 3, 5, 6, 7, 8,
];

/** Every field the thread opens, planned once from the bound source. */
export const executionNativeScriptInvalidFieldPlans = (
  material: DirectScriptMaterial,
  prover: string,
): ((field: number) => DirectPurposeFieldPlan) => {
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
