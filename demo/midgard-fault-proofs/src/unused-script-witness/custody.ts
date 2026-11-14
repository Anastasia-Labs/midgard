/** The fields the `unusedScriptWitness` thread opens. Its custody
 * transitions are the kit's `DirectPurposeTransition`. */
import type { DirectPurposeFieldPlan } from "../direct-purpose-family/custody.js";
import { createDirectFieldPlan } from "../direct-script-derivation/index.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import { UNUSED_SCRIPT_WITNESS_CATEGORY_LABEL } from "./contracts.js";

/** Every field a thread transition opens: the input, output, observer, mint
 * and witness fields. */
export const UNUSED_SCRIPT_WITNESS_FIELDS: readonly number[] = [
  0, 1, 2, 3, 5, 6,
];

/** Every field the thread opens, planned once from the bound source. */
export const unusedScriptWitnessFieldPlans = (
  material: DirectScriptMaterial,
  prover: string,
): ((field: number) => DirectPurposeFieldPlan) => {
  const plans = new Map(
    UNUSED_SCRIPT_WITNESS_FIELDS.map((field) => [
      field,
      createDirectFieldPlan(material, field, prover),
    ]),
  );
  return (field) => {
    const plan = plans.get(field);
    if (plan === undefined)
      throw new Error(
        `${UNUSED_SCRIPT_WITNESS_CATEGORY_LABEL}: field ${field.toString()} unplanned`,
      );
    return plan;
  };
};
