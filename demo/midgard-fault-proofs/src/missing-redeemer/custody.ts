/** One custody transition of the `missingRedeemer` thread: the role it spends,
 * the role it pays, the exact next datum, the fields it opens and the
 * redeemer built against Lucid's completed layout. */
import type * as SDK from "@al-ft/midgard-sdk";
import { Data, type RedeemerContext, type UTxO } from "@lucid-evolution/lucid";

import { createDirectFieldPlan } from "../direct-script-derivation/index.js";
import type { DirectScriptMaterial } from "../direct-script-derivation/material.js";
import type { MissingRedeemerRole } from "./contracts.js";

export type MissingRedeemerIndices = {
  readonly input_index: bigint;
  readonly output_index: bigint;
};

export type MissingRedeemerRedeemerArgs = {
  readonly indices: MissingRedeemerIndices;
  readonly open: (field: number) => SDK.FieldOpening;
  readonly ctx: RedeemerContext;
  readonly thread: UTxO;
  readonly unit: string;
  readonly nextAddress: string;
  readonly membershipRewardAddress: string;
};

export type MissingRedeemerTransition = {
  readonly label: string;
  readonly role: MissingRedeemerRole;
  readonly next: MissingRedeemerRole;
  readonly datum: string;
  readonly fields: readonly number[];
  /** A PHAS membership withdrawal rides the transaction. */
  readonly membership?: { readonly redeemer: string };
  readonly redeemer: (args: MissingRedeemerRedeemerArgs) => string;
};

export type MissingRedeemerFieldPlan = ReturnType<typeof createDirectFieldPlan>;

/** Every field a thread transition can open: the input, output, observer,
 * mint, witness and redeemer fields. */
export const MISSING_REDEEMER_FIELDS: readonly number[] = [0, 1, 2, 3, 5, 6, 8];

/** Every field the thread opens, planned once from the bound source. */
export const missingRedeemerFieldPlans = (
  material: DirectScriptMaterial,
  prover: string,
): ((field: number) => MissingRedeemerFieldPlan) => {
  const plans = new Map(
    MISSING_REDEEMER_FIELDS.map((field) => [
      field,
      createDirectFieldPlan(material, field, prover),
    ]),
  );
  return (field) => {
    const plan = plans.get(field);
    if (plan === undefined)
      throw new Error(`missing-redeemer: field ${field.toString()} unplanned`);
    return plan;
  };
};

export const stepDatum = <T>(
  prover: string,
  data: T,
  schema: { fraud_prover: string; data: T | null },
): string => Data.to({ fraud_prover: prover, data } as never, schema as never);

/** Bounded page boundaries over `count` items: [start, budget] pairs. */
export const pages = (
  count: number,
  size: number,
): readonly (readonly [number, number])[] => {
  if (!Number.isSafeInteger(size) || size <= 0)
    throw new Error("missing-redeemer: page size must be positive");
  const result: (readonly [number, number])[] = [];
  for (let start = 0; start < count; start += size)
    result.push([start, Math.min(size, count - start)]);
  return result;
};

/** One bounded page's progress arguments at `start` of `phase`. */
export const progressArgs =
  (
    plan: MissingRedeemerFieldPlan,
    phase: "Certifying" | "Walking",
    start: number,
    budget: number,
  ) =>
  ({
    indices,
    open,
  }: Pick<MissingRedeemerRedeemerArgs, "indices" | "open">) => ({
    ...indices,
    opening: open(plan.fieldIndex),
    checkpoint_bytes: plan.position(phase, start).checkpointBytes,
    budget: BigInt(budget),
  });

/** Grammar certification always takes at least one bounded step, even over
 * an empty field, because the certify worker requires a positive budget. */
export const grammarPages = (
  count: number,
  size: number,
): readonly (readonly [number, number])[] =>
  count === 0 ? [[0, 1]] : pages(count, size);
