/** The hops of a thread of ours through the one-step resolution of a CEK
 * context step, past the CEK resolver: from the context step resolver, which
 * binds the step's witness and evidence (ContextBound), to the context
 * control; from the control, which stages the step's context and native
 * facts (ContextStaged), to the stage its context routes the step to; from
 * that stage, which verifies the step (ContextVerified), to the context
 * settle; and from the settle to the award. Every hop is redeemed with the
 * step's witness and evidence (`VerifyContext`), and each successor carries
 * the state its next holder checks, derived from our trace
 * (one-step-context.ts). */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";

import type { ValidationTraceHop } from "./one-step-builders.js";
import { contextBoundSource } from "./one-step-context.js";
import {
  bytes,
  constr,
  continued,
  integer,
  stepDatum,
} from "./one-step-data.js";
import {
  CEK_CONTEXT_STAGE_HOLDERS,
  type ValidationTraceContextStageHolder,
  type ValidationTraceOneStepHolder,
} from "./one-step-holders.js";
import {
  hashSerialisedData,
  type ValidationTraceContextStepPlan,
  type ValidationTraceOneStepPlan,
} from "./one-step-plan.js";

/** A holder of the thread in a context step's resolution, before the award. */
export type ValidationTraceContextHolder =
  | "context"
  | "contextControl"
  | ValidationTraceContextStageHolder
  | "contextSettle";

const STAGE_HOLDERS: ReadonlySet<string> = new Set(
  Object.values(CEK_CONTEXT_STAGE_HOLDERS),
);

export const isValidationTraceContextHolder = (
  holder: ValidationTraceOneStepHolder,
): holder is ValidationTraceContextHolder =>
  holder === "context" ||
  holder === "contextControl" ||
  holder === "contextSettle" ||
  STAGE_HOLDERS.has(holder);

/**
 * The hop of the thread whose datum is `datum`, of prover `prover`, held by
 * the context holder `holder`, in the one-step resolution `plan`.
 */
export const validationTraceContextHop = ({
  plan,
  holder,
  datum,
  prover,
}: {
  readonly plan: ValidationTraceOneStepPlan;
  readonly holder: ValidationTraceContextHolder;
  readonly datum: string;
  readonly prover: string;
}): ValidationTraceHop => {
  if (plan.kind !== "context")
    throw new Error(
      `validation-trace ${holder} holds a thread whose step is not a CEK context step`,
    );
  const { context }: ValidationTraceContextStepPlan = plan;
  // The state the thread's holder checked, which the next holder's state
  // carries first.
  const state = plutusConstrFieldCbor(datum, [1, 0]);
  const redeemer = (inputIndex: bigint, outputIndex: bigint) =>
    continued(
      constr(0, [
        integer(inputIndex),
        integer(outputIndex),
        plan.transitionCbor,
        plan.auxiliaryCbor,
      ]),
    );
  const hop = (
    next: ValidationTraceHop["next"],
    fields: readonly string[],
  ): ValidationTraceHop => ({
    next,
    datum: stepDatum(prover, constr(0, fields)),
    redeemer,
    proverOnly: true,
  });
  if (holder === "context") {
    // ContextBound { prepared, control, auxiliary_hash, transaction_id,
    // source_kind }.
    const source = contextBoundSource(state);
    return hop("contextControl", [
      state,
      context.controlCbor,
      bytes(hashSerialisedData(plan.auxiliaryCbor)),
      source.transactionIdCbor,
      source.sourceKindCbor,
    ]);
  }
  if (holder === "contextControl")
    // ContextStaged { bound, context, native, execution_cursor }.
    return hop(CEK_CONTEXT_STAGE_HOLDERS[context.stage], [
      state,
      context.contextCbor,
      context.nativeFactsCbor,
      integer(context.executionCursor),
    ]);
  if (holder === "contextSettle")
    // The winning resolution, which any party may move the thread to.
    return { ...hop("award", [integer(1)]), proverOnly: false };
  // A stage: ContextVerified { staged, successor }.
  return hop("contextSettle", [state, context.successorCbor]);
};
