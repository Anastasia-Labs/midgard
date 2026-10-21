/** The hops of a thread of ours through the one-step resolution of a CEK
 * context step, past the CEK resolver: from the context step resolver, which
 * binds the step's witness and evidence (ContextBound), to the context
 * control; from the control, which stages the step's context and native
 * facts (ContextStaged), to the stage its context routes the step to; from
 * that stage, which verifies the step (ContextVerified), to the context
 * settle, or from the item bind stage through the redeemer item step
 * (one-step-context-item-hops.ts), or from the observer authenticate, which
 * binds the step's observer opening (ObserverPending), through the observer
 * fold, or from the redeemer select authenticate, which binds the select
 * (RedeemerSelectionPending), through its initialize (RedeemerPurposePending),
 * hash (RedeemerInitialPending) and finish, or from the finalize
 * authenticate, which binds a Cardano spend's redeemers
 * (FinalizationPending), through the finalize spend; and from the settle to
 * the award. Every hop is redeemed with the step's witness and evidence
 * (`VerifyContext`), and each successor carries the state its next holder
 * checks, derived from our trace (one-step-context.ts). */
import { plutusConstrFieldCbor } from "@al-ft/midgard-core/plutus-data-cbor";

import type { ValidationTraceHop } from "./one-step-builders.js";
import { contextBoundSource } from "./one-step-context.js";
import {
  isValidationTraceContextFollowerHolder,
  type ValidationTraceContextFollowerHolder,
} from "./one-step-context-followers.js";
import {
  isValidationTraceContextItemHolder,
  type ValidationTraceContextItemHolder,
} from "./one-step-context-item-holders.js";
import {
  validationTraceContextItemHop,
  type ValidationTraceContextItemReads,
} from "./one-step-context-item-hops.js";
import type { ValidationTraceContextRedeemerSelect } from "./one-step-context-redeemer-select.js";
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
  | ValidationTraceContextItemHolder
  | ValidationTraceContextFollowerHolder
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
  STAGE_HOLDERS.has(holder) ||
  isValidationTraceContextItemHolder(holder) ||
  isValidationTraceContextFollowerHolder(holder);

const redeemerSelectOf = (
  holder: ValidationTraceContextHolder,
  select: ValidationTraceContextRedeemerSelect | undefined,
): ValidationTraceContextRedeemerSelect => {
  if (select === undefined)
    throw new Error(
      `validation-trace ${holder} holds a thread whose step selects no redeemer`,
    );
  return select;
};

/**
 * The hop of the thread whose datum is `datum`, of prover `prover`, held by
 * the context holder `holder`, in the one-step resolution `plan`; a hop
 * through the redeemer item step reads `item`.
 */
export const validationTraceContextHop = ({
  plan,
  holder,
  datum,
  prover,
  item,
}: {
  readonly plan: ValidationTraceOneStepPlan;
  readonly holder: ValidationTraceContextHolder;
  readonly datum: string;
  readonly prover: string;
  readonly item?: ValidationTraceContextItemReads;
}): ValidationTraceHop => {
  if (plan.kind !== "context")
    throw new Error(
      `validation-trace ${holder} holds a thread whose step is not a CEK context step`,
    );
  if (
    holder === "contextItemBind" ||
    isValidationTraceContextItemHolder(holder)
  )
    return validationTraceContextItemHop({
      plan,
      holder,
      datum,
      prover,
      reads: item,
    });
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
  if (holder === CEK_CONTEXT_STAGE_HOLDERS.observerAuthenticate) {
    // ObserverPending { staged, opening }.
    if (context.observerOpeningCbor === undefined)
      throw new Error(
        `validation-trace ${holder} holds a thread whose step opens no observer`,
      );
    return hop("contextObserverFold", [state, context.observerOpeningCbor]);
  }
  if (holder === "contextObserverFold")
    // ContextVerified { staged, successor }, from the pending opening's
    // staged context.
    return hop("contextSettle", [
      plutusConstrFieldCbor(datum, [1, 0, 0]),
      context.successorCbor,
    ]);
  if (holder === CEK_CONTEXT_STAGE_HOLDERS.finalizeAuthenticate) {
    // FinalizationPending { staged, redeemer_items, redeemer }.
    const { finalization } = context;
    if (finalization === undefined)
      throw new Error(
        `validation-trace ${holder} holds a thread whose step finalizes no Cardano spend`,
      );
    return hop("contextFinalizeSpend", [
      state,
      finalization.redeemerItemsCbor,
      finalization.redeemerCbor,
    ]);
  }
  if (holder === "contextFinalizeSpend")
    // ContextVerified { staged, successor }, from the pending
    // finalization's staged context.
    return hop("contextSettle", [
      plutusConstrFieldCbor(datum, [1, 0, 0]),
      context.successorCbor,
    ]);
  const select = context.redeemerSelect;
  if (
    holder === CEK_CONTEXT_STAGE_HOLDERS.redeemerSelectAuthenticate &&
    select !== undefined
  )
    // RedeemerSelectionPending { staged, control, item_index, ... }.
    return hop("contextRedeemerSelectInitialize", [
      state,
      ...select.selectionFieldsCbor,
    ]);
  if (holder === "contextRedeemerSelectInitialize")
    // RedeemerPurposePending { selection, purpose }.
    return hop("contextRedeemerSelectHash", [
      state,
      redeemerSelectOf(holder, select).purposeCbor,
    ]);
  if (holder === "contextRedeemerSelectHash") {
    // RedeemerInitialPending { selection, purpose, initial_hash }.
    const { purposeCbor, initialHashCbor } = redeemerSelectOf(holder, select);
    return hop("contextRedeemerSelectFinish", [
      plutusConstrFieldCbor(datum, [1, 0, 0]),
      purposeCbor,
      initialHashCbor,
    ]);
  }
  if (holder === "contextRedeemerSelectFinish")
    // ContextVerified { staged, successor }, from the selection's staged
    // context.
    return hop("contextSettle", [
      plutusConstrFieldCbor(datum, [1, 0, 0, 0]),
      context.successorCbor,
    ]);
  // A stage, or the redeemer select authenticate's skip of a native
  // execution: ContextVerified { staged, successor }.
  return hop("contextSettle", [state, context.successorCbor]);
};
