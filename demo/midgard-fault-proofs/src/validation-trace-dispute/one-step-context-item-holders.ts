/** The holders a thread of ours visits through the redeemer item step of a
 * CEK context step, between the item bind stage and the context settle: the
 * shared item chain's CEK entry, its traversal and outer normalizers, its
 * source authenticator, the executor the item step selects and the CEK
 * item settlement; then the item return, which routes the verified step by
 * its context and item stages, a hash or finalize where the route needs
 * one, and the continue or finish that writes the context's successor. */
import type * as SDK from "@al-ft/midgard-sdk";

/** The shared item chain's executors, in its authenticated roster order. */
export const CONTEXT_ITEM_EXECUTOR_HOLDERS = [
  "redeemerItemFoldMapExecutor",
  "redeemerItemFinalizeFrameExecutor",
  "redeemerItemOpenHeaderExecutor",
  "redeemerItemOpenTailExecutor",
  "redeemerItemHeadScalarExecutor",
  "redeemerItemHeadSequenceExecutor",
  "redeemerItemHeadMapExecutor",
  "redeemerItemHeadLargeConstructorExecutor",
  "redeemerItemAttachIntegerExecutor",
  "redeemerItemAttachBytesExecutor",
  "redeemerItemFoldListExecutor",
  "redeemerItemAdvanceIntegerExecutor",
  "redeemerItemAdvanceBytesExecutor",
  "redeemerItemAdvanceLargeConstructorExecutor",
  "redeemerItemAdvanceLargeFieldsExecutor",
  "redeemerItemCloseExecutor",
  "redeemerItemFinishDataExecutor",
] as const;

/** The shared item chain's holders past the item bind stage, before the
 * item return. */
export const CONTEXT_ITEM_CHAIN_HOLDERS = [
  "contextItemEntry",
  "redeemerItemTraversalNormalizer",
  "redeemerItemOuterNormalizer",
  "redeemerItemSourceAuthenticator",
  ...CONTEXT_ITEM_EXECUTOR_HOLDERS,
  "contextItemSettlement",
] as const;

export type ValidationTraceContextItemChainHolder =
  (typeof CONTEXT_ITEM_CHAIN_HOLDERS)[number];

/**
 * The item return's routes, in the order of its `return_script_hashes`:
 * a selection scan's step that continues or finishes the scan, and a data
 * scan's step that continues, finishes a descriptor or finishes a value.
 */
export const CONTEXT_ITEM_ROUTES = [
  "contextItemSelectionHash",
  "contextItemSelectionFinish",
  "contextItemDataHash",
  "contextItemDataFinishDescriptor",
  "contextItemFinalize",
] as const;

/** The holders from the item return to the context settle. */
export const CONTEXT_ITEM_TAIL_HOLDERS = [
  "contextItemReturn",
  ...CONTEXT_ITEM_ROUTES,
  "contextItemSelectionContinue",
  "contextItemDataContinue",
  "contextItemDataFinishValue",
] as const;

export type ValidationTraceContextItemTailHolder =
  (typeof CONTEXT_ITEM_TAIL_HOLDERS)[number];

export type ValidationTraceContextItemHolder =
  | ValidationTraceContextItemChainHolder
  | ValidationTraceContextItemTailHolder;

export const CONTEXT_ITEM_HOLDERS: readonly ValidationTraceContextItemHolder[] =
  Object.freeze([...CONTEXT_ITEM_CHAIN_HOLDERS, ...CONTEXT_ITEM_TAIL_HOLDERS]);

const ITEM_HOLDERS: ReadonlySet<string> = new Set(CONTEXT_ITEM_HOLDERS);
const CHAIN_HOLDERS: ReadonlySet<string> = new Set(CONTEXT_ITEM_CHAIN_HOLDERS);

export const isValidationTraceContextItemHolder = (
  holder: string,
): holder is ValidationTraceContextItemHolder => ITEM_HOLDERS.has(holder);

export const isValidationTraceContextItemChainHolder = (
  holder: string,
): holder is ValidationTraceContextItemChainHolder => CHAIN_HOLDERS.has(holder);

/** The deployment manifest's name for each item holder. */
export const CONTEXT_ITEM_CONTRACT_NAMES = Object.freeze({
  contextItemEntry: "validationTraceDisputeCekContextItemEntry",
  redeemerItemTraversalNormalizer:
    "validationTraceDisputeRedeemerItemTraversalNormalizer",
  redeemerItemOuterNormalizer:
    "validationTraceDisputeRedeemerItemOuterNormalizer",
  redeemerItemSourceAuthenticator:
    "validationTraceDisputeRedeemerItemSourceAuthenticator",
  redeemerItemFoldMapExecutor:
    "validationTraceDisputeRedeemerItemFoldMapExecutor",
  redeemerItemFinalizeFrameExecutor:
    "validationTraceDisputeRedeemerItemFinalizeFrameExecutor",
  redeemerItemOpenHeaderExecutor:
    "validationTraceDisputeRedeemerItemOpenHeaderExecutor",
  redeemerItemOpenTailExecutor:
    "validationTraceDisputeRedeemerItemOpenTailExecutor",
  redeemerItemHeadScalarExecutor:
    "validationTraceDisputeRedeemerItemHeadScalarExecutor",
  redeemerItemHeadSequenceExecutor:
    "validationTraceDisputeRedeemerItemHeadSequenceExecutor",
  redeemerItemHeadMapExecutor:
    "validationTraceDisputeRedeemerItemHeadMapExecutor",
  redeemerItemHeadLargeConstructorExecutor:
    "validationTraceDisputeRedeemerItemHeadLargeConstructorExecutor",
  redeemerItemAttachIntegerExecutor:
    "validationTraceDisputeRedeemerItemAttachIntegerExecutor",
  redeemerItemAttachBytesExecutor:
    "validationTraceDisputeRedeemerItemAttachBytesExecutor",
  redeemerItemFoldListExecutor:
    "validationTraceDisputeRedeemerItemFoldListExecutor",
  redeemerItemAdvanceIntegerExecutor:
    "validationTraceDisputeRedeemerItemAdvanceIntegerExecutor",
  redeemerItemAdvanceBytesExecutor:
    "validationTraceDisputeRedeemerItemAdvanceBytesExecutor",
  redeemerItemAdvanceLargeConstructorExecutor:
    "validationTraceDisputeRedeemerItemAdvanceLargeConstructorExecutor",
  redeemerItemAdvanceLargeFieldsExecutor:
    "validationTraceDisputeRedeemerItemAdvanceLargeFieldsExecutor",
  redeemerItemCloseExecutor: "validationTraceDisputeRedeemerItemCloseExecutor",
  redeemerItemFinishDataExecutor:
    "validationTraceDisputeRedeemerItemFinishDataExecutor",
  contextItemSettlement: "validationTraceDisputeCekContextItemSettlement",
  contextItemReturn: "validationTraceDisputeCekContextItemReturn",
  contextItemSelectionHash: "validationTraceDisputeCekContextItemSelectionHash",
  contextItemSelectionFinish:
    "validationTraceDisputeCekContextItemSelectionFinish",
  contextItemDataHash: "validationTraceDisputeCekContextItemDataHash",
  contextItemDataFinishDescriptor:
    "validationTraceDisputeCekContextItemDataFinishDescriptor",
  contextItemFinalize: "validationTraceDisputeCekContextItemFinalize",
  contextItemSelectionContinue:
    "validationTraceDisputeCekContextItemSelectionContinue",
  contextItemDataContinue: "validationTraceDisputeCekContextItemDataContinue",
  contextItemDataFinishValue:
    "validationTraceDisputeCekContextItemDataFinishValue",
} as const satisfies Readonly<
  Record<ValidationTraceContextItemHolder, string>
>);

type Validator = SDK.SpendingValidator;

/** The validator of each item holder on a deployment's chain. */
export const contextItemValidators = ({
  cekContextStages: tail,
  cekContextItemStages: chain,
}: Readonly<{
  cekContextStages: SDK.CekContextStages;
  cekContextItemStages: SDK.SharedRedeemerItemStages;
}>): Readonly<Record<ValidationTraceContextItemHolder, Validator>> => {
  const executors = {} as Record<
    (typeof CONTEXT_ITEM_EXECUTOR_HOLDERS)[number],
    Validator
  >;
  for (const [index, holder] of CONTEXT_ITEM_EXECUTOR_HOLDERS.entries()) {
    const executor = chain.executors[index];
    if (executor === undefined)
      throw new Error("validation-trace CEK item executor roster is short");
    executors[holder] = executor;
  }
  return Object.freeze({
    contextItemEntry: chain.entry,
    redeemerItemTraversalNormalizer: chain.traversalNormalizer,
    redeemerItemOuterNormalizer: chain.outerNormalizer,
    redeemerItemSourceAuthenticator: chain.sourceAuthenticator,
    ...executors,
    contextItemSettlement: chain.settlement,
    contextItemReturn: tail.itemReturn,
    contextItemSelectionHash: tail.itemSelectionHash,
    contextItemSelectionFinish: tail.itemSelectionFinish,
    contextItemDataHash: tail.itemDataHash,
    contextItemDataFinishDescriptor: tail.itemDataFinishDescriptor,
    contextItemFinalize: tail.itemFinalize,
    contextItemSelectionContinue: tail.itemSelectionContinue,
    contextItemDataContinue: tail.itemDataContinue,
    contextItemDataFinishValue: tail.itemDataFinishValue,
  });
};
