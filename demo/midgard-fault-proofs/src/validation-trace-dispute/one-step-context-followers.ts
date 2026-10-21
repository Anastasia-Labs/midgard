/** The holders a thread of ours visits past the first holder of a CEK
 * context stage that verifies its step in more than one hop, before the
 * context settle: the observer fold, which folds the opening the observer
 * authenticate binds into the step's successor; and the redeemer select's
 * initialize, hash and finish, which carry on the select the redeemer
 * select authenticate binds; and the finalize spend, which finalizes the
 * Cardano spend whose redeemers the finalize authenticate binds. */
import type * as SDK from "@al-ft/midgard-sdk";

/** Each follower holder's contract among the deployment's context stages. */
export const CONTEXT_FOLLOWER_STAGES = Object.freeze({
  contextObserverFold: "observerFold",
  contextRedeemerSelectInitialize: "redeemerSelectInitialize",
  contextRedeemerSelectHash: "redeemerSelectHash",
  contextRedeemerSelectFinish: "redeemerSelectFinish",
  contextFinalizeSpend: "finalizeSpend",
} as const satisfies Readonly<Record<string, keyof SDK.CekContextStages>>);

export type ValidationTraceContextFollowerHolder =
  keyof typeof CONTEXT_FOLLOWER_STAGES;

export const CONTEXT_FOLLOWER_HOLDERS = Object.freeze(
  Object.keys(CONTEXT_FOLLOWER_STAGES),
) as readonly ValidationTraceContextFollowerHolder[];

export const isValidationTraceContextFollowerHolder = (
  holder: string,
): holder is ValidationTraceContextFollowerHolder =>
  Object.hasOwn(CONTEXT_FOLLOWER_STAGES, holder);

/** The deployment manifest's name for each follower holder. */
export const CONTEXT_FOLLOWER_CONTRACT_NAMES = Object.freeze({
  contextObserverFold: "validationTraceDisputeCekContextObserverFold",
  contextRedeemerSelectInitialize:
    "validationTraceDisputeCekContextRedeemerSelectInitialize",
  contextRedeemerSelectHash:
    "validationTraceDisputeCekContextRedeemerSelectHash",
  contextRedeemerSelectFinish:
    "validationTraceDisputeCekContextRedeemerSelectFinish",
  contextFinalizeSpend: "validationTraceDisputeCekContextFinalizeSpend",
} as const satisfies Readonly<
  Record<ValidationTraceContextFollowerHolder, string>
>);

/** The validator of each follower holder among `stages`. */
export const contextFollowerValidators = (
  stages: SDK.CekContextStages,
): Readonly<
  Record<ValidationTraceContextFollowerHolder, SDK.SpendingValidator>
> =>
  Object.freeze(
    Object.fromEntries(
      CONTEXT_FOLLOWER_HOLDERS.map((holder) => [
        holder,
        stages[CONTEXT_FOLLOWER_STAGES[holder]],
      ]),
    ) as Record<ValidationTraceContextFollowerHolder, SDK.SpendingValidator>,
  );
