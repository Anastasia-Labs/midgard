import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { LucidEvolution, TxSignBuilder } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { planSubmitTiming, type SubmitTimingPlan } from "./submit-timing.js";
import { inspectSignedTxValidityInterval } from "./utils.await-required-output-visibility.js";
import {
  EARLY_VALIDITY_RETRY_SLOT_BUFFER,
  type SignedTxValidityInterval,
} from "./utils.parse-structured-outside-validity-interval-details.js";
import {
  resolvePreSubmitSlotSnapshot,
  type SubmitRecoveryOptions,
} from "./utils.submit-recovery-options.js";

export const preSubmitValidityCheck = (
  lucid: LucidEvolution,
  signed: Awaited<ReturnType<TxSignBuilder["complete"]>>,
  options: SubmitRecoveryOptions,
): Effect.Effect<SubmitTimingPlan, never> =>
  Effect.gen(function* () {
    if (typeof signed.toCBOR !== "function") {
      return {
        status: "ready",
        callerLabel: options.label ?? "submit",
        waitMs: 0,
      };
    }
    const interval = inspectSignedTxValidityInterval(signed.toCBOR());
    if (
      interval.invalidBeforeSlot === undefined &&
      interval.invalidHereafterSlot === undefined
    ) {
      return {
        status: "ready",
        callerLabel: options.label ?? "submit",
        waitMs: 0,
      };
    }
    const slotSnapshotResult = yield* Effect.either(
      resolvePreSubmitSlotSnapshot(lucid, options.slotSnapshot),
    );
    if (
      slotSnapshotResult._tag === "Left" &&
      options.requireSlotForBoundedTx !== true
    ) {
      return {
        status: "ready",
        callerLabel: options.label ?? "submit",
        waitMs: 0,
      };
    }
    return planSubmitTiming({
      ...interval,
      callerLabel: options.label ?? "submit",
      slotSnapshot:
        slotSnapshotResult._tag === "Right"
          ? slotSnapshotResult.right
          : undefined,
      slotSnapshotError:
        slotSnapshotResult._tag === "Left"
          ? slotSnapshotResult.left
          : undefined,
      submitSlotBuffer: EARLY_VALIDITY_RETRY_SLOT_BUFFER,
      inlineWaitPolicy: options.inlineWaitPolicy,
      dependencyKey: options.noInlineSubmitDefer?.dependencyKey,
      invalidationKey: options.noInlineSubmitDefer?.invalidationKey,
    });
  });

export const inspectSignedTxValidityIntervalIfAvailable = (
  signed: Awaited<ReturnType<TxSignBuilder["complete"]>>,
): SignedTxValidityInterval | undefined =>
  typeof signed.toCBOR === "function"
    ? inspectSignedTxValidityInterval(signed.toCBOR())
    : undefined;

export const submitTimingFailureError = (
  txHash: string,
  plan: Exclude<SubmitTimingPlan, { readonly status: "ready" }>,
): Error => {
  switch (plan.status) {
    case "wait":
    case "not_due":
      return new Error(
        `Tx ${txHash} was built before its lower bound: the L1 ledger tip (slot ${plan.currentSlot.toString()}, ${plan.slotSource}) has not reached invalidBefore=${plan.invalidBeforeSlot.toString()} plus the submit margin (slot ${plan.targetSlot.toString()}). Build it once the tip reaches the bound, or register due work.`,
      );
    case "expired":
      return new Error(
        `Tx ${txHash} validity window is expired: currentSlot=${plan.currentSlot.toString()}, invalidBefore=${plan.invalidBeforeSlot?.toString() ?? "none"}, invalidHereafter=${plan.invalidHereafterSlot.toString()}; rebuild required`,
      );
    case "window_too_narrow":
      return new Error(
        `Tx ${txHash} validity window is too narrow for the submit margin: currentSlot=${plan.currentSlot.toString()}, invalidBefore=${plan.invalidBeforeSlot.toString()}, invalidHereafter=${plan.invalidHereafterSlot.toString()}, targetSlot=${plan.targetSlot.toString()}; rebuild required`,
      );
    case "slot_source_unavailable":
      return new Error(
        `Tx ${txHash} requires a local submit slot snapshot before bounded production submission, but no strict slot source was available: invalidBefore=${plan.invalidBeforeSlot?.toString() ?? "none"}, invalidHereafter=${plan.invalidHereafterSlot?.toString() ?? "none"}, cause=${formatUnknownError(plan.cause, { includeCause: true })}`,
      );
  }
};
