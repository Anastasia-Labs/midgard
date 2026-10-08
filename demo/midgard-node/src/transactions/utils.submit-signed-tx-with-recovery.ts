import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import { type SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import { LucidEvolution, TxSignBuilder } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import {
  IntentJournal,
  type SubmissionIntent,
} from "../services/intent-journal.js";
import {
  planSubmitTiming,
  planSubmitTimingAfterInlineWait,
} from "./submit-timing.js";
import {
  awaitExactTransactionConfirmation,
  BeforeSignedTransactionSubmission,
} from "./utils.await-required-output-visibility.js";
import {
  DEFAULT_SIGNED_TX_INLINE_WAIT_MS,
  EARLY_VALIDITY_RETRY_SLOT_BUFFER,
  INIT_RETRY_AFTER_MILLIS,
  isUnknownOutputReferenceSubmitError,
  parseOutsideValidityIntervalDetails,
  resolveEarlyValidityRetry,
  RETRY_ATTEMPTS,
  SLOT_LENGTH_MS,
  SUBMIT_RECOVERY_AWAIT_TIMEOUT_MS,
  SUBMIT_RECOVERY_POLL_INTERVAL_MS,
} from "./utils.parse-structured-outside-validity-interval-details.js";
import {
  inspectSignedTxValidityIntervalIfAvailable,
  preSubmitValidityCheck,
  submitTimingFailureError,
} from "./utils.pre-submit-validity-check.js";
import {
  noInlineSubmitDeferFromTimingPlan,
  noInlineSubmitProviderSlotDefer,
  resolvePreSubmitSlotSnapshot,
  type SubmitRecoveryOptions,
  submitRecoverySleep,
} from "./utils.reconcile-wallet-utxos-from-signed-tx.js";

/**
 * Submits signed bytes with recovery for provider races and early-validity
 * failures. The intent journal (§8.2) records the exact bytes immediately
 * before the first submission; a refusal stops the submission
 * (`IntentJournalRefused`). Every retry here sends the same bytes.
 */
export const submitSignedTxWithRecovery = (
  lucid: LucidEvolution,
  signed: Awaited<ReturnType<TxSignBuilder["complete"]>>,
  txHash: string,
  intent: SubmissionIntent,
  options: SubmitRecoveryOptions = {},
): Effect.Effect<void, unknown, IntentJournal> =>
  Effect.gen(function* () {
    const journal = yield* IntentJournal;
    let journaled = false;
    const sleep = options.sleep ?? submitRecoverySleep(lucid);
    let providerRetryAttempts = 0;
    let outsideValidityRecoveryAttempts = 0;
    let outsideValidityRecoveryWaitedMs = 0;
    const maxOutsideValidityRecoveryWaitMs =
      options.maxPreSubmitWaitMs ?? DEFAULT_SIGNED_TX_INLINE_WAIT_MS;
    const maxOutsideValidityRecoveryAttempts = Math.max(
      1,
      Math.ceil(maxOutsideValidityRecoveryWaitMs / SLOT_LENGTH_MS),
    );

    for (;;) {
      const preSubmitValidity = yield* preSubmitValidityCheck(
        lucid,
        signed,
        options,
      );
      if (
        preSubmitValidity.status === "expired" ||
        preSubmitValidity.status === "window_too_narrow"
      ) {
        return yield* Effect.fail(
          submitTimingFailureError(txHash, preSubmitValidity),
        );
      }
      if (preSubmitValidity.status === "slot_source_unavailable") {
        return yield* Effect.fail(
          submitTimingFailureError(txHash, preSubmitValidity),
        );
      }
      const preSubmitDefer = noInlineSubmitDeferFromTimingPlan(
        "pre_submit_validity",
        txHash,
        preSubmitValidity,
        options,
      );
      if (preSubmitDefer !== undefined) {
        return yield* Effect.fail(preSubmitDefer);
      }
      if (preSubmitValidity.status === "not_due") {
        return yield* Effect.fail(
          submitTimingFailureError(txHash, preSubmitValidity),
        );
      }
      if (preSubmitValidity.status === "slot_source_stalled") {
        return yield* Effect.fail(
          submitTimingFailureError(txHash, preSubmitValidity),
        );
      }
      if (preSubmitValidity.status === "wait") {
        yield* Effect.logWarning(
          [
            `Tx ${txHash} has not reached its validity submit margin;`,
            `currentSlot=${preSubmitValidity.currentSlot.toString()},`,
            `slotSource=${preSubmitValidity.slotSource},`,
            `invalidBefore=${preSubmitValidity.invalidBeforeSlot.toString()},`,
            `invalidHereafter=${preSubmitValidity.invalidHereafterSlot?.toString() ?? "none"}.`,
            `Waiting ${preSubmitValidity.waitMs.toString()}ms before submit`,
            options.label === undefined ? "" : `label=${options.label}.`,
          ].join(" "),
        );
        yield* sleep(preSubmitValidity.waitMs);
        const refreshedSnapshot = yield* Effect.either(
          resolvePreSubmitSlotSnapshot(lucid, options.slotSnapshot),
        );
        if (refreshedSnapshot._tag === "Left") {
          return yield* Effect.fail(
            new Error(
              `Tx ${txHash} failed to refresh local submit slot evidence after inline wait: ${formatUnknownError(
                refreshedSnapshot.left,
                { includeCause: true },
              )}`,
            ),
          );
        }
        const afterWait = planSubmitTimingAfterInlineWait(
          preSubmitValidity,
          refreshedSnapshot.right,
        );
        if (afterWait.status !== "ready") {
          if (afterWait.status === "wait") {
            return yield* Effect.fail(
              new Error(
                `Tx ${txHash} local submit slot evidence still requires another wait after inline wait: currentSlot=${afterWait.currentSlot.toString()}, targetSlot=${afterWait.targetSlot.toString()}; rebuild required`,
              ),
            );
          }
          return yield* Effect.fail(
            submitTimingFailureError(txHash, afterWait),
          );
        }
      }
      if (!journaled) {
        yield* journal.record(intent, signed.toCBOR(), txHash);
        journaled = true;
      }
      // The callback must finish its durable commit before any provider call.
      // It runs for each attempt so a generation change also fences retries.
      const durable = yield* Effect.serviceOption(
        BeforeSignedTransactionSubmission,
      );
      if (Option.isSome(durable))
        yield* durable.value.persist({ txHash, signedTxCbor: signed.toCBOR() });
      const submitResult = yield* Effect.either(signed.submitProgram());
      if (submitResult._tag === "Right") {
        return;
      }

      const e = submitResult.left;
      const submitError = formatUnknownError(e, { includeCause: true });
      const outsideValidityDetails =
        parseOutsideValidityIntervalDetails(e) ??
        parseOutsideValidityIntervalDetails(submitError);
      if (outsideValidityDetails !== null) {
        if (
          outsideValidityRecoveryAttempts >= maxOutsideValidityRecoveryAttempts
        ) {
          return yield* Effect.fail(
            new Error(
              `Tx ${txHash} provider still reports OutsideValidityInterval after ${outsideValidityRecoveryAttempts.toString()} bounded early-validity recoveries (${outsideValidityRecoveryWaitedMs.toString()}ms waited): ${submitError}`,
            ),
          );
        }
        const signedValidity =
          inspectSignedTxValidityIntervalIfAvailable(signed);
        const invalidHereafterSlot =
          outsideValidityDetails.invalidHereafterSlot ??
          signedValidity?.invalidHereafterSlot;
        const slotSnapshotResult = yield* Effect.either(
          resolvePreSubmitSlotSnapshot(lucid, options.slotSnapshot),
        );
        const providerSnapshot: SubmitSlotSnapshot = {
          source: "test",
          currentSlot: outsideValidityDetails.currentSlot,
          observedAtMs: Date.now(),
          slotLengthMs: SLOT_LENGTH_MS,
        };
        const recoveryPlan = planSubmitTiming({
          invalidBeforeSlot: outsideValidityDetails.invalidBeforeSlot,
          invalidHereafterSlot,
          callerLabel: options.label ?? "submit",
          slotSnapshot:
            slotSnapshotResult._tag === "Right"
              ? slotSnapshotResult.right
              : providerSnapshot,
          slotSnapshotError:
            slotSnapshotResult._tag === "Left"
              ? slotSnapshotResult.left
              : undefined,
          submitSlotBuffer: EARLY_VALIDITY_RETRY_SLOT_BUFFER,
          maxInlineWaitMs:
            options.maxPreSubmitWaitMs ?? DEFAULT_SIGNED_TX_INLINE_WAIT_MS,
          inlineWaitPolicy: options.inlineWaitPolicy,
          dependencyKey: options.noInlineSubmitDefer?.dependencyKey,
          invalidationKey: options.noInlineSubmitDefer?.invalidationKey,
        });
        if (recoveryPlan.status === "ready") {
          const providerDetails = {
            ...outsideValidityDetails,
            ...(invalidHereafterSlot === undefined
              ? {}
              : { invalidHereafterSlot }),
          };
          const providerRetry = resolveEarlyValidityRetry(
            providerDetails,
            outsideValidityRecoveryAttempts,
            maxOutsideValidityRecoveryAttempts,
          );
          if (providerRetry.status === "wait") {
            const providerDefer = noInlineSubmitProviderSlotDefer({
              txHash,
              options,
              callerLabel: recoveryPlan.callerLabel,
              kind: "provider_slot_wait",
              currentSlot: outsideValidityDetails.currentSlot,
              targetSlot: providerRetry.targetSlot,
              waitMs: providerRetry.waitMs,
              invalidBeforeSlot: outsideValidityDetails.invalidBeforeSlot,
              invalidHereafterSlot,
            });
            if (providerDefer !== undefined) {
              return yield* Effect.fail(providerDefer);
            }
            const remainingWaitMs =
              maxOutsideValidityRecoveryWaitMs -
              outsideValidityRecoveryWaitedMs;
            if (
              providerRetry.waitMs > remainingWaitMs ||
              remainingWaitMs <= 0
            ) {
              return yield* Effect.fail(
                new Error(
                  `Tx ${txHash} stale Ogmios early-validity recovery would wait ${providerRetry.waitMs.toString()}ms, exceeding remaining bounded recovery budget=${Math.max(0, remainingWaitMs).toString()}ms after ${outsideValidityRecoveryAttempts.toString()} attempt(s) and ${outsideValidityRecoveryWaitedMs.toString()}ms waited; rebuild required`,
                ),
              );
            }
            outsideValidityRecoveryAttempts += 1;
            outsideValidityRecoveryWaitedMs += providerRetry.waitMs;
            yield* Effect.logWarning(
              [
                `Tx ${txHash} provider returned an early-validity error,`,
                `and its submit ledger slot is still behind the submit margin`,
                `(providerSlot=${outsideValidityDetails.currentSlot},`,
                `localSlot=${recoveryPlan.currentSlot ?? "unknown"},`,
                `targetSlot=${providerRetry.targetSlot},`,
                `invalidBefore=${outsideValidityDetails.invalidBeforeSlot},`,
                `invalidHereafter=${invalidHereafterSlot ?? "none"}).`,
                `Waiting ${providerRetry.waitMs.toString()}ms before bounded retry`,
                `${outsideValidityRecoveryAttempts.toString()}/${maxOutsideValidityRecoveryAttempts.toString()}`,
                `(waited=${outsideValidityRecoveryWaitedMs.toString()}ms/${maxOutsideValidityRecoveryWaitMs.toString()}ms).`,
              ].join(" "),
            );
            yield* sleep(providerRetry.waitMs);
            continue;
          }
          if (providerRetry.status === "attempts_exhausted") {
            return yield* Effect.fail(
              new Error(
                `Tx ${txHash} stale Ogmios early-validity recovery exhausted ${maxOutsideValidityRecoveryAttempts.toString()} attempts after ${outsideValidityRecoveryWaitedMs.toString()}ms waited; rebuild required`,
              ),
            );
          }
          if (
            providerRetry.status === "expired" ||
            providerRetry.status === "window_too_narrow"
          ) {
            return yield* Effect.fail(
              new Error(
                `Tx ${txHash} stale Ogmios early-validity recovery is not safe: status=${providerRetry.status}, providerSlot=${outsideValidityDetails.currentSlot.toString()}, invalidBefore=${outsideValidityDetails.invalidBeforeSlot.toString()}, invalidHereafter=${invalidHereafterSlot?.toString() ?? "none"}; rebuild required`,
              ),
            );
          }
          const remainingWaitMs =
            maxOutsideValidityRecoveryWaitMs - outsideValidityRecoveryWaitedMs;
          const staleProviderDefer = noInlineSubmitProviderSlotDefer({
            txHash,
            options,
            callerLabel: recoveryPlan.callerLabel,
            kind: "provider_slot_wait",
            currentSlot: outsideValidityDetails.currentSlot,
            targetSlot: outsideValidityDetails.currentSlot + 1,
            waitMs: SLOT_LENGTH_MS,
            invalidBeforeSlot: outsideValidityDetails.invalidBeforeSlot,
            invalidHereafterSlot,
          });
          if (staleProviderDefer !== undefined) {
            return yield* Effect.fail(staleProviderDefer);
          }
          if (SLOT_LENGTH_MS > remainingWaitMs || remainingWaitMs <= 0) {
            return yield* Effect.fail(
              new Error(
                `Tx ${txHash} stale Ogmios early-validity recovery would wait ${SLOT_LENGTH_MS.toString()}ms, exceeding remaining bounded recovery budget=${Math.max(0, remainingWaitMs).toString()}ms after ${outsideValidityRecoveryAttempts.toString()} attempt(s) and ${outsideValidityRecoveryWaitedMs.toString()}ms waited; rebuild required`,
              ),
            );
          }
          outsideValidityRecoveryAttempts += 1;
          outsideValidityRecoveryWaitedMs += SLOT_LENGTH_MS;
          yield* Effect.logWarning(
            [
              `Tx ${txHash} provider returned an early-validity error,`,
              `but local slot evidence now satisfies the submit margin`,
              `(slot=${recoveryPlan.currentSlot ?? outsideValidityDetails.currentSlot},`,
              `invalidBefore=${outsideValidityDetails.invalidBeforeSlot},`,
              `invalidHereafter=${invalidHereafterSlot ?? "none"}).`,
              `Waiting ${SLOT_LENGTH_MS.toString()}ms before bounded retry`,
              `${outsideValidityRecoveryAttempts.toString()}/${maxOutsideValidityRecoveryAttempts.toString()}`,
              `(waited=${outsideValidityRecoveryWaitedMs.toString()}ms/${maxOutsideValidityRecoveryWaitMs.toString()}ms).`,
            ].join(" "),
          );
          yield* sleep(SLOT_LENGTH_MS);
          continue;
        }
        const recoveryDefer = noInlineSubmitDeferFromTimingPlan(
          "early_validity_recovery",
          txHash,
          recoveryPlan,
          options,
        );
        if (recoveryDefer !== undefined) {
          return yield* Effect.fail(recoveryDefer);
        }
        if (recoveryPlan.status !== "wait") {
          return yield* Effect.fail(
            submitTimingFailureError(txHash, recoveryPlan),
          );
        }
        const remainingWaitMs =
          maxOutsideValidityRecoveryWaitMs - outsideValidityRecoveryWaitedMs;
        if (
          recoveryPlan.waitMs > remainingWaitMs ||
          remainingWaitMs <= 0 ||
          outsideValidityRecoveryAttempts >= maxOutsideValidityRecoveryAttempts
        ) {
          return yield* Effect.fail(
            new Error(
              `Tx ${txHash} early-validity recovery would wait ${recoveryPlan.waitMs.toString()}ms, exceeding bounded recovery budget=${Math.max(0, remainingWaitMs).toString()}ms after ${outsideValidityRecoveryAttempts.toString()} attempt(s) and ${outsideValidityRecoveryWaitedMs.toString()}ms waited; rebuild required`,
            ),
          );
        }
        outsideValidityRecoveryAttempts += 1;
        outsideValidityRecoveryWaitedMs += recoveryPlan.waitMs;
        yield* Effect.logWarning(
          [
            `Tx ${txHash} submitted before validity interval opened `,
            `(slot=${recoveryPlan.currentSlot},`,
            `invalidBefore=${recoveryPlan.invalidBeforeSlot},`,
            `invalidHereafter=${recoveryPlan.invalidHereafterSlot ?? "none"}).`,
            ` Waiting ${recoveryPlan.waitMs.toString()}ms before bounded retry`,
            `${outsideValidityRecoveryAttempts.toString()}/${maxOutsideValidityRecoveryAttempts.toString()}`,
            `(waited=${outsideValidityRecoveryWaitedMs.toString()}ms/${maxOutsideValidityRecoveryWaitMs.toString()}ms).`,
          ].join(" "),
        );
        yield* sleep(recoveryPlan.waitMs);
        continue;
      }

      if (
        isUnknownOutputReferenceSubmitError(e) &&
        options.inlineWaitPolicy === "defer_positive_wait" &&
        options.unknownInputsFailFast === true
      ) {
        // An owner that opted in never blocks on a status wait: it keeps its
        // durable intent and its own reconciliation decides whether this
        // exact transaction landed or its inputs were spent by another.
        return yield* Effect.fail(
          new Error(
            `Tx ${txHash} submit reported unknown inputs in no-inline mode; failing without an inline confirmation wait: ${submitError}`,
            { cause: e },
          ),
        );
      }

      if (isUnknownOutputReferenceSubmitError(e)) {
        yield* Effect.logWarning(
          `Tx submit reported unknown inputs for ${txHash}; verifying the exact transaction through provider-neutral status before failing: ${submitError}`,
        );
        const confirmation = yield* Effect.either(
          Effect.tryPromise(() =>
            awaitExactTransactionConfirmation(lucid, txHash, {
              timeout: SUBMIT_RECOVERY_AWAIT_TIMEOUT_MS,
              checkInterval: SUBMIT_RECOVERY_POLL_INTERVAL_MS,
            }),
          ),
        );
        if (confirmation._tag === "Right") {
          yield* Effect.logInfo(
            `Tx ${txHash} confirmed after submit race; treating submission as successful.`,
          );
          return;
        }
        return yield* Effect.fail(e);
      }

      if (options.inlineWaitPolicy === "defer_positive_wait") {
        return yield* Effect.fail(
          new Error(
            `Tx ${txHash} submit failed with provider error in no-inline mode; refusing provider retry sleep under ownership: ${submitError}`,
          ),
        );
      }

      if (providerRetryAttempts < RETRY_ATTEMPTS) {
        const waitMs = INIT_RETRY_AFTER_MILLIS * 2 ** providerRetryAttempts;
        providerRetryAttempts += 1;
        yield* Effect.logWarning(
          `Tx ${txHash} submit failed with provider error; waiting ${waitMs}ms before retry ${providerRetryAttempts}/${RETRY_ATTEMPTS}: ${submitError}`,
        );
        yield* sleep(waitMs);
        continue;
      }

      return yield* Effect.fail(e);
    }
  });
