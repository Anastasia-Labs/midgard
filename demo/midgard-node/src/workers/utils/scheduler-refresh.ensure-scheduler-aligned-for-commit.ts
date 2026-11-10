import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import type { SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect, Schedule } from "effect";

import {
  type IntentJournal,
  type IntentPlan,
  journaledIntent,
} from "../../services/intent-journal.js";
import { planSubmitTiming } from "../../transactions/submit-timing.js";
import {
  handleSignSubmitNoConfirmation,
  type IntendedStateProbe,
  type NoInlineSubmitRecoveryOptions,
  type TxSignError,
  type TxSubmitError,
} from "../../transactions/utils.js";
import { readSelectedWalletView } from "../../transactions/utils.wallet-view.js";
import { compareOutRefs, outRefLabel } from "../../tx-context.js";
import { getSchedulerDatumFromUTxO } from "./scheduler-refresh.fetch-fresh-active-operator-input-for-commit.js";
import {
  appointedSchedulerOperator,
  awaitSubmittedSchedulerTx,
  describeSchedulerDatum,
  parseNodeSetUtxos,
  resolveRefreshedSchedulerStartTime,
  resolveSchedulerFirstAppointmentValidityWindow,
  resolveSchedulerRefreshValidityWindow,
  resolveSchedulerRefreshWitnessSelection,
  toSdkSchedulerRefreshWitnessSelection,
} from "./scheduler-refresh.resolve-scheduler-refresh-witness-selection.js";
import {
  activeSchedulerDatum,
  activeSchedulerState,
  captureSchedulerSlotSnapshot,
  SCHEDULER_ALIGNMENT_MAX_REFRESHES_PER_CALL,
  SCHEDULER_MAX_PRE_SUBMIT_WAIT_MS,
  SCHEDULER_REFRESH_MAX_POLLS,
  SCHEDULER_REFRESH_POLL_INTERVAL,
  SCHEDULER_SUBMISSION_CONFIRMATION_POLL_INTERVAL_MS,
  type SchedulerAlignmentResult,
  schedulerRefreshDependencyKey,
  schedulerRefreshDueWorkFromNoInlineSubmitDefer,
  schedulerRefreshDueWorkFromSubmitTiming,
  schedulerRefreshRequiredOutsideMutationWorkerDueWork,
  schedulerRefreshStartTimeModeForSpendingScriptHash,
  schedulerSlotSnapshotFromSubmitSlot,
  schedulerStateCoversCommitTarget,
} from "./scheduler-refresh.scheduler-refresh-due-work-from-no-inline-submit-defer.js";

export const ensureSchedulerAlignedForCommit = (
  lucid: LucidEvolution,
  contracts: SDK.MidgardValidators,
  operatorKeyHash: string,
  schedulerRefInput: UTxO,
  activeOperatorUtxos: readonly UTxO[],
  registeredOperatorUtxos: readonly UTxO[],
  alignedEndTime: number,
  schedulerWitnessUnit: string,
  /** S5: the caller's plan, opened before it read the scheduler and lists. */
  plan: IntentPlan,
  schedulerSpendingScriptRef?: UTxO,
  submitSlotSnapshot?: () => Effect.Effect<SubmitSlotSnapshot, unknown>,
  allowSchedulerRefresh: boolean = true,
  /**
   * Whether `successor` holds all of this node's pending work (see
   * `l2-peer/successor-holds-pending.ts`). Omitted, it never does.
   */
  successorHoldsAllPending?: (successor: string) => Effect.Effect<boolean>,
): Effect.Effect<
  SchedulerAlignmentResult,
  SDK.StateQueueError | TxSignError | TxSubmitError,
  IntentJournal
> =>
  Effect.gen(function* () {
    const targetStartTime = BigInt(alignedEndTime);
    const activeNodes = yield* parseNodeSetUtxos(
      activeOperatorUtxos,
      "active-operators",
    );
    const registeredNodes = yield* parseNodeSetUtxos(
      registeredOperatorUtxos,
      "registered-operators",
    );
    let currentSchedulerRefInput = schedulerRefInput;
    const startTimeMode = schedulerRefreshStartTimeModeForSpendingScriptHash(
      contracts.scheduler.spendingScriptHash,
    );

    for (
      let refreshCount = 0;
      refreshCount <= SCHEDULER_ALIGNMENT_MAX_REFRESHES_PER_CALL;
      refreshCount += 1
    ) {
      const schedulerDatum = yield* getSchedulerDatumFromUTxO(
        currentSchedulerRefInput,
      );
      const currentSchedulerState = activeSchedulerState(schedulerDatum);
      if (
        schedulerStateCoversCommitTarget({
          currentSchedulerState,
          operatorKeyHash,
          targetStartTime,
        })
      ) {
        return { schedulerRefInput: currentSchedulerRefInput };
      }
      if (refreshCount === SCHEDULER_ALIGNMENT_MAX_REFRESHES_PER_CALL) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Scheduler alignment exceeded the maximum refresh count for one block commitment attempt",
            cause: `max_refreshes=${SCHEDULER_ALIGNMENT_MAX_REFRESHES_PER_CALL.toString()},target_commit_end=${targetStartTime.toString()},current_scheduler=${describeSchedulerDatum(schedulerDatum)}`,
          }),
        );
      }

      const currentOperator = currentSchedulerState?.operator ?? "";
      const currentStartTime = currentSchedulerState?.startTime ?? 0n;
      const allowGenesisRewind = currentSchedulerState === undefined;
      const submitSlot =
        submitSlotSnapshot === undefined
          ? undefined
          : yield* submitSlotSnapshot().pipe(
              Effect.mapError(
                (cause) =>
                  new SDK.StateQueueError({
                    message:
                      "Failed to capture local Ogmios slot for scheduler refresh validity",
                    cause,
                  }),
              ),
            );
      const schedulerSlotSnapshot =
        submitSlot === undefined
          ? captureSchedulerSlotSnapshot(lucid)
          : schedulerSlotSnapshotFromSubmitSlot(lucid, submitSlot);
      // The operator appointed is itself, or the successor it pushes the turn
      // to (see the selection).
      const { selection, appointedOperator } = yield* Effect.try({
        try: () => {
          const selection = resolveSchedulerRefreshWitnessSelection({
            currentOperator,
            targetOperator: operatorKeyHash,
            activeNodes,
            registeredNodes,
            allowGenesisRewind,
          });
          return {
            selection,
            appointedOperator: appointedSchedulerOperator(selection),
          };
        },
        catch: (cause) =>
          new SDK.StateQueueError({
            message:
              "Current operator is not eligible to advance or rewind the scheduler for this commit window",
            cause,
          }),
      });
      // A push to a successor that holds all this node's pending work waits
      // the grace period in which that successor is expected to take the
      // turn itself; a push it might need goes at once.
      const successorHoldsPending =
        selection.kind !== "AppointFirst" &&
        appointedOperator !== operatorKeyHash &&
        successorHoldsAllPending !== undefined &&
        (yield* successorHoldsAllPending(appointedOperator));
      const { validFrom, validTo } =
        selection.kind === "AppointFirst"
          ? yield* Effect.try({
              try: () =>
                resolveSchedulerFirstAppointmentValidityWindow(
                  lucid,
                  targetStartTime,
                  schedulerSlotSnapshot,
                ),
              catch: (cause) =>
                new SDK.StateQueueError({
                  message:
                    "Failed to resolve scheduler first-appointment validity window",
                  cause,
                }),
            })
          : resolveSchedulerRefreshValidityWindow(
              lucid,
              currentStartTime,
              schedulerSlotSnapshot,
              successorHoldsPending
                ? SDK.NEW_SHIFT_INACTIVITY_GRACE_PERIOD_MS
                : 0n,
            );
      const submitTimingSnapshot: SubmitSlotSnapshot = submitSlot ?? {
        source: "test",
        currentSlot: schedulerSlotSnapshot.currentSlot,
        observedAtMs: schedulerSlotSnapshot.observedAtMs,
        slotLengthMs: 1_000,
      };
      const invalidBeforeSlot = Number(lucid.unixTimeToSlot(Number(validFrom)));
      const invalidHereafterSlot = Number(
        lucid.unixTimeToSlot(Number(validTo)),
      );
      const schedulerDependencyKey = schedulerRefreshDependencyKey({
        schedulerOutRef: outRefLabel(currentSchedulerRefInput),
        currentOperator,
        currentStartTime,
        targetOperator: appointedOperator,
        selectionKind: selection.kind,
        successorHoldsPending,
      });
      const submitTiming = planSubmitTiming({
        callerLabel: "scheduler-refresh",
        invalidBeforeSlot,
        invalidHereafterSlot,
        slotSnapshot: submitTimingSnapshot,
        maxInlineWaitMs: SCHEDULER_MAX_PRE_SUBMIT_WAIT_MS,
        inlineWaitPolicy: "defer_positive_wait",
        dependencyKey: schedulerDependencyKey,
        invalidationKey: schedulerDependencyKey,
      });
      if (submitTiming.status === "not_due") {
        if (
          submitTiming.dependencyKey === undefined ||
          submitTiming.invalidationKey === undefined
        ) {
          return yield* Effect.fail(
            new SDK.StateQueueError({
              message:
                "Scheduler refresh due-work planning lost dependency evidence",
              cause: `status=not_due,dependency_key=${submitTiming.dependencyKey ?? "missing"},invalidation_key=${submitTiming.invalidationKey ?? "missing"}`,
            }),
          );
        }
        const dueWorkOutput = schedulerRefreshDueWorkFromSubmitTiming({
          plan: {
            ...submitTiming,
            dependencyKey: submitTiming.dependencyKey,
            invalidationKey: submitTiming.invalidationKey,
          },
        });
        const entry = dueWorkOutput.dueWork;
        if (successorHoldsPending)
          yield* Effect.logInfo(
            `🔹 Successor ${appointedOperator} holds all pending L2 txs; letting it take the turn first, pushing only after the new-shift grace period (due_slot=${entry.dueSlot.toString()}).`,
          );
        yield* Effect.logInfo(
          `🔹 Scheduler refresh is not due yet; registering slot-aware due work discovery_stage=scheduler_refresh_deep (kind=${entry.kind},key=${entry.key},current_slot=${entry.observedSlot.toString()},due_slot=${entry.dueSlot.toString()},wait_ms=${entry.waitMs.toString()},slot_source=${entry.slotSource},dependency_key=${entry.dependencyKey}).`,
        );
        return dueWorkOutput;
      }
      // A window that has not opened yet is a wait, handled above, and never
      // a failure: after pushing the turn to its successor the node waits
      // here for that shift to end. Only an open window can leave a stale
      // commit target behind.
      const previousShiftCatchUp =
        startTimeMode === "previous-shift-end" &&
        currentStartTime <= targetStartTime;
      if (targetStartTime < validFrom && !previousShiftCatchUp) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Resolved commit end-time falls before the scheduler refresh window",
            cause: `commit_end=${targetStartTime.toString()},scheduler_valid_from=${validFrom.toString()}`,
          }),
        );
      }
      if (!allowSchedulerRefresh) {
        const dueWorkOutput =
          schedulerRefreshRequiredOutsideMutationWorkerDueWork({
            submitTimingSnapshot,
            invalidBeforeSlot,
            invalidHereafterSlot,
            schedulerDependencyKey,
          });
        yield* Effect.logInfo(
          `🔹 Scheduler refresh is required inside the commit mutation worker; deferring so refresh can run before COMMIT_WORKER_ACTIVE and block_commitment lease (kind=${dueWorkOutput.dueWork.kind},key=${dueWorkOutput.dueWork.key},current_slot=${dueWorkOutput.dueWork.observedSlot.toString()},due_slot=${dueWorkOutput.dueWork.dueSlot.toString()},wait_ms=${dueWorkOutput.dueWork.waitMs.toString()},dependency_key=${dueWorkOutput.dueWork.dependencyKey}).`,
        );
        return dueWorkOutput;
      }
      if (submitTiming.status !== "ready" && submitTiming.status !== "wait") {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Scheduler refresh validity planning failed before transaction build",
            cause: `status=${submitTiming.status},current_slot=${"currentSlot" in submitTiming ? submitTiming.currentSlot.toString() : "none"},invalid_before_slot=${invalidBeforeSlot.toString()},invalid_hereafter_slot=${invalidHereafterSlot.toString()}`,
          }),
        );
      }
      const refreshedSchedulerStartTime = yield* Effect.try({
        try: () =>
          resolveRefreshedSchedulerStartTime({
            selection,
            currentSchedulerState,
            validFrom,
            validTo,
            startTimeMode,
          }),
        catch: (cause) =>
          new SDK.StateQueueError({
            message: "Failed to resolve refreshed scheduler start time",
            cause,
          }),
      });
      const refreshedSchedulerDatum = activeSchedulerDatum(
        appointedOperator,
        refreshedSchedulerStartTime,
      );
      if (selection.kind !== "Advance") {
        const registeredWitness = selection.registeredWitnessNode;
        if (registeredWitness.datum.key !== "Empty") {
          const activationKey = registeredWitness.datum.key.Key.key;
          const activationTime = BigInt(
            `0x${activationKey === "" ? "0" : activationKey}`,
          );
          if (validTo >= activationTime) {
            return yield* Effect.fail(
              new SDK.StateQueueError({
                message:
                  "Scheduler rewind window overlaps the next registered operator activation time",
                cause: `valid_to=${validTo.toString()},activation_time=${activationTime.toString()},registered_witness=${outRefLabel(registeredWitness.utxo)}`,
              }),
            );
          }
        }
      }
      const operatorWalletView = yield* readSelectedWalletView(lucid).pipe(
        Effect.mapError(
          (cause) =>
            new SDK.StateQueueError({
              message: `Failed to read the operator wallet view for the scheduler refresh tx: ${cause.message}`,
              cause,
            }),
        ),
      );
      const presetWalletInputs = yield* SDK.requireOperatorWalletInputs(
        operatorWalletView.utxos,
        "scheduler refresh tx",
      );
      yield* Effect.logInfo(
        `🔹 Refreshing scheduler witness datum for commit window via ${selection.kind}, appointing ${appointedOperator === operatorKeyHash ? "self" : "the successor this node pushes the turn to"} (attempt=${(
          refreshCount + 1
        ).toString()}/${SCHEDULER_ALIGNMENT_MAX_REFRESHES_PER_CALL.toString()}, from=${describeSchedulerDatum(schedulerDatum)} to=${describeSchedulerDatum(refreshedSchedulerDatum)}, target=${targetStartTime.toString()}, validTo=${validTo.toString()}).`,
      );
      const refreshTxResult = yield* SDK.buildUnsignedSchedulerRefreshTxProgram(
        {
          lucid,
          scheduler: contracts.scheduler,
          operatorKeyHash,
          presetWalletInputs,
          schedulerInput: currentSchedulerRefInput,
          refreshedDatum: refreshedSchedulerDatum,
          validFrom,
          validTo,
          selection: toSdkSchedulerRefreshWitnessSelection(selection),
          schedulerSpendingScriptRef,
        },
      ).pipe(
        Effect.mapError(
          (cause) =>
            new SDK.StateQueueError({
              message: `Failed to build scheduler refresh transaction through SDK: ${formatUnknownError(
                cause,
                { includeCause: true },
              )}`,
              cause,
            }),
        ),
      );
      const refreshTx = refreshTxResult.tx;

      // The scheduler output that replaced the one this tx spends, whoever's
      // tx landed it: at a shift end the successor may take the same turn
      // itself, and then this tx never lands. The loop replans from it.
      const spentInput = outRefLabel(currentSchedulerRefInput);
      const nextSchedulerUtxo = Effect.tryPromise({
        try: () =>
          lucid.utxosAtWithUnit(
            contracts.scheduler.spendingScriptAddress,
            schedulerWitnessUnit,
          ),
        catch: (cause) =>
          new SDK.StateQueueError({
            message:
              "Failed to fetch scheduler witness UTxOs while waiting for scheduler refresh",
            cause,
          }),
      }).pipe(
        Effect.map((utxos) =>
          [...utxos]
            .sort(compareOutRefs)
            .find((utxo) => outRefLabel(utxo) !== spentInput),
        ),
      );
      // What this refresh is for: the scheduler input it spends replaced by
      // one that gives the turn to the operator it appoints.
      const appointedOnChain = nextSchedulerUtxo.pipe(
        Effect.flatMap((next) =>
          next === undefined
            ? Effect.succeed(false)
            : getSchedulerDatumFromUTxO(next).pipe(
                Effect.map(
                  (datum) =>
                    activeSchedulerState(datum)?.operator === appointedOperator,
                ),
              ),
        ),
        Effect.orElseSucceed(() => false),
      );

      const submitRecoveryOptions: NoInlineSubmitRecoveryOptions &
        IntendedStateProbe = {
        label: "scheduler-refresh",
        slotSnapshot: submitSlotSnapshot,
        requireSlotForBoundedTx: submitSlotSnapshot !== undefined,
        maxPreSubmitWaitMs: SCHEDULER_MAX_PRE_SUBMIT_WAIT_MS,
        inlineWaitPolicy: "defer_positive_wait",
        noInlineSubmitDefer: {
          key: "block_commitment",
          dependencyKey: schedulerDependencyKey,
          invalidationKey: schedulerDependencyKey,
        },
        intendedStateOnChain: () => appointedOnChain,
      };
      const refreshSubmitResult = yield* handleSignSubmitNoConfirmation(
        lucid,
        refreshTx,
        journaledIntent(
          "scheduler_refresh",
          `scheduler:${schedulerRefInput.txHash}#${schedulerRefInput.outputIndex.toString()}`,
          plan,
        ),
        submitRecoveryOptions,
      );
      if (refreshSubmitResult.status === "deferred") {
        return schedulerRefreshDueWorkFromNoInlineSubmitDefer({
          defer: refreshSubmitResult.defer,
          localSubmitSlot: submitSlot,
          nowMs: Date.now(),
        });
      }
      const refreshTxHash = refreshSubmitResult.txHash;
      if (refreshSubmitResult.status === "lost_race") {
        yield* Effect.logInfo(
          `🔹 Scheduler refresh ${refreshTxHash} lost the race to another refresh that already gave the turn to ${appointedOperator}; replanning from it.`,
        );
      } else {
        yield* Effect.logInfo(
          `🔹 Scheduler refresh transaction submitted: ${refreshTxHash}`,
        );
        yield* Effect.raceFirst(
          awaitSubmittedSchedulerTx(lucid, refreshTxHash, "refresh"),
          Effect.either(nextSchedulerUtxo).pipe(
            Effect.repeat({
              until: (next) =>
                next._tag === "Right" && next.right !== undefined,
              schedule: Schedule.spaced(
                SCHEDULER_SUBMISSION_CONFIRMATION_POLL_INTERVAL_MS,
              ),
            }),
            Effect.asVoid,
          ),
        );
      }

      let refreshedSchedulerRefInput: UTxO | undefined;
      let pollCount = 0;
      while (pollCount < SCHEDULER_REFRESH_MAX_POLLS) {
        refreshedSchedulerRefInput = yield* nextSchedulerUtxo;
        if (refreshedSchedulerRefInput !== undefined) {
          break;
        }

        pollCount += 1;
        yield* Effect.sleep(SCHEDULER_REFRESH_POLL_INTERVAL);
      }

      if (refreshedSchedulerRefInput === undefined) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message:
              "Timed out waiting for refreshed scheduler UTxO to appear on-chain",
            cause: refreshTxHash,
          }),
        );
      }

      currentSchedulerRefInput = refreshedSchedulerRefInput;
    }

    return yield* Effect.fail(
      new SDK.StateQueueError({
        message: "Scheduler alignment loop exited unexpectedly",
        cause: `target_commit_end=${targetStartTime.toString()}`,
      }),
    );
  });
