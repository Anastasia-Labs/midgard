import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import type { SubmitSlotSnapshot } from "@al-ft/midgard-core/ogmios-slot";
import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, type UTxO } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  applySubmittedTxToOperatorWalletView,
  availableOperatorWalletUtxos,
  fetchOperatorWalletView,
  type OperatorWalletView,
} from "../../operator-wallet-view.js";
import {
  type IntentJournal,
  type IntentPlan,
  journaledIntent,
} from "../../services/intent-journal.js";
import { planSubmitTiming } from "../../transactions/submit-timing.js";
import {
  handleSignSubmitNoConfirmation,
  type NoInlineSubmitRecoveryOptions,
  type TxSignError,
  type TxSubmitError,
} from "../../transactions/utils.js";
import { compareOutRefs, outRefLabel } from "../../tx-context.js";
import { getSchedulerDatumFromUTxO } from "./scheduler-refresh.fetch-fresh-active-operator-input-for-commit.js";
import {
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
  operatorWalletView?: OperatorWalletView,
  schedulerSpendingScriptRef?: UTxO,
  submitSlotSnapshot?: () => Effect.Effect<SubmitSlotSnapshot, unknown>,
  allowSchedulerRefresh: boolean = true,
): Effect.Effect<
  SchedulerAlignmentResult,
  SDK.StateQueueError | TxSignError | TxSubmitError,
  IntentJournal
> =>
  Effect.gen(function* () {
    const resolveOperatorWalletView = (
      walletView?: OperatorWalletView,
    ): Effect.Effect<OperatorWalletView, SDK.StateQueueError> =>
      walletView === undefined
        ? Effect.tryPromise({
            try: () => fetchOperatorWalletView(lucid),
            catch: (cause) =>
              new SDK.StateQueueError({
                message:
                  "Failed to initialize operator wallet view for scheduler alignment",
                cause,
              }),
          })
        : Effect.succeed(walletView);
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
    let currentOperatorWalletView = operatorWalletView;
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
        return {
          schedulerRefInput: currentSchedulerRefInput,
          operatorWalletView: yield* resolveOperatorWalletView(
            currentOperatorWalletView,
          ),
        };
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
      const selection = yield* Effect.try({
        try: () =>
          resolveSchedulerRefreshWitnessSelection({
            currentOperator,
            targetOperator: operatorKeyHash,
            activeNodes,
            registeredNodes,
            allowGenesisRewind,
          }),
        catch: (cause) =>
          new SDK.StateQueueError({
            message:
              "Current operator is not eligible to advance or rewind the scheduler for this commit window",
            cause,
          }),
      });
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
            );
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
        targetOperator: operatorKeyHash,
        selectionKind: selection.kind,
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
        yield* Effect.logInfo(
          `🔹 Scheduler refresh is not due yet; registering slot-aware due work discovery_stage=scheduler_refresh_deep (kind=${entry.kind},key=${entry.key},current_slot=${entry.observedSlot.toString()},due_slot=${entry.dueSlot.toString()},wait_ms=${entry.waitMs.toString()},slot_source=${entry.slotSource},dependency_key=${entry.dependencyKey}).`,
        );
        return dueWorkOutput;
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
        operatorKeyHash,
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
      const flowOperatorWalletView = yield* resolveOperatorWalletView(
        currentOperatorWalletView,
      );
      const presetWalletInputs = yield* SDK.requireOperatorWalletInputs(
        availableOperatorWalletUtxos(flowOperatorWalletView),
        "scheduler refresh tx",
      );
      yield* Effect.logInfo(
        `🔹 Refreshing scheduler witness datum for commit window via ${selection.kind} (attempt=${(
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

      const submitRecoveryOptions: NoInlineSubmitRecoveryOptions = {
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
      const refreshedOperatorWalletView = applySubmittedTxToOperatorWalletView(
        flowOperatorWalletView,
        refreshTx.toTransaction(),
        refreshTxHash,
      );
      yield* Effect.logInfo(
        `🔹 Scheduler refresh transaction submitted: ${refreshTxHash}`,
      );
      yield* Effect.logInfo(
        `🔹 Scheduler refresh tx updated operator wallet view: available_utxos=${refreshedOperatorWalletView.knownUtxos.length.toString()},consumed_outrefs=${refreshedOperatorWalletView.consumedOutRefs.length.toString()}.`,
      );
      yield* awaitSubmittedSchedulerTx(lucid, refreshTxHash, "refresh");

      let refreshedSchedulerRefInput: UTxO | undefined;
      let pollCount = 0;
      while (pollCount < SCHEDULER_REFRESH_MAX_POLLS) {
        const schedulerWitnessUtxos = yield* Effect.tryPromise({
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
        });
        for (const utxo of [...schedulerWitnessUtxos].sort(compareOutRefs)) {
          const utxoDatumEither = yield* Effect.either(
            getSchedulerDatumFromUTxO(utxo),
          );
          if (utxoDatumEither._tag === "Left") {
            continue;
          }
          const active = activeSchedulerState(utxoDatumEither.right);
          if (
            active?.operator === operatorKeyHash &&
            active.startTime === refreshedSchedulerStartTime
          ) {
            refreshedSchedulerRefInput = utxo;
            break;
          }
        }
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
      currentOperatorWalletView = refreshedOperatorWalletView;
    }

    return yield* Effect.fail(
      new SDK.StateQueueError({
        message: "Scheduler alignment loop exited unexpectedly",
        cause: `target_commit_end=${targetStartTime.toString()}`,
      }),
    );
  });
