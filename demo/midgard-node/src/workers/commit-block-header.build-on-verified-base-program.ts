import { maxDaPayloadInnerBytes } from "@al-ft/midgard-core/da-payload-sizing";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option } from "effect";

import { readDaHardeningConfig } from "../da/hardening-config.js";
import {
  CommitBuildCalibrationDB,
  MempoolDB,
  MpfEngineStateDB,
  ProcessedMempoolDB,
} from "../database/index.js";
import {
  Columns as TxColumns,
  type EntryWithTimeStamp,
} from "../database/utils/tx.js";
import { unixTimeToSlotForConfig } from "../lucid-time.js";
import {
  configureCommitMpfRuntime,
  MidgardMpf,
  type NativeMpfBuildContext,
  processMpfs,
} from "../mpf/index.js";
import { assertFollowerWrite } from "../services/follower-write-gate.js";
import {
  commitEventHorizon,
  HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
} from "../services/history-commit-window.js";
import {
  ContractDeploymentIdentity,
  Database,
  Lucid,
  MidgardContracts,
  NativeMpfWorkerPortClient,
  NodeConfig,
} from "../services/index.js";
import type { IntentJournal } from "../services/intent-journal.js";
import { outRefLabel } from "../tx-context.js";
import {
  alignCommitMpfsToBase,
  MEMPOOL_LEDGER_REVERTED_NOTICE,
  type NotifyCommitWorkerParent,
  shouldShortCircuitIdleCommitAttempt,
  workerPreIngestionDueWorkOutputFromPlan,
} from "./commit-block-header.commit-explicit-block-header-program.js";
import { notifyCommitPlanningSelection } from "./commit-block-header.notify-planning-selection.js";
import {
  type CommitLucidFactory,
  CommitWorkerInvariantError,
  shouldHydrateCommitBaseEntries,
} from "./commit-block-header.pending-user-event-counts-up-to.js";
import { planAdditiveCommitDa } from "./commit-block-header.plan-additive-da.js";
import { planHistoryCommitEventWindow } from "./commit-block-header.plan-event-window.js";
import { recordSuccessfulBuildCalibration as recordBuildCalibration } from "./commit-block-header.record-successful-build-calibration.js";
import { resolveCommitBaseLedgerEntries } from "./commit-block-header.resolve-commit-base-ledger-entries.js";
import { verifyAdditiveCommitDaFrame } from "./commit-block-header.verify-additive-da.js";
import type { CommitEviction } from "./commit-block-header/commit-da-oversize-eviction.js";
import * as DaPrefix from "./commit-block-header/commit-da-prefix-search.js";
import { getLatestBlockDatumEndTime } from "./commit-block-header/state-queue.js";
import {
  deferProcessedCommitPayloadUntilConfirmation,
  recoverLocalFinalizationAgainstConfirmedBlock,
  submitDepositOnlyCommit,
  submitTxBackedCommit,
} from "./commit-block-header/submission.js";
import { COMMIT_PLANNING_STARTED_NOTICE } from "./commit-planning-notice.js";
import {
  type CommitCandidateRoots,
  deserializeStateQueueUTxO,
  WorkerInput,
  WorkerOutput,
} from "./utils/commit-block-header.js";
import {
  COMMIT_DA_FRAME_FITS_NOTICE,
  nothingToCommitWithNoWork,
} from "./utils/commit-block-planner.commit-da-frame-notice.js";
import {
  buildCommitTxCandidateSelection,
  calibratedCommitBuildMsPerTx,
  type CommitSchedulerStateQueueEvidence,
  type CurrentOperatorSchedulerWindow,
  DEFAULT_COMMIT_BATCH_BUDGET_LIMITS,
  establishEndTimeFromTxRequests,
  planCommitBatchBudgets,
  planSchedulerAwareCommitSelection,
  schedulerAwareCommitWindowBudgets,
  selectCommitTxCandidates,
  shouldDeferCommitSubmission,
  shouldSkipIdleCommitBehindUnmergedTail,
} from "./utils/commit-block-planner.js";
import {
  COMMIT_MINIMUM_FUTURE_BUFFER_MS,
  resolveCommitEndTimeFit,
  resolveLatestFeasibleCommitEndTime,
} from "./utils/commit-end-time.js";
import {
  resolveCurrentOperatorSchedulerWindow,
  resolveEarliestCommitSchedulerDueWorkPlan,
} from "./utils/scheduler-refresh.js";

export const buildOnVerifiedCommitBaseProgram = (
  workerInput: WorkerInput,
  transactionsMpf: MidgardMpf,
  notifyParent: NotifyCommitWorkerParent | undefined,
  nativeMpfClient: NativeMpfWorkerPortClient | undefined,
  nativeMpfState:
    | {
        context?: NativeMpfBuildContext;
        preserve: boolean;
      }
    | undefined,
  acquireCommitLucid: CommitLucidFactory,
  /** Normal transactions this build rejects before validation. */
  commitEvictions?: ReadonlyMap<string, CommitEviction>,
): Effect.Effect<
  WorkerOutput,
  unknown,
  | MidgardContracts
  | ContractDeploymentIdentity
  | Database
  | NodeConfig
  | IntentJournal
> =>
  Effect.gen(function* () {
    let acquiredCommitLucid: Lucid | undefined;
    const acquireCommitLucidOnce = Effect.suspend(() =>
      acquiredCommitLucid === undefined
        ? acquireCommitLucid().pipe(
            Effect.tap((lucid) =>
              Effect.sync(() => {
                acquiredCommitLucid = lucid;
              }),
            ),
          )
        : Effect.succeed(acquiredCommitLucid),
    );
    yield* assertFollowerWrite(workerInput.history);
    if (notifyParent !== undefined)
      yield* notifyParent(COMMIT_PLANNING_STARTED_NOTICE);
    const workerStartedAtMs = Date.now();
    const nodeConfig = yield* NodeConfig;
    const deploymentIdentity = yield* ContractDeploymentIdentity;
    if (deploymentIdentity.deploymentMarker === undefined) {
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message:
            "V1 commit production requires a verified final deployment marker",
        }),
      );
    }
    const deploymentMarker = deploymentIdentity.deploymentMarker;
    yield* configureCommitMpfRuntime(nodeConfig);
    if (nativeMpfClient === undefined || workerInput.nativeMpf === undefined) {
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message:
            "Architecture G commit worker is missing its main-owner port/root",
        }),
      );
    }
    yield* MpfEngineStateDB.assertLedgerAuditHealthy;
    yield* Effect.logInfo(
      `pipeline_trace phase=commit_worker_started at_ms=${workerStartedAtMs.toString()}`,
    );
    const retrievedMempoolTxs: EntryWithTimeStamp[] = [];
    let mempoolCursor: MempoolDB.MempoolCursor | undefined;
    while (retrievedMempoolTxs.length < nodeConfig.MEMPOOL_RETRIEVE_PAGE_SIZE) {
      const page = yield* MempoolDB.retrievePage({
        after: mempoolCursor,
        limit: nodeConfig.MEMPOOL_RETRIEVE_PAGE_SIZE,
        upTo: new Date(workerStartedAtMs),
      });
      retrievedMempoolTxs.push(...page.entries);
      if (page.nextCursor === null) break;
      mempoolCursor = page.nextCursor;
    }
    if (retrievedMempoolTxs.length > nodeConfig.MEMPOOL_RETRIEVE_PAGE_SIZE) {
      retrievedMempoolTxs.length = nodeConfig.MEMPOOL_RETRIEVE_PAGE_SIZE;
    }
    const currentBlockStartTime = new Date(
      workerInput.data.currentBlockStartTimeMs,
    );
    const mempoolTxs = retrievedMempoolTxs;
    const processedPendingTxs = yield* ProcessedMempoolDB.retrieve;
    const rawCandidateSelection = selectCommitTxCandidates({
      mempoolTxs,
      processedMempoolTxs: processedPendingTxs,
    });
    const availableConfirmedBlock = workerInput.data.availableConfirmedBlock;
    const availableLocalFinalizationBlock =
      workerInput.data.availableLocalFinalizationBlock;
    const hasAvailableConfirmedBlock = availableConfirmedBlock !== "";
    const hasAvailableLocalFinalizationBlock =
      availableLocalFinalizationBlock !== "";
    const canBuildOnConfirmedBlock =
      hasAvailableConfirmedBlock && !workerInput.data.localFinalizationPending;
    let latestBlockForSchedulerPlanning: SDK.StateQueueUTxO | undefined;
    let latestEndTimeMsForSchedulerPlanning: number | undefined;
    if (canBuildOnConfirmedBlock) {
      latestBlockForSchedulerPlanning = yield* deserializeStateQueueUTxO(
        availableConfirmedBlock,
      );
      latestEndTimeMsForSchedulerPlanning = Number(
        (yield* getLatestBlockDatumEndTime(
          latestBlockForSchedulerPlanning.datum,
        )).getTime(),
      );
      const stateQueueEvidence: CommitSchedulerStateQueueEvidence = {
        tailCommitBaseOutRef: outRefLabel(latestBlockForSchedulerPlanning.utxo),
        tailBlockEndTimeMs: latestEndTimeMsForSchedulerPlanning,
        stateQueueHasUnmergedTail:
          workerInput.data.stateQueueHasUnmergedTail ?? false,
      };
      const contracts = yield* MidgardContracts;
      const lucid = yield* acquireCommitLucidOnce;
      yield* lucid.switchToOperatorsMainWallet;
      const preIngestionPlan = yield* Effect.either(
        resolveEarliestCommitSchedulerDueWorkPlan({
          lucid: lucid.api,
          contracts,
          submitSlotSnapshot: lucid.submitSlotSnapshot,
          stateQueueEvidence,
          localFinalizationPending: workerInput.data.localFinalizationPending,
          callerLabel: "commit-scheduler-worker-pre-ingestion",
          discoveryStage: "worker_pre_ingestion",
        }),
      );
      if (preIngestionPlan._tag === "Right") {
        const output = workerPreIngestionDueWorkOutputFromPlan(
          preIngestionPlan.right,
        );
        if (output !== undefined) {
          yield* Effect.logInfo(
            `🔹 Registered slot-aware due work before commit ingestion barriers discovery_stage=worker_pre_ingestion (kind=${output.dueWork.kind},key=${output.dueWork.key},current_slot=${output.dueWork.observedSlot.toString()},due_slot=${output.dueWork.dueSlot.toString()},due_at_ms=${output.dueWork.dueAtMs.toString()},wait_ms=${output.dueWork.waitMs.toString()},slot_source=${output.dueWork.slotSource},dependency_key=${output.dueWork.dependencyKey}).`,
          );
          return output;
        }
      } else {
        yield* Effect.logWarning(
          `🔹 Worker pre-ingestion scheduler due-work preflight failed; continuing to full planner: ${String(preIngestionPlan.left)}`,
        );
      }
    }
    // The event horizon (plan §8.1): min(the commit anchor's cap, the
    // earliest forced order not yet rebuilt), the anchor being the follower
    // block d (the profile's commit-event depth) below the permit's view.
    // No view yet, or no anchor block: no end time is safe; the commit holds.
    const eventHorizon = yield* commitEventHorizon({
      view: workerInput.history?.view,
      depth: nodeConfig.COMMIT_EVENT_DEPTH,
      slotToUnixTime: Effect.map(
        acquireCommitLucidOnce,
        (lucid) => lucid.api.slotToUnixTime,
      ),
    });
    if (eventHorizon === null) {
      yield* Effect.logInfo(
        "🔹 The L1 follower has no ingested view at its current chain, or no block the commit-event depth below it; holding the commit.",
      );
      return { type: "NothingToCommitOutput" } satisfies WorkerOutput;
    }
    const { horizonMs: eventHorizonMs, anchor: commitAnchor } = eventHorizon;
    const depositIngestionBarrierTime = new Date(eventHorizonMs);
    const withdrawalIngestionBarrierTime = depositIngestionBarrierTime;
    const userEventOnlyEndTime = depositIngestionBarrierTime;

    let currentSchedulerWindow: CurrentOperatorSchedulerWindow | undefined;
    let currentWindowCommitEndTimeFit:
      | ReturnType<typeof resolveCommitEndTimeFit>
      | undefined;
    const schedulerPlanningNowMs = Date.now();
    if (canBuildOnConfirmedBlock) {
      const contracts = yield* MidgardContracts;
      const lucid = yield* acquireCommitLucidOnce;
      yield* lucid.switchToOperatorsMainWallet;
      currentSchedulerWindow = yield* resolveCurrentOperatorSchedulerWindow(
        lucid.api,
        contracts,
      );
      if (currentSchedulerWindow !== undefined) {
        const latestEndTimeMs = Number(latestEndTimeMsForSchedulerPlanning);
        const txBackedCandidateEndTime = establishEndTimeFromTxRequests(
          rawCandidateSelection.candidateTxs,
        );
        const candidateEndTimeMs = Option.isSome(txBackedCandidateEndTime)
          ? txBackedCandidateEndTime.value.getTime()
          : userEventOnlyEndTime.getTime();
        // A source-owned commit's history horizon caps its end; it is not a
        // floor. The current window fits whenever some end inside it clears
        // the monotonic and current-time floors. Selected L2 transactions are
        // timestamped no later than the worker start, below the current-time
        // floor.
        currentWindowCommitEndTimeFit =
          workerInput.history === undefined
            ? resolveCommitEndTimeFit({
                lucid: lucid.api,
                latestEndTime: latestEndTimeMs,
                candidateEndTime: candidateEndTimeMs,
                nowMs: schedulerPlanningNowMs,
                minimumFutureBufferMs: COMMIT_MINIMUM_FUTURE_BUFFER_MS,
                maximumEndTimeMs: currentSchedulerWindow.endTimeMs,
              })
            : resolveLatestFeasibleCommitEndTime({
                lucid: lucid.api,
                latestEndTime: latestEndTimeMs,
                nowMs: schedulerPlanningNowMs,
                minimumFutureBufferMs: HISTORY_COMMIT_MINIMUM_FUTURE_BUFFER_MS,
                maximumEndTimeMs: currentSchedulerWindow.endTimeMs,
              });
      }
    }
    const schedulerAwareCommitSelection = planSchedulerAwareCommitSelection({
      candidateSelection: rawCandidateSelection,
      userEventOnlyEndTime,
      currentSchedulerWindow,
      currentBlockStartTimeMs: currentBlockStartTime.getTime(),
      nowMs: schedulerPlanningNowMs,
      ...schedulerAwareCommitWindowBudgets(workerInput.history !== undefined),
      currentWindowCommitEndTimeFit,
    });
    if (
      schedulerAwareCommitSelection.status === "using_current_scheduler_window"
    ) {
      yield* Effect.logInfo(
        `🔹 Scheduler-aware commit planner using current scheduler window (${schedulerAwareCommitSelection.reason},user_event_end_time=${schedulerAwareCommitSelection.userEventOnlyEndTime.toISOString()}).`,
      );
    } else if (
      schedulerAwareCommitSelection.status ===
        "current_scheduler_budget_too_low" ||
      schedulerAwareCommitSelection.status ===
        "current_scheduler_end_time_floor_exceeds_window"
    ) {
      yield* Effect.logInfo(
        `🔹 Scheduler-aware commit planner will not cap to current scheduler window (${schedulerAwareCommitSelection.reason}).`,
      );
    }
    const calibration =
      nodeConfig.COMMIT_BUILD_COST_MODEL === "ewma"
        ? yield* CommitBuildCalibrationDB.retrieve
        : undefined;
    const estimatedCommitBuildMsPerTx =
      calibration === undefined
        ? DEFAULT_COMMIT_BATCH_BUDGET_LIMITS.estimatedCommitBuildMsPerTx
        : calibratedCommitBuildMsPerTx({
            msPerTxEwma: calibration.msPerTxEwma,
            safetyFactor: nodeConfig.COMMIT_BUILD_EWMA_SAFETY_FACTOR,
          });
    const daFrameInnerLimit = maxDaPayloadInnerBytes(
      readDaHardeningConfig().envelopeMode,
    );
    const commitBatchBudgetLimits = {
      ...DEFAULT_COMMIT_BATCH_BUDGET_LIMITS,
      maxL2TxCount: nodeConfig.COMMIT_MAX_L2_TX_COUNT,
      maxLedgerOpCount: nodeConfig.COMMIT_MAX_LEDGER_OP_COUNT,
      maxTransitionStepCount: nodeConfig.COMMIT_MAX_TRANSITION_STEP_COUNT,
      blockExUnitsBudget: nodeConfig.BLOCK_EX_UNITS_BUDGET,
      maxDaPayloadBytes: daFrameInnerLimit,
      estimatedCommitBuildMsPerTx,
    };
    const budgetedCommitSelection = planCommitBatchBudgets({
      candidateSelection: schedulerAwareCommitSelection.candidateSelection,
      limits: commitBatchBudgetLimits,
    });
    const batchSelectedAtMs = Date.now();
    if (
      budgetedCommitSelection.plan.selectedTxCount > 0 ||
      budgetedCommitSelection.prunedTxCount > 0
    ) {
      yield* Effect.logInfo(
        `🔹 Commit batch planner selected tx_count=${budgetedCommitSelection.plan.selectedTxCount.toString()}, tx_bytes=${budgetedCommitSelection.plan.selectedTxBytes.toString()}, estimated_da_payload_bytes=${budgetedCommitSelection.plan.estimatedDaPayloadBytes.toString()}, estimated_commit_build_ms=${budgetedCommitSelection.plan.estimatedCommitBuildMs.toString()}, stop_reason=${budgetedCommitSelection.plan.stopReason}, pruned_tx_count=${budgetedCommitSelection.prunedTxCount.toString()}.`,
      );
    }
    let candidateSelection = budgetedCommitSelection.candidateSelection;
    yield* Effect.logInfo(
      `pipeline_trace phase=batch_selected at_ms=${batchSelectedAtMs.toString()} elapsed_ms=${Math.max(0, batchSelectedAtMs - workerStartedAtMs).toString()} selected_tx_count=${budgetedCommitSelection.plan.selectedTxCount.toString()} selected_tx_bytes=${budgetedCommitSelection.plan.selectedTxBytes.toString()} stop_reason=${budgetedCommitSelection.plan.stopReason}`,
    );
    const effectiveUserEventOnlyEndTime =
      schedulerAwareCommitSelection.userEventOnlyEndTime;
    if (
      shouldDeferCommitSubmission({
        localFinalizationPending: workerInput.data.localFinalizationPending,
        hasAvailableConfirmedBlock: hasAvailableLocalFinalizationBlock,
      })
    ) {
      yield* Effect.logInfo(
        "🔹 Local finalization pending and no recoverable confirmed block is available yet; deferring new submission.",
      );
      return {
        type: "NothingToCommitOutput",
      } satisfies WorkerOutput;
    }

    const recoverableLocalFinalizationBlock =
      workerInput.data.localFinalizationPending &&
      hasAvailableLocalFinalizationBlock
        ? availableLocalFinalizationBlock
        : undefined;
    if (recoverableLocalFinalizationBlock !== undefined) {
      const recoverableConfirmedBlock = yield* deserializeStateQueueUTxO(
        recoverableLocalFinalizationBlock,
      );
      const lucid = yield* acquireCommitLucidOnce;
      return yield* recoverLocalFinalizationAgainstConfirmedBlock({
        latestBlock: recoverableConfirmedBlock,
        transactionsMpf,
        processedMempoolTxs: [],
        mempoolTxHashes: [],
        workerInput,
        sizeOfProcessedTxs: 0,
        consensusProfile: deploymentIdentity.consensusProfile,
      }).pipe(Effect.provideService(Lucid, lucid));
    }

    const lucidForEventPlan = yield* acquireCommitLucidOnce;
    const limits = deploymentIdentity.consensusProfile.limits;
    const eventPlan = yield* planHistoryCommitEventWindow({
      latestEndTime:
        latestEndTimeMsForSchedulerPlanning ?? currentBlockStartTime.getTime(),
      nowMs: schedulerPlanningNowMs,
      horizonMs: Math.min(
        eventHorizonMs,
        effectiveUserEventOnlyEndTime.getTime(),
      ),
      schedulerWindowEndTimeMs: schedulerAwareCommitSelection.blockEndTimeCapMs,
      limits: {
        deposits: limits.maxDepositCount,
        forcedTransactions: limits.maxForcedTransactionCount,
        withdrawals: limits.maxWithdrawalCount,
        totalEvents: limits.maxTotalEventCount,
      },
    }).pipe(Effect.provideService(Lucid, lucidForEventPlan));
    if (eventPlan.kind === "clock_hold") {
      yield* Effect.logInfo(
        `Commit clock cannot clear the selected scheduler or append fence: ${eventPlan.clockFit.reason}`,
      );
      return { type: "NothingToCommitOutput" } satisfies WorkerOutput;
    }
    if (eventPlan.window.mode === "instant_exceeds_cap")
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message: `catch_up_instant_exceeds_cap inclusion_time_ms=${eventPlan.window.inclusionTimeMs}`,
        }),
      );
    if (eventPlan.window.mode === "empty")
      return yield* nothingToCommitWithNoWork(notifyParent);
    const blockTimeMs = eventPlan.blockTimeMs;
    const blockEndTimeCapMs = eventPlan.clockFit.maximumEndTimeMs;
    let fixedHistoryEndTime = new Date(eventPlan.window.endTimeMs);
    if (eventPlan.window.mode === "catch_up")
      candidateSelection = buildCommitTxCandidateSelection(
        [],
        candidateSelection.sourceTable,
      );
    const pendingUserEventCounts = eventPlan.window.counts;
    const pendingUserEventCount =
      pendingUserEventCounts.deposits +
      pendingUserEventCounts.forcedTransactions +
      pendingUserEventCounts.withdrawals;
    if (
      shouldSkipIdleCommitBehindUnmergedTail({
        localFinalizationPending: workerInput.data.localFinalizationPending,
        stateQueueHasUnmergedTail:
          workerInput.data.stateQueueHasUnmergedTail ?? false,
        mempoolTxCount: mempoolTxs.length,
        processedTxCount: processedPendingTxs.length,
        pendingUserEventCount,
      })
    ) {
      yield* Effect.logInfo(
        "🔹 State queue has an unmerged tail and no pending tx/user-event work; waiting for merge before the next commit attempt.",
      );
      return yield* nothingToCommitWithNoWork(notifyParent);
    }

    if (
      shouldShortCircuitIdleCommitAttempt({
        candidateTxCount: candidateSelection.candidateTxs.length,
        processedPendingTxCount: processedPendingTxs.length,
        pendingUserEventCount,
        localFinalizationPending: workerInput.data.localFinalizationPending,
      })
    ) {
      yield* Effect.logInfo(
        "🔹 No pending tx/user-event work for block commitment; skipping commit base hydration.",
      );
      return yield* nothingToCommitWithNoWork(notifyParent);
    }

    const baseHydrationStartedAtMs = Date.now();
    const commitBase = yield* resolveCommitBaseLedgerEntries({
      availableConfirmedBlock,
      nativeMpfRoot: workerInput.nativeMpf.durableRoot,
      requireEntries: shouldHydrateCommitBaseEntries({
        payloadRootCheck: nodeConfig.MPF_PAYLOAD_ROOT_CHECK,
        recordCorpus: nodeConfig.MPF_RECORD_CORPUS,
        candidateTxCount: candidateSelection.candidateTxs.length,
        pendingForcedTransactionCount:
          pendingUserEventCounts.forcedTransactions,
        pendingWithdrawalCount: pendingUserEventCounts.withdrawals,
      }),
    });
    const initialLedgerEntries = yield* alignCommitMpfsToBase({
      nativeMpfRoot: workerInput.nativeMpf.durableRoot,
      transactionsMpf,
      base: commitBase,
    });
    const nativeMpfOwnerBinarySha256 = workerInput.nativeMpf.ownerBinarySha256;
    const forkNativeMpfContext = Effect.gen(function* () {
      const context: NativeMpfBuildContext = {
        client: nativeMpfClient,
        handle: yield* Effect.tryPromise({
          try: () => nativeMpfClient.fork(commitBase.root),
          catch: (cause) =>
            new CommitWorkerInvariantError({
              message: `Architecture G fork failed: ${String(cause)}`,
            }),
        }),
        ownerBinarySha256: nativeMpfOwnerBinarySha256,
      };
      if (nativeMpfState !== undefined) {
        nativeMpfState.context = context;
      }
      return context;
    });
    const nativeMpfContext = yield* forkNativeMpfContext;
    yield* Effect.logInfo(
      `🔹 Commit base hydration phase completed duration_ms=${Math.max(
        0,
        Date.now() - baseHydrationStartedAtMs,
      ).toString()},source=${commitBase.source},base_entry_count=${initialLedgerEntries.length.toString()}`,
    );
    let mpfProcessingStartedAtMs = Date.now();
    // Canonical V1 is the only consensus profile this node can carry: the
    // deployment manifest parser and the derived-contract path both reject
    // anything that is not exactly MIDGARD_CONSENSUS_PROFILE_V1, so forced
    // proof validation is unconditional and its slot mapping is required.
    // The mapping is plain node-selected data, which keeps candidate
    // construction provider-free.
    const proofValidationSlotConfig =
      workerInput.data.forcedValidationSlotConfig;
    if (proofValidationSlotConfig === undefined) {
      return yield* Effect.fail(
        new Error(
          "Canonical V1 commitment input is missing its node-selected slot configuration",
        ),
      );
    }
    const daFramePlan = planCommitBatchBudgets({
      candidateSelection,
      limits: commitBatchBudgetLimits,
    });
    yield* DaPrefix.logDaPrefixPreselection(daFramePlan);
    const additivePlan = yield* planAdditiveCommitDa({
      window: eventPlan.window,
      base: initialLedgerEntries,
      candidates: daFramePlan.candidateSelection.candidateTxs,
      profile: deploymentIdentity.consensusProfile,
      maxInnerBytes: daFrameInnerLimit,
    });
    if (additivePlan.kind === "frame_hold") {
      if (notifyParent !== undefined)
        yield* notifyParent({
          type: "CommitDaFrameNotice",
          status: "events_overflow",
          detail: additivePlan.reason,
        });
      yield* Effect.logError(additivePlan.reason).pipe(
        Effect.annotateLogs({ alarm: "catch_up_da_frame_hold" }),
      );
      return yield* Effect.fail(
        new CommitWorkerInvariantError({ message: additivePlan.reason }),
      );
    }
    yield* notifyCommitPlanningSelection({
      notify: notifyParent,
      workerInput,
      parent: latestBlockForSchedulerPlanning,
      identity: deploymentIdentity,
      baseRoot: commitBase.root,
      startTimeMs:
        latestEndTimeMsForSchedulerPlanning ?? currentBlockStartTime.getTime(),
      blockTimeMs,
      canonicalHorizonMs: eventHorizonMs,
      eligibleCutoffMs: Math.min(
        eventHorizonMs,
        effectiveUserEventOnlyEndTime.getTime(),
        blockTimeMs,
      ),
      groups: eventPlan.groups,
      plan: additivePlan,
    });
    fixedHistoryEndTime = new Date(additivePlan.window.endTimeMs);
    candidateSelection = buildCommitTxCandidateSelection(
      daFramePlan.candidateSelection.candidateTxs.slice(
        0,
        additivePlan.candidateCount,
      ),
      daFramePlan.candidateSelection.sourceTable,
    );
    // Exactly one native build. No failed frame can restart a descending replay
    // against this fixed commit clock; an underestimate is a visible bound defect.
    mpfProcessingStartedAtMs = Date.now();
    const processed = yield* processMpfs(
      transactionsMpf,
      candidateSelection.candidateTxs,
      {
        fixedBlockEndTime: fixedHistoryEndTime,
        fixedBlockTime: new Date(blockTimeMs),
        currentBlockStartTime: canBuildOnConfirmedBlock
          ? currentBlockStartTime
          : undefined,
        processedOnlyEndTime:
          candidateSelection.sourceTable === ProcessedMempoolDB.tableName
            ? candidateSelection.candidateTxs[0]?.[TxColumns.TIMESTAMPTZ]
            : undefined,
        depositVisibilityBarrierTime: canBuildOnConfirmedBlock
          ? depositIngestionBarrierTime
          : undefined,
        withdrawalVisibilityBarrierTime: canBuildOnConfirmedBlock
          ? withdrawalIngestionBarrierTime
          : undefined,
        txOrderVisibilityBarrierTime: canBuildOnConfirmedBlock
          ? depositIngestionBarrierTime
          : undefined,
        depositOnlyEndTime: canBuildOnConfirmedBlock
          ? effectiveUserEventOnlyEndTime
          : undefined,
        initialLedgerEntries,
        consensusProfile: deploymentIdentity.consensusProfile,
        forcedValidation: {
          expectedNetworkId: nodeConfig.NETWORK === "Mainnet" ? 1n : 0n,
          minFeeA: nodeConfig.MIN_FEE_A,
          minFeeB: nodeConfig.MIN_FEE_B,
          bucketConcurrency: nodeConfig.VALIDATION_G4_BUCKET_CONCURRENCY,
          slotForUnixTime: (unixTimeMs) =>
            BigInt(
              unixTimeToSlotForConfig(unixTimeMs, proofValidationSlotConfig),
            ),
          l1SlotConfig: proofValidationSlotConfig,
        },
        selectedBaseUtxoRoot: commitBase.root,
        payloadRootCheck: nodeConfig.MPF_PAYLOAD_ROOT_CHECK,
        recordCorpusPath: nodeConfig.MPF_RECORD_CORPUS,
        onMempoolLedgerReverted: notifyParent?.(MEMPOOL_LEDGER_REVERTED_NOTICE),
        onCommitBuildAlarm: (alarm, stage) =>
          notifyParent?.({ type: "CommitBuildAlarmNotice", alarm, stage }) ??
          Effect.void,
        commitEvictions,
        nativeMpf: nativeMpfContext,
      },
    );
    yield* verifyAdditiveCommitDaFrame({
      processed,
      baseRoot: commitBase.root,
      profile: deploymentIdentity.consensusProfile,
      nodeConfig,
      additivePlan,
      latestBlockForSchedulerPlanning,
      lucid: lucidForEventPlan,
      fixedHistoryEndTime,
      blockTimeMs,
      proofValidationSlotConfig,
      daFrameInnerLimit,
      notifyParent,
    });
    const mpfProcessingFinishedAtMs = yield* DaPrefix.logMpfProcessingFinished(
      mpfProcessingStartedAtMs,
    );

    const {
      utxoRoot,
      rawTxRoot,
      txRoot,
      transitionTraceRoot,
      eventToStepRoot,
      validationTracesRoot,
      transitionTraceMembers,
      eventToStepMembers,
      validationTraceMembers,
      transitionStepCount,
      validationTraceCount,
      ledgerDelta,
      rejectedMempoolTxsCount,
      includedDepositEntriesCount,
      includedDepositEntries,
      includedDepositEventIds,
      includedForcedTransactionEntriesCount,
      includedForcedTransactionEntries,
      includedForcedTransactionEventIds,
      includedWithdrawalEntriesCount,
      includedWithdrawalEntries,
      includedWithdrawalEventIds,
      nativeMpfReplay,
      nativeMpfHandle,
    } = processed;

    const attachNativeMpfPromotion = (output: WorkerOutput): WorkerOutput => {
      if (
        nativeMpfHandle === undefined ||
        (output.type !== "SubmittedAwaitingConfirmationOutput" &&
          output.type !== "SubmittedAwaitingLocalFinalizationOutput" &&
          output.type !== "SuccessfulSubmissionOutput")
      ) {
        return output;
      }
      if (nativeMpfState !== undefined) nativeMpfState.preserve = true;
      return {
        ...output,
        nativeMpfPromotion: { handle: nativeMpfHandle },
      };
    };

    const processedMempoolTxs = processed.processedMempoolTxs;
    const mempoolTxHashes = processed.mempoolTxHashes;
    const sizeOfProcessedTxs = processed.sizeOfProcessedTxs;
    const mempoolTxSourceTable =
      candidateSelection.sourceTable === "processed_mempool"
        ? ProcessedMempoolDB.tableName
        : candidateSelection.sourceTable === "mempool"
          ? MempoolDB.tableName
          : "none";
    const recordSuccessfulBuildCalibration = (output: WorkerOutput) =>
      recordBuildCalibration({
        calibration,
        processedTxCount: processedMempoolTxs.length,
        measuredBuildMs: Math.max(
          0,
          mpfProcessingFinishedAtMs - mpfProcessingStartedAtMs,
        ),
        alpha: nodeConfig.COMMIT_BUILD_EWMA_ALPHA,
        output,
      });
    if (candidateSelection.sourceTable === "processed_mempool") {
      yield* Effect.logWarning(
        `🔹 Prioritizing ${processedPendingTxs.length.toString()} deferred processed tx(s) before newer mempool tx(s).`,
      );
    }

    if (rejectedMempoolTxsCount > 0) {
      yield* Effect.logWarning(
        `Rejected ${rejectedMempoolTxsCount} malformed tx(s) during commitment preprocessing.`,
      );
    }
    if (includedDepositEntriesCount > 0) {
      yield* Effect.logInfo(
        `🔹 Commitment pre-state includes ${includedDepositEntriesCount} due deposit UTxO(s).`,
      );
    }
    if (includedWithdrawalEntriesCount > 0) {
      yield* Effect.logInfo(
        `🔹 Commitment pre-state includes ${includedWithdrawalEntriesCount} due withdrawal event(s).`,
      );
    }
    if (includedForcedTransactionEntriesCount > 0) {
      yield* Effect.logInfo(
        `🔹 Commitment source set includes ${includedForcedTransactionEntriesCount} due tx-order event(s).`,
      );
    }

    const mempoolTxsCount = processedMempoolTxs.length;
    const optEndTime = establishEndTimeFromTxRequests(processedMempoolTxs);
    const submissionContracts = yield* MidgardContracts;
    const submissionLucid = yield* acquireCommitLucidOnce;
    if (availableConfirmedBlock === "") {
      // Confirmation is pending; preserve this validated ordinary payload
      // for the next attempt.
      const candidate = {
        endTimeMs: (
          fixedHistoryEndTime ??
          (Option.isSome(optEndTime)
            ? optEndTime.value
            : effectiveUserEventOnlyEndTime)
        ).getTime(),
        roots: {
          utxos: utxoRoot,
          rawTransactions: rawTxRoot,
          transactions: txRoot,
          transitionTrace: transitionTraceRoot,
          eventToStep: eventToStepRoot,
        } satisfies CommitCandidateRoots,
      };
      if (mempoolTxSourceTable === ProcessedMempoolDB.tableName) {
        yield* Effect.logInfo(
          "🔹 No confirmed block available and selected tx payload is already durable in ProcessedMempoolDB; preserving it for the next commit attempt.",
        );
        return {
          type: "SkippedSubmissionOutput",
          mempoolTxsCount: 0,
          sizeOfProcessedTxs: 0,
          candidate,
        } satisfies WorkerOutput;
      }
      const deferred = yield* deferProcessedCommitPayloadUntilConfirmation({
        processedMempoolTxs,
        mempoolTxHashes,
        mempoolTxsCount,
        sizeOfProcessedTxs,
      });
      const output: WorkerOutput =
        deferred.type === "SkippedSubmissionOutput"
          ? { ...deferred, candidate }
          : deferred;
      yield* recordSuccessfulBuildCalibration(output);
      return output;
    } else {
      yield* Effect.logInfo(
        "🔹 Previous submitted block is now confirmed, deserializing...",
      );
      const latestBlock = yield* deserializeStateQueueUTxO(
        availableConfirmedBlock,
      );

      if (Option.isNone(optEndTime)) {
        // No transaction requests found (neither in `ProcessedMempoolDB`, nor
        // in `MempoolDB`). We check if there are any user events slated for
        // inclusion within `startTime` and current moment.
        yield* Effect.logInfo(
          "🔹 Checking for user events... (no tx requests in queue)",
        );
        const output = yield* submitDepositOnlyCommit({
          contracts: submissionContracts,
          consensusProfile: deploymentIdentity.consensusProfile,
          deploymentMarker,
          latestBlock,
          endTime: fixedHistoryEndTime,
          blockTimeMs,
          includedDepositEntries,
          includedDepositEventIds,
          includedForcedTransactionEntries,
          includedForcedTransactionEventIds,
          includedWithdrawalEntries,
          includedWithdrawalEventIds,
          workerInput,
          commitAnchor,
          blockEndTimeCapMs,
          utxoRoot,
          txRoot,
          transitionTraceRoot,
          eventToStepRoot,
          validationTracesRoot,
          transitionTraceMembers,
          eventToStepMembers,
          validationTraceMembers,
          transitionStepCount,
          validationTraceCount,
          ledgerDelta,
          selectedBaseUtxosRoot: commitBase.root,
          implicitGenesisEntries:
            commitBase.source === "genesis" ? initialLedgerEntries : [],
          afterDaFrameAccepted: notifyParent?.(COMMIT_DA_FRAME_FITS_NOTICE),
          nativeMpfReplay,
        }).pipe(Effect.provideService(Lucid, submissionLucid));
        return attachNativeMpfPromotion(output);
      } else {
        // One or more transactions found in either `ProcessedMempoolDB` or
        // `MempoolDB`. Use the shared max-candidate timestamp rule as the upper
        // bound of the block we are about to submit.
        const endTime = fixedHistoryEndTime;

        yield* Effect.logInfo("🔹 Checking for user events...");
        const output = yield* submitTxBackedCommit({
          contracts: submissionContracts,
          consensusProfile: deploymentIdentity.consensusProfile,
          deploymentMarker,
          latestBlock,
          endTime,
          blockTimeMs,
          includedDepositEntries,
          includedDepositEventIds,
          includedForcedTransactionEntries,
          includedForcedTransactionEventIds,
          includedWithdrawalEntries,
          includedWithdrawalEventIds,
          utxoRoot,
          txRoot,
          transitionTraceRoot,
          eventToStepRoot,
          validationTracesRoot,
          transitionTraceMembers,
          eventToStepMembers,
          validationTraceMembers,
          transitionStepCount,
          validationTraceCount,
          ledgerDelta,
          selectedBaseUtxosRoot: commitBase.root,
          implicitGenesisEntries:
            commitBase.source === "genesis" ? initialLedgerEntries : [],
          transactionsMpf,
          processedMempoolTxs,
          mempoolTxHashes,
          mempoolTxSourceTable,
          workerInput,
          commitAnchor,
          sizeOfProcessedTxs,
          blockEndTimeCapMs,
          afterDaFrameAccepted: notifyParent?.(COMMIT_DA_FRAME_FITS_NOTICE),
          nativeMpfReplay,
        }).pipe(Effect.provideService(Lucid, submissionLucid));
        yield* recordSuccessfulBuildCalibration(output);
        return attachNativeMpfPromotion(output);
      }
    }
  });
