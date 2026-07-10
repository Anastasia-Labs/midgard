import { Option } from "effect";
import { describe, expect, it } from "vitest";

import {
  Columns as TxColumns,
  EntryWithTimeStamp,
} from "@/database/utils/tx.js";
import { shouldShortCircuitIdleCommitAttempt } from "@/workers/commit-block-header.js";
import {
  buildSuccessfulCommitBatches,
  type CommitSchedulerStateQueueEvidence,
  establishEndTimeFromTxRequests,
  planCommitBatchBudgets,
  planEarliestCommitSchedulerDueWork,
  planSchedulerAwareCommitSelection,
  selectCommitRoots,
  selectCommitTxCandidates,
} from "@/workers/utils/commit-block-planner.js";

const mkTxEntry = (
  seed: number,
  timestamp = new Date("2026-01-01T00:00:00.000Z"),
): EntryWithTimeStamp => ({
  [TxColumns.TX_ID]: Buffer.from(seed.toString(16).padStart(64, "0"), "hex"),
  [TxColumns.TX]: Buffer.from(`tx-${seed}`),
  [TxColumns.TIMESTAMPTZ]: timestamp,
});

const fitInsideSchedulerWindow = ({
  resolvedEndTimeMs,
  maximumEndTimeMs,
}: {
  readonly resolvedEndTimeMs: number;
  readonly maximumEndTimeMs: number;
}) =>
  ({
    status: "fits",
    alignedCandidateEndTime: resolvedEndTimeMs,
    minimumMonotonicEndTime: resolvedEndTimeMs - 2_000,
    minimumCurrentTimeEndTime: resolvedEndTimeMs - 1_000,
    resolvedEndTime: resolvedEndTimeMs,
    maximumEndTimeMs,
  }) as const;

const fitExceedingSchedulerWindow = ({
  resolvedEndTimeMs,
  maximumEndTimeMs,
}: {
  readonly resolvedEndTimeMs: number;
  readonly maximumEndTimeMs: number;
}) =>
  ({
    status: "exceeds_cap",
    alignedCandidateEndTime: resolvedEndTimeMs - 2_000,
    minimumMonotonicEndTime: resolvedEndTimeMs - 1_000,
    minimumCurrentTimeEndTime: resolvedEndTimeMs,
    resolvedEndTime: resolvedEndTimeMs,
    maximumEndTimeMs,
    reason: `resolved_end_time_ms=${resolvedEndTimeMs.toString()},maximum_end_time_ms=${maximumEndTimeMs.toString()}`,
  }) as const;

const submitSlotSnapshot = {
  source: "test",
  currentSlot: 10,
  observedAtMs: 50_000,
  slotLengthMs: 1_000,
} as const;

const stateQueueEvidence: CommitSchedulerStateQueueEvidence = {
  tailCommitBaseOutRef: "stateq#1",
  tailBlockEndTimeMs: Date.parse("2026-01-01T00:00:00.000Z"),
  stateQueueHasUnmergedTail: false,
};

describe("commit block planner", () => {
  it("includes processed mempool txs even when mempool is empty", () => {
    const processed = [mkTxEntry(1), mkTxEntry(2), mkTxEntry(3)];
    const batches = buildSuccessfulCommitBatches([], [], processed, 2);

    const txIds = batches
      .flatMap((b) => b.blockTxHashes)
      .map((b) => b.toString("hex"));
    expect(txIds).toStrictEqual(processed.map((p) => p.tx_id.toString("hex")));
    expect(batches.every((b) => b.clearMempoolTxHashes.length === 0)).toBe(
      true,
    );
  });

  it("includes both mempool and processed txs in block insertion batches", () => {
    const mempool = [mkTxEntry(10), mkTxEntry(11)];
    const mempoolHashes = mempool.map((m) => m.tx_id);
    const processed = [mkTxEntry(12), mkTxEntry(13)];

    const batches = buildSuccessfulCommitBatches(
      mempool,
      mempoolHashes,
      processed,
      2,
    );

    const txIds = batches
      .flatMap((b) => b.blockTxHashes)
      .map((b) => b.toString("hex"));
    expect(txIds).toStrictEqual([
      ...mempoolHashes.map((h) => h.toString("hex")),
      ...processed.map((p) => p.tx_id.toString("hex")),
    ]);

    const cleared = batches
      .flatMap((b) => b.clearMempoolTxHashes)
      .map((b) => b.toString("hex"));
    expect(cleared).toStrictEqual(mempoolHashes.map((h) => h.toString("hex")));
  });

  it("uses computed roots in deposits-only commit path", () => {
    const roots = selectCommitRoots({
      hasTxRequests: false,
      computedUtxoRoot: "0xutxo",
      computedTxRoot: "0xtx",
      emptyRoot: "0xempty",
    });

    expect(roots.utxoRoot).toBe("0xutxo");
    expect(roots.txRoot).toBe("0xtx");
  });

  it("selects durable processed transactions before newer mempool transactions", () => {
    const processed = [mkTxEntry(1)];
    const mempool = [mkTxEntry(2)];

    const selection = selectCommitTxCandidates({
      mempoolTxs: mempool,
      processedMempoolTxs: processed,
    });

    expect(selection.sourceTable).toBe("processed_mempool");
    expect(selection.candidateTxs).toStrictEqual(processed);
    expect(selection.candidateTxHashes.map((h) => h.toString("hex"))).toEqual([
      processed[0][TxColumns.TX_ID].toString("hex"),
    ]);
    expect(selection.candidateTxsSize).toBe(processed[0][TxColumns.TX].length);
  });

  it("selects mempool transactions when there is no deferred processed payload", () => {
    const mempool = [mkTxEntry(3), mkTxEntry(4)];

    const selection = selectCommitTxCandidates({
      mempoolTxs: mempool,
      processedMempoolTxs: [],
    });

    expect(selection.sourceTable).toBe("mempool");
    expect(selection.candidateTxs).toStrictEqual(mempool);
    expect(selection.candidateTxHashes.map((h) => h.toString("hex"))).toEqual(
      mempool.map((entry) => entry[TxColumns.TX_ID].toString("hex")),
    );
  });

  it("selects no tx source when both pending queues are empty", () => {
    const selection = selectCommitTxCandidates({
      mempoolTxs: [],
      processedMempoolTxs: [],
    });

    expect(selection).toMatchObject({
      candidateTxs: [],
      candidateTxHashes: [],
      candidateTxsSize: 0,
      sourceTable: "none",
    });
  });

  it("plans conservative commit batches with explicit budget stop reasons", () => {
    const selection = selectCommitTxCandidates({
      mempoolTxs: [mkTxEntry(1), mkTxEntry(2), mkTxEntry(3)],
      processedMempoolTxs: [],
    });

    const planned = planCommitBatchBudgets({
      candidateSelection: selection,
      limits: {
        maxL2TxCount: 2,
        maxCanonicalTxBytes: 1_000,
        maxLedgerOpCount: 1_000,
        maxTransitionStepCount: 1_000,
        maxDaPayloadBytes: 1_000,
        maxCommitTxBytes: 1_000,
        maxEstimatedCommitBuildMs: 1_000,
        estimatedLedgerOpsPerTx: 1,
        estimatedTransitionStepsPerTx: 1,
        estimatedDaOverheadBytesPerTx: 1,
        estimatedCommitTxOverheadBytes: 1,
        estimatedCommitBuildMsPerTx: 1,
      },
    });

    expect(planned.plan.stopReason).toBe("tx_count_budget");
    expect(planned.plan.selectedTxCount).toBe(2);
    expect(planned.prunedTxCount).toBe(1);
    expect(planned.candidateSelection.candidateTxHashes).toHaveLength(2);
  });

  it("uses the first candidate tx timestamp as the shared tx-backed candidate end-time source", () => {
    const first = mkTxEntry(1, new Date("2026-01-01T00:03:00.000Z"));
    const second = mkTxEntry(2, new Date("2026-01-01T00:04:00.000Z"));
    const candidateEndTime = establishEndTimeFromTxRequests([first, second]);

    expect(Option.isSome(candidateEndTime)).toBe(true);
    expect(Option.getOrThrow(candidateEndTime).getTime()).toBe(
      first[TxColumns.TIMESTAMPTZ].getTime(),
    );
  });

  it("short-circuits idle commitment only when no recoverable work remains", () => {
    expect(
      shouldShortCircuitIdleCommitAttempt({
        candidateTxCount: 0,
        processedPendingTxCount: 0,
        pendingUserEventCount: 0,
        localFinalizationPending: false,
      }),
    ).toBe(true);

    for (const nonIdle of [
      { candidateTxCount: 1 },
      { processedPendingTxCount: 1 },
      { pendingUserEventCount: 1 },
      { localFinalizationPending: true },
    ]) {
      expect(
        shouldShortCircuitIdleCommitAttempt({
          candidateTxCount: 0,
          processedPendingTxCount: 0,
          pendingUserEventCount: 0,
          localFinalizationPending: false,
          ...nonIdle,
        }),
      ).toBe(false);
    }
  });

  it("caps tx and user-event selection to a safe current scheduler window", () => {
    const cap = Date.parse("2026-01-01T00:05:00.000Z");
    const beforeCap = mkTxEntry(1, new Date("2026-01-01T00:04:59.000Z"));
    const afterCap = mkTxEntry(2, new Date("2026-01-01T00:05:01.000Z"));
    const selection = {
      candidateTxs: [beforeCap, afterCap],
      candidateTxHashes: [beforeCap.tx_id, afterCap.tx_id],
      candidateTxsSize: afterCap.tx.length + beforeCap.tx.length,
      sourceTable: "mempool" as const,
    };

    const plan = planSchedulerAwareCommitSelection({
      candidateSelection: selection,
      userEventOnlyEndTime: new Date("2026-01-01T00:06:00.000Z"),
      currentSchedulerWindow: {
        schedulerOutRef: "aa#0",
        operatorKeyHash: "operator",
        startTimeMs: Date.parse("2026-01-01T00:00:00.000Z"),
        endTimeMs: cap,
      },
      currentBlockStartTimeMs: Date.parse("2025-12-31T23:59:00.000Z"),
      nowMs: Date.parse("2026-01-01T00:00:00.000Z"),
      minimumCurrentWindowBudgetMs: 60_000,
      productionMinimumFutureBufferMs: 240_000,
      currentWindowCommitEndTimeFit: fitInsideSchedulerWindow({
        resolvedEndTimeMs: Date.parse("2026-01-01T00:04:30.000Z"),
        maximumEndTimeMs: cap,
      }),
    });

    expect(plan.status).toBe("using_current_scheduler_window");
    expect(plan.prunedTxCount).toBe(1);
    expect(plan.userEventOnlyEndTime.getTime()).toBe(cap);
    expect(plan.candidateSelection.candidateTxs).toEqual([beforeCap]);
    expect(plan.candidateSelection.candidateTxHashes).toEqual([
      beforeCap.tx_id,
    ]);
  });

  it("does not cap to the current scheduler window when the remaining budget is too low", () => {
    const cap = Date.parse("2026-01-01T00:05:00.000Z");
    const afterCap = mkTxEntry(2, new Date("2026-01-01T00:05:01.000Z"));
    const selection = selectCommitTxCandidates({
      mempoolTxs: [afterCap],
      processedMempoolTxs: [],
    });

    const plan = planSchedulerAwareCommitSelection({
      candidateSelection: selection,
      userEventOnlyEndTime: new Date("2026-01-01T00:06:00.000Z"),
      currentSchedulerWindow: {
        schedulerOutRef: "aa#0",
        operatorKeyHash: "operator",
        startTimeMs: Date.parse("2026-01-01T00:00:00.000Z"),
        endTimeMs: cap,
      },
      currentBlockStartTimeMs: Date.parse("2025-12-31T23:59:00.000Z"),
      nowMs: cap - 10_000,
      minimumCurrentWindowBudgetMs: 60_000,
      productionMinimumFutureBufferMs: 480_000,
    });

    expect(plan.status).toBe("current_scheduler_budget_too_low");
    expect(plan.candidateSelection).toBe(selection);
    expect(plan.userEventOnlyEndTime.getTime()).toBe(
      Date.parse("2026-01-01T00:06:00.000Z"),
    );
  });

  it("does not cap to a seven-minute scheduler window when the production end-time floor needs eight minutes", () => {
    const nowMs = Date.parse("2026-01-01T00:00:00.000Z");
    const cap = nowMs + 7 * 60_000;
    const afterCap = mkTxEntry(2, new Date(cap + 1_000));
    const selection = selectCommitTxCandidates({
      mempoolTxs: [afterCap],
      processedMempoolTxs: [],
    });
    const userEventOnlyEndTime = new Date(cap + 30_000);

    const plan = planSchedulerAwareCommitSelection({
      candidateSelection: selection,
      userEventOnlyEndTime,
      currentSchedulerWindow: {
        schedulerOutRef: "aa#0",
        operatorKeyHash: "operator",
        startTimeMs: nowMs - 60_000,
        endTimeMs: cap,
      },
      currentBlockStartTimeMs: nowMs - 120_000,
      nowMs,
      minimumCurrentWindowBudgetMs: 6 * 60_000,
      productionMinimumFutureBufferMs: 8 * 60_000,
      currentWindowCommitEndTimeFit: fitExceedingSchedulerWindow({
        resolvedEndTimeMs: nowMs + 8 * 60_000 + 1_000,
        maximumEndTimeMs: cap,
      }),
    });

    expect(plan.status).toBe("current_scheduler_end_time_floor_exceeds_window");
    expect(plan.candidateSelection).toBe(selection);
    expect(plan.userEventOnlyEndTime).toBe(userEventOnlyEndTime);
    expect(plan.reason).toContain(
      `resolved_end_time_ms=${(nowMs + 8 * 60_000 + 1_000).toString()}`,
    );
    expect(plan.reason).toContain(`current_scheduler_end_ms=${cap.toString()}`);
    expect(plan.reason).toContain("minimum_future_buffer_ms=480000");
  });

  it("does not prune candidate txs when the pre-pruning tx-backed candidate end-time cannot fit the current scheduler window", () => {
    const cap = Date.parse("2026-01-01T00:05:00.000Z");
    const beforeCap = mkTxEntry(1, new Date("2026-01-01T00:04:59.000Z"));
    const afterCap = mkTxEntry(2, new Date("2026-01-01T00:05:01.000Z"));
    const selection = {
      candidateTxs: [afterCap, beforeCap],
      candidateTxHashes: [afterCap.tx_id, beforeCap.tx_id],
      candidateTxsSize: afterCap.tx.length + beforeCap.tx.length,
      sourceTable: "mempool" as const,
    };

    const plan = planSchedulerAwareCommitSelection({
      candidateSelection: selection,
      userEventOnlyEndTime: new Date("2026-01-01T00:06:00.000Z"),
      currentSchedulerWindow: {
        schedulerOutRef: "aa#0",
        operatorKeyHash: "operator",
        startTimeMs: Date.parse("2026-01-01T00:00:00.000Z"),
        endTimeMs: cap,
      },
      currentBlockStartTimeMs: Date.parse("2025-12-31T23:59:00.000Z"),
      nowMs: Date.parse("2026-01-01T00:00:00.000Z"),
      minimumCurrentWindowBudgetMs: 60_000,
      productionMinimumFutureBufferMs: 240_000,
      currentWindowCommitEndTimeFit: fitExceedingSchedulerWindow({
        resolvedEndTimeMs: Date.parse("2026-01-01T00:05:01.000Z"),
        maximumEndTimeMs: cap,
      }),
    });

    expect(plan.status).toBe("current_scheduler_end_time_floor_exceeds_window");
    expect(plan.candidateSelection).toBe(selection);
    expect(plan.prunedTxCount).toBe(0);
    expect(plan.userEventOnlyEndTime.getTime()).toBe(
      Date.parse("2026-01-01T00:06:00.000Z"),
    );
  });

  it("requires the current-window fit to be computed against the selected scheduler cap", () => {
    const cap = Date.parse("2026-01-01T00:05:00.000Z");
    const beforeCap = mkTxEntry(1, new Date("2026-01-01T00:04:00.000Z"));
    const selection = selectCommitTxCandidates({
      mempoolTxs: [beforeCap],
      processedMempoolTxs: [],
    });

    const plan = planSchedulerAwareCommitSelection({
      candidateSelection: selection,
      userEventOnlyEndTime: new Date("2026-01-01T00:04:00.000Z"),
      currentSchedulerWindow: {
        schedulerOutRef: "aa#0",
        operatorKeyHash: "operator",
        startTimeMs: Date.parse("2026-01-01T00:00:00.000Z"),
        endTimeMs: cap,
      },
      currentBlockStartTimeMs: Date.parse("2025-12-31T23:59:00.000Z"),
      nowMs: Date.parse("2026-01-01T00:00:00.000Z"),
      minimumCurrentWindowBudgetMs: 60_000,
      productionMinimumFutureBufferMs: 240_000,
      currentWindowCommitEndTimeFit: fitInsideSchedulerWindow({
        resolvedEndTimeMs: Date.parse("2026-01-01T00:04:00.000Z"),
        maximumEndTimeMs: cap + 60_000,
      }),
    });

    expect(plan.status).toBe("current_scheduler_end_time_floor_exceeds_window");
    expect(plan.reason).toContain("commit_end_time_fit_cap_mismatch");
    expect(plan.candidateSelection).toBe(selection);
  });

  it("registers earliest due work only when another active scheduler operator is strictly not due", () => {
    const plan = planEarliestCommitSchedulerDueWork({
      callerLabel: "commit-scheduler-preflight",
      discoveryStage: "pre_lease",
      schedulerOutRef: "scheduler#0",
      schedulerState: {
        status: "active",
        operatorKeyHash: "other-operator",
        startTimeMs: Date.parse("2026-01-01T00:00:00.000Z"),
        transitionInvalidBeforeSlot: 100,
        transitionInvalidHereafterSlot: 600,
      },
      currentOperatorKeyHash: "this-operator",
      submitSlotSnapshot,
      stateQueueEvidence,
      localFinalizationPending: false,
      maxInlineWaitMs: 60_000,
    });

    expect(plan.status).toBe("register_due_work");
    if (plan.status !== "register_due_work") {
      throw new Error(`unexpected plan status ${plan.status}`);
    }
    expect(plan.discoveryStage).toBe("pre_lease");
    expect(plan.dueWork).toMatchObject({
      kind: "commit_scheduler_refresh",
      key: "block_commitment",
      callerLabel: "commit-scheduler-preflight",
      reason: "scheduler_transition_not_reached",
      observedSlot: 10,
      dueSlot: 102,
      waitMs: 92_000,
      dueAtMs: 142_000,
      slotSource: "test",
    });
    expect(plan.dueWork.dependencyKey).toContain("scheduler=scheduler#0");
    expect(plan.dueWork.dependencyKey).toContain(
      "scheduler_operator=other-operator",
    );
    expect(plan.dueWork.dependencyKey).toContain(
      "current_operator=this-operator",
    );
    expect(plan.dueWork.dependencyKey).toContain(
      "state_queue_tail_base=stateq#1",
    );
  });

  it("does not register earliest scheduler due work without strict local slot evidence", () => {
    const plan = planEarliestCommitSchedulerDueWork({
      callerLabel: "commit-scheduler-preflight",
      discoveryStage: "pre_lease",
      schedulerOutRef: "scheduler#0",
      schedulerState: {
        status: "active",
        operatorKeyHash: "other-operator",
        startTimeMs: Date.parse("2026-01-01T00:00:00.000Z"),
        transitionInvalidBeforeSlot: 100,
        transitionInvalidHereafterSlot: 600,
      },
      currentOperatorKeyHash: "this-operator",
      submitSlotSnapshotError: new Error("slot unavailable"),
      stateQueueEvidence,
      localFinalizationPending: false,
      maxInlineWaitMs: 60_000,
    });

    expect(plan).toMatchObject({
      status: "ambiguous",
      reason: "scheduler_submit_timing_slot_source_unavailable",
    });
  });

  it("does not register earliest due work for no-active or current-operator scheduler states", () => {
    const noActive = planEarliestCommitSchedulerDueWork({
      callerLabel: "commit-scheduler-preflight",
      discoveryStage: "pre_lease",
      schedulerOutRef: "scheduler#0",
      schedulerState: { status: "no_active_operators" },
      currentOperatorKeyHash: "this-operator",
      submitSlotSnapshot,
      stateQueueEvidence,
      localFinalizationPending: false,
      maxInlineWaitMs: 60_000,
    });
    expect(noActive).toMatchObject({
      status: "ambiguous",
      reason: "scheduler_has_no_active_operator",
    });

    const currentOperator = planEarliestCommitSchedulerDueWork({
      callerLabel: "commit-scheduler-preflight",
      discoveryStage: "pre_lease",
      schedulerOutRef: "scheduler#0",
      schedulerState: {
        status: "active",
        operatorKeyHash: "this-operator",
        startTimeMs: Date.parse("2026-01-01T00:00:00.000Z"),
        transitionInvalidBeforeSlot: 100,
        transitionInvalidHereafterSlot: 600,
      },
      currentOperatorKeyHash: "this-operator",
      submitSlotSnapshot,
      stateQueueEvidence,
      localFinalizationPending: false,
      maxInlineWaitMs: 60_000,
    });
    expect(currentOperator).toMatchObject({
      status: "proceed",
      reason: "current_operator_already_active",
    });
  });

  it("registers due work even when the scheduler transition is inside the former inline wait budget", () => {
    const plan = planEarliestCommitSchedulerDueWork({
      callerLabel: "commit-scheduler-preflight",
      discoveryStage: "pre_lease",
      schedulerOutRef: "scheduler#0",
      schedulerState: {
        status: "active",
        operatorKeyHash: "other-operator",
        startTimeMs: Date.parse("2026-01-01T00:00:00.000Z"),
        transitionInvalidBeforeSlot: 20,
        transitionInvalidHereafterSlot: 600,
      },
      currentOperatorKeyHash: "this-operator",
      submitSlotSnapshot,
      stateQueueEvidence,
      localFinalizationPending: false,
      maxInlineWaitMs: 60_000,
    });

    expect(plan).toMatchObject({
      status: "register_due_work",
      reason: "scheduler_transition_not_reached",
      discoveryStage: "pre_lease",
      dueWork: {
        kind: "commit_scheduler_refresh",
        key: "block_commitment",
        callerLabel: "commit-scheduler-preflight",
        reason: "scheduler_transition_not_reached",
        observedSlot: 10,
        dueSlot: 22,
        waitMs: 12_000,
        dueAtMs: 62_000,
      },
    });
  });

  it("changes earliest due-work dependency keys when scheduler or state-queue evidence changes", () => {
    const baseInput = {
      callerLabel: "commit-scheduler-preflight",
      discoveryStage: "pre_lease",
      schedulerOutRef: "scheduler#0",
      schedulerState: {
        status: "active",
        operatorKeyHash: "other-operator",
        startTimeMs: Date.parse("2026-01-01T00:00:00.000Z"),
        transitionInvalidBeforeSlot: 100,
        transitionInvalidHereafterSlot: 600,
      },
      currentOperatorKeyHash: "this-operator",
      submitSlotSnapshot,
      stateQueueEvidence,
      localFinalizationPending: false,
      maxInlineWaitMs: 60_000,
    } as const;
    const base = planEarliestCommitSchedulerDueWork(baseInput);
    const changedScheduler = planEarliestCommitSchedulerDueWork({
      ...baseInput,
      schedulerOutRef: "scheduler#1",
    });
    const changedStateQueue = planEarliestCommitSchedulerDueWork({
      ...baseInput,
      stateQueueEvidence: {
        ...stateQueueEvidence,
        tailCommitBaseOutRef: "stateq#2",
      },
    });
    const changedTailEndTime = planEarliestCommitSchedulerDueWork({
      ...baseInput,
      stateQueueEvidence: {
        ...stateQueueEvidence,
        tailBlockEndTimeMs: stateQueueEvidence.tailBlockEndTimeMs + 1_000,
      },
    });
    const localFinalizationPending = planEarliestCommitSchedulerDueWork({
      ...baseInput,
      localFinalizationPending: true,
    });

    expect(base.status).toBe("register_due_work");
    expect(changedScheduler.status).toBe("register_due_work");
    expect(changedStateQueue.status).toBe("register_due_work");
    expect(changedTailEndTime.status).toBe("register_due_work");
    expect(localFinalizationPending.status).toBe("proceed");
    if (
      base.status !== "register_due_work" ||
      changedScheduler.status !== "register_due_work" ||
      changedStateQueue.status !== "register_due_work" ||
      changedTailEndTime.status !== "register_due_work" ||
      localFinalizationPending.status !== "proceed"
    ) {
      throw new Error("expected due-work and local-finalization proceed plans");
    }
    expect(changedScheduler.dueWork.dependencyKey).not.toBe(
      base.dueWork.dependencyKey,
    );
    expect(changedStateQueue.dueWork.dependencyKey).not.toBe(
      base.dueWork.dependencyKey,
    );
    expect(changedTailEndTime.dueWork.dependencyKey).not.toBe(
      base.dueWork.dependencyKey,
    );
    expect(localFinalizationPending.dependencyKey).not.toBe(
      base.dueWork.dependencyKey,
    );
    expect(localFinalizationPending.dependencyKey).toContain(
      "local_finalization_pending=true",
    );
  });
});
