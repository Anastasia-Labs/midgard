import { readFile } from "node:fs/promises";

import type { ProductionCommitBlockHeaderParams } from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  releaseCommitMutationWorkerPhase,
  releaseCommitSchedulerAlignmentPhase,
  shouldAttemptCommitPipeline,
  shouldDeferCommitWorkerForLocalFinalization,
  shouldRunPreLeaseSchedulerAlignment,
  tryAcquireCommitMutationWorkerPhase,
  tryAcquireCommitSchedulerAlignmentPhase,
} from "@/fibers/block-commitment.js";
import { Globals } from "@/services/index.js";
import {
  shouldPreserveCommitMpfRoots,
  shouldShortCircuitIdleCommitAttempt,
  workerPreIngestionDueWorkOutputFromPlan,
} from "@/workers/commit-block-header.js";
import type {
  SerializedStateQueueUTxO,
  WorkerOutput,
} from "@/workers/utils/commit-block-header.js";
import { MidgardMpf, withMpfRootTransactions } from "@/workers/utils/mpf.js";

const dueWork = {
  kind: "commit_scheduler_refresh",
  key: "block_commitment",
  callerLabel: "scheduler-refresh",
  reason: "scheduler_transition_not_reached",
  observedSlot: 10,
  dueSlot: 20,
  dueAtMs: 2_000,
  waitMs: 10_000,
  slotSource: "test",
  dependencyKey: "dep",
  invalidationKey: "dep",
} as const;

const confirmedRecoveryBlock = {} as SerializedStateQueueUTxO;

const assertRootTransactionHandling = (
  output: WorkerOutput,
  shouldPreserve: boolean,
) =>
  Effect.gen(function* () {
    const ledgerMpf = yield* MidgardMpf.createScratch("ledger");
    const transactionsMpf = yield* MidgardMpf.createScratch("transactions");
    const beforeLedgerRoot = yield* ledgerMpf.rootHex();
    const beforeTransactionsRoot = yield* transactionsMpf.rootHex();
    const result = yield* withMpfRootTransactions(
      [ledgerMpf, transactionsMpf],
      Effect.gen(function* () {
        yield* ledgerMpf.applyBatch([
          {
            type: "insert",
            key: Buffer.from("01", "hex"),
            value: Buffer.from("aa", "hex"),
          },
        ]);
        yield* transactionsMpf.applyBatch([
          {
            type: "insert",
            key: Buffer.from("02", "hex"),
            value: Buffer.from("bb", "hex"),
          },
        ]);
        return output;
      }),
      shouldPreserveCommitMpfRoots,
    );
    const afterLedgerRoot = yield* ledgerMpf.rootHex();
    const afterTransactionsRoot = yield* transactionsMpf.rootHex();

    expect(result).toStrictEqual(output);
    if (shouldPreserve) {
      expect(afterLedgerRoot).not.toBe(beforeLedgerRoot);
      expect(afterTransactionsRoot).not.toBe(beforeTransactionsRoot);
    } else {
      expect(afterLedgerRoot).toBe(beforeLedgerRoot);
      expect(afterTransactionsRoot).toBe(beforeTransactionsRoot);
    }
  });

describe("commit block worker output handling", () => {
  it("keeps commit-block submit due-work unregistered while SDK commit txs have no lower validity bound", () => {
    type CommitBlockHasNoLowerValidityBound =
      "validFrom" extends keyof ProductionCommitBlockHeaderParams
        ? false
        : true;
    const commitBlockHasNoLowerValidityBound: CommitBlockHasNoLowerValidityBound =
      true;

    expect(commitBlockHasNoLowerValidityBound).toBe(true);
  });

  it("treats registered due work as normal non-submission control flow", () => {
    const output: WorkerOutput = {
      type: "RegisteredDueWorkOutput",
      dueWork,
    };

    expect(shouldPreserveCommitMpfRoots(output)).toBe(false);
  });

  it("materializes worker pre-ingestion scheduler due-work as normal worker output", () => {
    expect(
      workerPreIngestionDueWorkOutputFromPlan({
        status: "register_due_work",
        reason: "scheduler_transition_not_reached",
        discoveryStage: "worker_pre_ingestion",
        dueWork,
      }),
    ).toStrictEqual({
      type: "RegisteredDueWorkOutput",
      dueWork,
    });

    expect(
      workerPreIngestionDueWorkOutputFromPlan({
        status: "ambiguous",
        reason: "slot_source_unavailable",
      }),
    ).toBeUndefined();
  });

  it("keeps scheduler alignment outside the active mutation-worker phase", async () => {
    const result = await Effect.runPromise(
      Effect.gen(function* () {
        const globals = yield* Globals;

        const workerAcquire =
          yield* tryAcquireCommitMutationWorkerPhase(globals);
        const workerActiveAfterAcquire = yield* globals.COMMIT_WORKER_ACTIVE;
        const alignmentBlockedByWorker =
          yield* tryAcquireCommitSchedulerAlignmentPhase(globals);
        yield* releaseCommitMutationWorkerPhase(globals);

        const alignmentAcquire =
          yield* tryAcquireCommitSchedulerAlignmentPhase(globals);
        const workerActiveDuringAlignment = yield* globals.COMMIT_WORKER_ACTIVE;
        const workerBlockedByAlignment =
          yield* tryAcquireCommitMutationWorkerPhase(globals);
        yield* releaseCommitSchedulerAlignmentPhase(globals);

        return {
          workerAcquire,
          workerActiveAfterAcquire,
          alignmentBlockedByWorker,
          alignmentAcquire,
          workerActiveDuringAlignment,
          workerBlockedByAlignment,
        };
      }).pipe(Effect.provide(Globals.Default)),
    );

    expect(result.workerAcquire).toStrictEqual({ acquired: true });
    expect(result.workerActiveAfterAcquire).toBe(true);
    expect(result.alignmentBlockedByWorker).toStrictEqual({
      acquired: false,
      activePhase: "mutation_worker",
    });
    expect(result.alignmentAcquire).toStrictEqual({ acquired: true });
    expect(result.workerActiveDuringAlignment).toBe(false);
    expect(result.workerBlockedByAlignment).toStrictEqual({
      acquired: false,
      activePhase: "scheduler_alignment",
    });
  });

  it("still runs detailed pre-lease alignment when the current operator is active", () => {
    expect(
      shouldRunPreLeaseSchedulerAlignment({
        status: "proceed",
        reason: "current_operator_already_active",
        dependencyKey: "dep",
        invalidationKey: "dep",
      }),
    ).toBe(true);

    expect(
      shouldRunPreLeaseSchedulerAlignment({
        status: "proceed",
        reason: "local_finalization_pending",
        dependencyKey: "dep",
        invalidationKey: "dep",
      }),
    ).toBe(false);
  });

  it("skips the pre-lease commit pipeline only when all commit work sources are empty", () => {
    const base = {
      localFinalizationPending: false,
      availableLocalFinalizationBlock: "" as const,
      mempoolTxCount: 0n,
      processedUnsubmittedTxCount: 0,
      pendingUserEventCount: 0,
    };

    expect(shouldAttemptCommitPipeline(base)).toBe(false);
    expect(
      shouldAttemptCommitPipeline({
        ...base,
        localFinalizationPending: true,
        availableLocalFinalizationBlock: confirmedRecoveryBlock,
      }),
    ).toBe(true);
    expect(
      shouldAttemptCommitPipeline({
        ...base,
        mempoolTxCount: 1n,
      }),
    ).toBe(true);
    expect(
      shouldAttemptCommitPipeline({
        ...base,
        processedUnsubmittedTxCount: 1,
      }),
    ).toBe(true);
    expect(
      shouldAttemptCommitPipeline({
        ...base,
        pendingUserEventCount: 1,
      }),
    ).toBe(true);
  });

  it("does not reacquire the commit lease while local finalization awaits a confirmed recovery block", () => {
    const pendingWithoutConfirmedBlock = {
      localFinalizationPending: true,
      availableLocalFinalizationBlock: "",
    } as const;

    // The v17 failure had 127 mempool transactions, but they cannot form a new
    // commitment until the submitted predecessor is confirmed and finalized.
    expect(
      shouldAttemptCommitPipeline({
        localFinalizationPending: true,
        availableLocalFinalizationBlock: "",
        mempoolTxCount: 127n,
        processedUnsubmittedTxCount: 127,
        pendingUserEventCount: 0,
      }),
    ).toBe(false);
    expect(
      shouldDeferCommitWorkerForLocalFinalization(pendingWithoutConfirmedBlock),
    ).toBe(true);
    // Confirmation is the bounded wake-up: once it publishes the matching
    // block, the next scheduled tick may run the local recovery worker.
    expect(
      shouldDeferCommitWorkerForLocalFinalization({
        localFinalizationPending: true,
        availableLocalFinalizationBlock: confirmedRecoveryBlock,
      }),
    ).toBe(false);
    expect(
      shouldAttemptCommitPipeline({
        localFinalizationPending: true,
        availableLocalFinalizationBlock: confirmedRecoveryBlock,
        mempoolTxCount: 127n,
        processedUnsubmittedTxCount: 127,
        pendingUserEventCount: 0,
      }),
    ).toBe(true);
  });

  it("evaluates the pending-finalization gate before acquiring the commit mutation lease", async () => {
    const source = await readFile(
      new URL("../src/fibers/block-commitment.ts", import.meta.url),
      "utf8",
    );
    const action = source.slice(
      source.indexOf("export const blockCommitmentAction"),
      source.indexOf("export const blockCommitmentFiber"),
    );
    const pendingFinalizationGate = action.indexOf(
      "shouldSkipIdleCommitPipelineBeforeSchedulerAlignment",
    );
    const mutationLease = action.indexOf(
      "StateQueueMutationLeasesDB.tryWithLease",
    );

    expect(pendingFinalizationGate).toBeGreaterThanOrEqual(0);
    expect(mutationLease).toBeGreaterThanOrEqual(0);
    expect(pendingFinalizationGate).toBeLessThan(mutationLease);
  });

  it("short-circuits idle attempts only after tx, event, and recovery work are absent", () => {
    expect(
      shouldShortCircuitIdleCommitAttempt({
        candidateTxCount: 0,
        processedPendingTxCount: 0,
        pendingUserEventCount: 0,
        localFinalizationPending: false,
      }),
    ).toBe(true);
    expect(
      shouldShortCircuitIdleCommitAttempt({
        candidateTxCount: 1,
        processedPendingTxCount: 0,
        pendingUserEventCount: 0,
        localFinalizationPending: false,
      }),
    ).toBe(false);
    expect(
      shouldShortCircuitIdleCommitAttempt({
        candidateTxCount: 0,
        processedPendingTxCount: 1,
        pendingUserEventCount: 0,
        localFinalizationPending: false,
      }),
    ).toBe(false);
    expect(
      shouldShortCircuitIdleCommitAttempt({
        candidateTxCount: 0,
        processedPendingTxCount: 0,
        pendingUserEventCount: 1,
        localFinalizationPending: false,
      }),
    ).toBe(false);
    expect(
      shouldShortCircuitIdleCommitAttempt({
        candidateTxCount: 0,
        processedPendingTxCount: 0,
        pendingUserEventCount: 0,
        localFinalizationPending: true,
      }),
    ).toBe(false);
  });

  it("resets or preserves MPF roots according to worker output semantics", async () => {
    const resetOutputs: readonly WorkerOutput[] = [
      { type: "FailureOutput", error: "boom" },
      { type: "RegisteredDueWorkOutput", dueWork },
      { type: "NothingToCommitOutput" },
    ];
    const preserveOutputs: readonly WorkerOutput[] = [
      {
        type: "SubmittedAwaitingConfirmationOutput",
        submittedTxHash: "tx",
        txSize: 1,
        mempoolTxsCount: 1,
        sizeOfBlocksTxs: 1,
        blockEndTimeMs: 1,
        submittedHeaderHash: "aa".repeat(28),
        submittedUtxosRoot: "bb".repeat(32),
      },
      {
        type: "SubmittedAwaitingLocalFinalizationOutput",
        submittedTxHash: "tx",
        txSize: 1,
        mempoolTxsCount: 1,
        sizeOfBlocksTxs: 1,
        blockEndTimeMs: 1,
        error: "local finalization pending",
        submittedHeaderHash: "aa".repeat(28),
        submittedUtxosRoot: "bb".repeat(32),
      },
      {
        type: "SuccessfulSubmissionOutput",
        submittedTxHash: "tx",
        txSize: 1,
        mempoolTxsCount: 1,
        sizeOfBlocksTxs: 1,
        blockEndTimeMs: 1,
        mempoolLedgerDeletedOutRefHexes: [],
      },
      {
        type: "SkippedSubmissionOutput",
        mempoolTxsCount: 0,
        sizeOfProcessedTxs: 0,
      },
      {
        type: "SuccessfulLocalFinalizationRecoveryOutput",
        finalizedHeaderHash: "cc".repeat(28),
        mempoolTxsCount: 1,
        sizeOfBlocksTxs: 1,
        mempoolLedgerDeletedOutRefHexes: [],
      },
    ];

    for (const output of resetOutputs) {
      await Effect.runPromise(assertRootTransactionHandling(output, false));
    }
    for (const output of preserveOutputs) {
      await Effect.runPromise(assertRootTransactionHandling(output, true));
    }
  });
});
