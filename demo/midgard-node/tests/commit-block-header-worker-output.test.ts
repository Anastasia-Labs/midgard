import type { ProductionCommitBlockHeaderParams } from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { describe, expect, it } from "vitest";

import {
  shouldPreserveCommitMpfRoots,
  shouldShortCircuitIdleCommitAttempt,
  workerPreIngestionDueWorkOutputFromPlan,
} from "@/workers/commit-block-header.js";
import type { WorkerOutput } from "@/workers/utils/commit-block-header.js";
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
      },
      {
        type: "SubmittedAwaitingLocalFinalizationOutput",
        submittedTxHash: "tx",
        txSize: 1,
        mempoolTxsCount: 1,
        sizeOfBlocksTxs: 1,
        blockEndTimeMs: 1,
        error: "local finalization pending",
      },
      {
        type: "SuccessfulSubmissionOutput",
        submittedTxHash: "tx",
        txSize: 1,
        mempoolTxsCount: 1,
        sizeOfBlocksTxs: 1,
        blockEndTimeMs: 1,
      },
      {
        type: "SkippedSubmissionOutput",
        mempoolTxsCount: 0,
        sizeOfProcessedTxs: 0,
      },
      {
        type: "SuccessfulLocalFinalizationRecoveryOutput",
        mempoolTxsCount: 1,
        sizeOfBlocksTxs: 1,
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
