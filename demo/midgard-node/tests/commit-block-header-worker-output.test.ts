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
} from "../src/fibers/block-commitment.js";
import { MidgardMpf, withMpfRootTransactions } from "../src/mpf/index.js";
import { Globals } from "../src/services/index.js";
import {
  shouldPreserveCommitMpfRoots,
  shouldShortCircuitIdleCommitAttempt,
  workerPreIngestionDueWorkOutputFromPlan,
} from "../src/workers/commit-block-header.js";
import type {
  SerializedStateQueueUTxO,
  WorkerOutput,
} from "../src/workers/utils/commit-block-header.js";

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

/**
 * Runs a scratch MPF mutation inside the production root-transaction wrapper
 * and reports, through real MPF roots, whether the mutation survived.
 */
const runRootTransaction = (output: WorkerOutput) =>
  Effect.runPromise(
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
      return {
        result,
        ledgerKept: (yield* ledgerMpf.rootHex()) !== beforeLedgerRoot,
        transactionsKept:
          (yield* transactionsMpf.rootHex()) !== beforeTransactionsRoot,
      };
    }),
  );

/**
 * The gates below are asserted as pure decisions. That the commit fiber
 * actually consults them *before* taking the state-queue mutation lease is
 * asserted behaviourally in
 * `tests/block-commitment-provider-evidence-preflight.test.ts`, by running
 * `blockCommitmentAction` and observing that the lease store is never touched.
 */
describe("commit block worker output handling", () => {
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

  /**
   * One case per worker-output type, keyed by the type itself: the `Record`
   * over `WorkerOutput["type"]` stops compiling when an output type is added
   * without deciding here whether it may keep the scratch MPF roots. The
   * expected value is the output's own meaning -- an output that told L1 (or
   * the durable database) about the new state must keep the roots, everything
   * else must roll them back -- not a copy of the production switch.
   */
  const ROOT_TRANSACTION_CASES: Record<
    WorkerOutput["type"],
    { readonly output: WorkerOutput; readonly preserve: boolean }
  > = {
    FailureOutput: {
      output: { type: "FailureOutput", error: "boom" },
      preserve: false,
    },
    RegisteredDueWorkOutput: {
      output: { type: "RegisteredDueWorkOutput", dueWork },
      preserve: false,
    },
    NothingToCommitOutput: {
      output: { type: "NothingToCommitOutput" },
      preserve: false,
    },
    AwaitingForeignDaOutput: {
      output: {
        type: "AwaitingForeignDaOutput",
        foreignHeaderHash: "dd".repeat(28),
        reason: "foreign DA payload not yet retrievable",
      },
      preserve: false,
    },
    // Speculative candidates only ever exist in memory; only the `type` field
    // takes part in the decision, so the payloads stay minimal.
    SpeculativeCandidateReadyOutput: {
      output: {
        type: "SpeculativeCandidateReadyOutput",
        candidate: {} as never,
      },
      preserve: false,
    },
    SpeculativeCandidateInvalidatedOutput: {
      output: {
        type: "SpeculativeCandidateInvalidatedOutput",
        candidateId: "candidate-1",
        reason: "T1" as never,
      },
      preserve: false,
    },
    SubmittedAwaitingConfirmationOutput: {
      output: {
        type: "SubmittedAwaitingConfirmationOutput",
        submittedTxHash: "tx",
        txSize: 1,
        mempoolTxsCount: 1,
        sizeOfBlocksTxs: 1,
        blockEndTimeMs: 1,
        submittedHeaderHash: "aa".repeat(28),
        submittedUtxosRoot: "bb".repeat(32),
      },
      preserve: true,
    },
    SubmittedAwaitingLocalFinalizationOutput: {
      output: {
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
      preserve: true,
    },
    SuccessfulSubmissionOutput: {
      output: {
        type: "SuccessfulSubmissionOutput",
        submittedTxHash: "tx",
        txSize: 1,
        mempoolTxsCount: 1,
        sizeOfBlocksTxs: 1,
        blockEndTimeMs: 1,
        mempoolLedgerDeletedOutRefHexes: [],
      },
      preserve: true,
    },
    SkippedSubmissionOutput: {
      output: {
        type: "SkippedSubmissionOutput",
        mempoolTxsCount: 0,
        sizeOfProcessedTxs: 0,
      },
      preserve: true,
    },
    SuccessfulLocalFinalizationRecoveryOutput: {
      output: {
        type: "SuccessfulLocalFinalizationRecoveryOutput",
        finalizedHeaderHash: "cc".repeat(28),
        mempoolTxsCount: 1,
        sizeOfBlocksTxs: 1,
        mempoolLedgerDeletedOutRefHexes: [],
      },
      preserve: true,
    },
  };

  it.each(Object.entries(ROOT_TRANSACTION_CASES))(
    "%s keeps or rolls back the scratch MPF roots as its meaning requires",
    async (_type, { output, preserve }) => {
      const observed = await runRootTransaction(output);

      expect(observed).toStrictEqual({
        result: output,
        ledgerKept: preserve,
        transactionsKept: preserve,
      });
    },
  );
});
