import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, toUnit } from "@lucid-evolution/lucid";
import { Effect, Schedule } from "effect";

import { PendingBlockFinalizationsDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import * as Ledger from "../database/utils/ledger.js";
import { MidgardMpf } from "../mpf/index.js";
import {
  Database,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import {
  awaitExactTransactionConfirmation,
  type TxSignError,
  type TxSubmitError,
} from "../transactions/utils.js";
import {
  EXPLICIT_COMMIT_BLOCK_VISIBILITY_DELAY,
  EXPLICIT_COMMIT_BLOCK_VISIBILITY_RETRIES,
  EXPLICIT_COMMIT_CONFIRMATION_POLL_INTERVAL_MS,
  EXPLICIT_COMMIT_CONFIRMATION_TIMEOUT_MS,
} from "./commit-block-header.pending-user-event-counts-up-to.js";
import { type ResolvedCommitBaseLedgerEntries } from "./commit-block-header.select-authenticated-foreign-base-candidate.js";
import { buildUnsignedCommitTx } from "./commit-block-header/build-unsigned-tx.js";
import { fetchLatestCommittedBlockLocal } from "./commit-block-header/state-queue.js";
import { makeEventCommitments } from "./commit-block-header/transition-commitments.js";
import {
  type MempoolLedgerRevertedNotice,
  type RegisteredDueWorkOutput,
  type SpeculativeCandidateReadyOutput,
  type SpeculativeCommitWorkerInstruction,
  type SuccessfulLocalFinalizationRecoveryOutput,
  WorkerOutput,
} from "./utils/commit-block-header.js";
import { type CommitDaFrameNotice } from "./utils/commit-block-planner.commit-da-frame-notice.js";
import { type EarliestCommitSchedulerPlan } from "./utils/commit-block-planner.js";
import { resolveExplicitCommitCandidateEndTimeMs } from "./utils/commit-end-time.js";

export const alignCommitMpfsToBase = ({
  nativeMpfRoot,
  transactionsMpf,
  base,
}: {
  readonly nativeMpfRoot: string;
  readonly transactionsMpf: MidgardMpf;
  readonly base: ResolvedCommitBaseLedgerEntries;
}): Effect.Effect<readonly Ledger.MinimalEntry[], unknown, never> =>
  Effect.gen(function* () {
    if (nativeMpfRoot !== base.root) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Architecture G durable marker differs from the selected commit base",
          cause: `source=${base.source},current_root=${nativeMpfRoot},expected_root=${base.root}`,
        }),
      );
    }

    const transactionsRootIsEmpty = yield* transactionsMpf.rootIsEmpty();
    if (!transactionsRootIsEmpty) {
      yield* transactionsMpf.resetToEmpty();
      yield* Effect.logInfo(
        `🔹 Reset per-block transactions MPF before building on ${base.source}.`,
      );
    }

    return base.entries ?? [];
  });

export const shouldPreserveCommitMpfRoots = (output: WorkerOutput): boolean => {
  switch (output.type) {
    case "SubmittedAwaitingConfirmationOutput":
    case "SubmittedAwaitingLocalFinalizationOutput":
    case "SuccessfulSubmissionOutput":
    case "SkippedSubmissionOutput":
      return true;
    case "SuccessfulLocalFinalizationRecoveryOutput":
      // Local finalization intentionally advances durable DB state and resets
      // the transactions MPF root; rolling back would undo recovery.
      return true;
    case "FailureOutput":
    case "AwaitingForeignDaOutput":
    case "RegisteredDueWorkOutput":
    case "NothingToCommitOutput":
    case "SpeculativeCandidateReadyOutput":
    case "SpeculativeCandidateInvalidatedOutput":
      return false;
  }
};

export const workerPreIngestionDueWorkOutputFromPlan = (
  plan: EarliestCommitSchedulerPlan,
): RegisteredDueWorkOutput | undefined =>
  plan.status === "register_due_work"
    ? {
        type: "RegisteredDueWorkOutput",
        dueWork: plan.dueWork,
      }
    : undefined;

export const shouldShortCircuitIdleCommitAttempt = ({
  candidateTxCount,
  processedPendingTxCount,
  pendingUserEventCount,
  localFinalizationPending,
}: {
  readonly candidateTxCount: number;
  readonly processedPendingTxCount: number;
  readonly pendingUserEventCount: number;
  readonly localFinalizationPending: boolean;
}): boolean =>
  candidateTxCount === 0 &&
  processedPendingTxCount === 0 &&
  pendingUserEventCount === 0 &&
  !localFinalizationPending;

export type ExplicitBlockHeaderCommitParams = {
  readonly utxosRoot: string;
  readonly transactionsRoot: string;
  readonly depositsRoot: string;
  readonly withdrawalsRoot: string;
  // For fault-proof drills that commit a non-empty transactions root, the
  // header must carry a matching l2_transaction_count (> 0) and, because
  // total_event_count > 0, non-empty transition roots. The transition roots are
  // not checked by the CommitBlockHeader validator, so callers supply arbitrary
  // non-empty values.
  readonly l2TransactionCount?: bigint;
  readonly transitionTraceRoot?: string;
  readonly eventToStepRoot?: string;
  readonly validationTracesRoot?: string;
  readonly validationTraceCount?: bigint;
  readonly endTimeMs?: number;
  readonly awaitConfirmation?: boolean;
};

export type ExplicitBlockHeaderCommitOutput = {
  readonly submittedTxHash: string;
  readonly headerHash: string;
  readonly blockOutRef: string | null;
  readonly txSize: number;
  readonly blockEndTimeMs: number;
  readonly roots: {
    readonly utxosRoot: string;
    readonly transactionsRoot: string;
    readonly depositsRoot: string;
    readonly withdrawalsRoot: string;
  };
};

const waitForTxConfirmation = (
  lucid: LucidEvolution,
  txHash: string,
): Effect.Effect<void, SDK.LucidError> =>
  Effect.tryPromise({
    try: () =>
      awaitExactTransactionConfirmation(lucid, txHash, {
        timeout: EXPLICIT_COMMIT_CONFIRMATION_TIMEOUT_MS,
        checkInterval: EXPLICIT_COMMIT_CONFIRMATION_POLL_INTERVAL_MS,
      }).then(() => undefined),
    catch: (cause) =>
      new SDK.LucidError({
        message: "Failed to confirm explicit block-header commit transaction",
        cause,
      }),
  });

const fetchCommittedBlockOutRef = ({
  lucid,
  contracts,
  headerHash,
}: {
  readonly lucid: LucidEvolution;
  readonly contracts: SDK.MidgardValidators;
  readonly headerHash: string;
}): Effect.Effect<string, SDK.LucidError> =>
  Effect.tryPromise({
    try: async () => {
      const unit = toUnit(
        contracts.stateQueue.policyId,
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + headerHash,
      );
      const utxos = await lucid.utxosAtWithUnit(
        contracts.stateQueue.spendingScriptAddress,
        unit,
      );
      if (utxos.length !== 1) {
        throw new Error(
          `expected exactly one committed state_queue block UTxO for ${headerHash}, found ${utxos.length}`,
        );
      }
      return `${utxos[0].txHash}#${utxos[0].outputIndex}`;
    },
    catch: (cause) =>
      new SDK.LucidError({
        message: "Failed to resolve committed state_queue block outref",
        cause,
      }),
  }).pipe(
    Effect.retry(
      Schedule.intersect(
        Schedule.fixed(EXPLICIT_COMMIT_BLOCK_VISIBILITY_DELAY),
        Schedule.recurs(EXPLICIT_COMMIT_BLOCK_VISIBILITY_RETRIES),
      ),
    ),
  );

/**
 * Explicit operator command helper for live fault-proof drills. The supplied
 * roots are committed through the same real state_queue, scheduler, and active
 * operator transaction builder used by the production block worker, but no
 * local database finalization is attempted.
 */
export const commitExplicitBlockHeaderProgram = (
  params: ExplicitBlockHeaderCommitParams,
): Effect.Effect<
  ExplicitBlockHeaderCommitOutput,
  | SDK.StateQueueError
  | SDK.DataCoercionError
  | SDK.HeaderTransitionCommitmentsError
  | SDK.LucidError
  | SDK.LinkedListError
  | SDK.HashingError
  | TxSignError
  | TxSubmitError,
  Lucid | MidgardContracts | NodeConfig
> =>
  Effect.gen(function* () {
    const lucidService = yield* Lucid;
    const contracts = yield* MidgardContracts;
    const lucid = lucidService.api;
    const fetchConfig: SDK.StateQueueFetchConfig = {
      stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
      stateQueuePolicyId: contracts.stateQueue.policyId,
    };

    const latestBlock = yield* fetchLatestCommittedBlockLocal(
      lucid,
      fetchConfig,
    );
    const endTime = new Date(
      resolveExplicitCommitCandidateEndTimeMs(params.endTimeMs),
    );
    const transitionCommitments = yield* makeEventCommitments(
      {
        withdrawalsRoot: params.withdrawalsRoot,
        forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        transactionsRoot: params.transactionsRoot,
        depositsRoot: params.depositsRoot,
        transitionTraceRoot:
          params.transitionTraceRoot ?? SDK.EMPTY_MERKLE_TREE_ROOT,
        eventToStepRoot: params.eventToStepRoot ?? SDK.EMPTY_MERKLE_TREE_ROOT,
      },
      {
        withdrawalCount: 0n,
        forcedTransactionCount: 0n,
        l2TransactionCount: params.l2TransactionCount ?? 0n,
        depositCount: 0n,
      },
      {
        validationTracesRoot:
          params.validationTracesRoot ?? SDK.EMPTY_MERKLE_TREE_ROOT,
        validationTraceCount: params.validationTraceCount ?? 0n,
      },
    );
    const explicitBuildResult = yield* buildUnsignedCommitTx(
      contracts,
      latestBlock,
      params.utxosRoot,
      params.transactionsRoot,
      params.depositsRoot,
      params.withdrawalsRoot,
      transitionCommitments,
      contracts.consensusProfile,
      endTime,
    );
    if ("dueWork" in explicitBuildResult) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message:
            "Explicit block commit discovered scheduler due work instead of a ready transaction",
          cause: `kind=${explicitBuildResult.dueWork.kind},due_slot=${explicitBuildResult.dueWork.dueSlot.toString()},wait_ms=${explicitBuildResult.dueWork.waitMs.toString()}`,
        }),
      );
    }
    const { newHeaderHash, blockEndTimeMs, signAndSubmitProgram, txSize } =
      explicitBuildResult;

    const submittedTxHash = yield* signAndSubmitProgram;
    const shouldAwait = params.awaitConfirmation ?? true;
    if (shouldAwait) {
      yield* waitForTxConfirmation(lucid, submittedTxHash);
    }
    const blockOutRef = shouldAwait
      ? yield* fetchCommittedBlockOutRef({
          lucid,
          contracts,
          headerHash: newHeaderHash,
        })
      : null;

    return {
      submittedTxHash,
      headerHash: newHeaderHash,
      blockOutRef,
      txSize,
      blockEndTimeMs,
      roots: {
        utxosRoot: params.utxosRoot,
        transactionsRoot: params.transactionsRoot,
        depositsRoot: params.depositsRoot,
        withdrawalsRoot: params.withdrawalsRoot,
      },
    };
  });

export type AwaitSpeculativeCommitInstruction = (
  candidate: SpeculativeCandidateReadyOutput["candidate"],
) => Effect.Effect<SpeculativeCommitWorkerInstruction, unknown, Database>;

/** Posts a message to the parent ahead of the worker's output. */
export type NotifyCommitWorkerParent = (
  message:
    | SuccessfulLocalFinalizationRecoveryOutput
    | MempoolLedgerRevertedNotice
    | CommitDaFrameNotice,
) => Effect.Effect<void>;

export const MEMPOOL_LEDGER_REVERTED_NOTICE: MempoolLedgerRevertedNotice = {
  type: "MempoolLedgerRevertedNotice",
};
