/**
 * Production block-commit worker entrypoint.
 * This module orchestrates MPF root processing, commit transaction
 * assembly, submission, and recovery by composing the smaller worker helpers.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { type LucidEvolution, toUnit } from "@lucid-evolution/lucid";
import { Cause, Data, Effect, Option, pipe, Schedule } from "effect";
import { parentPort, workerData } from "worker_threads";

import {
  ConfirmedLedgerDB,
  DepositsDB,
  ForcedTransactionsDB,
  MempoolDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  TxUtils as TxTable,
  WithdrawalsDB,
} from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import * as Ledger from "@/database/utils/ledger.js";
import { Columns as TxColumns } from "@/database/utils/tx.js";
import { fetchAndInsertDepositUTxOsForCommitBarrier } from "@/fibers/fetch-and-insert-deposit-utxos.js";
import { fetchAndInsertTxOrderUTxOsForCommitBarrier } from "@/fibers/fetch-and-insert-tx-order-utxos.js";
import { fetchAndInsertWithdrawalUTxOsForCommitBarrier } from "@/fibers/fetch-and-insert-withdrawal-utxos.js";
import {
  ConfigError,
  Database,
  DatabaseInitializationError,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import { materializeConfirmedLedgerSnapshot } from "@/transactions/state-queue/confirmed-ledger-snapshot.js";
import { type TxSignError, type TxSubmitError } from "@/transactions/utils.js";
import { buildUnsignedCommitTx } from "@/workers/commit-block-header/build-unsigned-tx.js";
import { fetchLatestCommittedBlockLocal } from "@/workers/commit-block-header/state-queue.js";
import {
  deferProcessedCommitPayloadUntilConfirmation,
  recoverLocalFinalizationAgainstConfirmedBlock,
  submitDepositOnlyCommit,
  submitTxBackedCommit,
} from "@/workers/commit-block-header/submission.js";
import { makeEventCommitments } from "@/workers/commit-block-header/transition-commitments.js";
import {
  deserializeStateQueueUTxO,
  type SerializedStateQueueUTxO,
  WorkerInput,
  WorkerOutput,
} from "@/workers/utils/commit-block-header.js";
import {
  selectCommitTxCandidates,
  shouldDeferCommitSubmission,
  shouldSkipIdleCommitBehindUnmergedTail,
} from "@/workers/utils/commit-block-planner.js";
import { resolveExplicitCommitCandidateEndTimeMs } from "@/workers/utils/commit-end-time.js";
import {
  computeLedgerMpfRootFromLedgerEntries,
  hydrateLedgerMpfFromLedgerEntries,
  makeMpfs,
  type MidgardMpf,
  processMpfs,
  utxoToInsertBatchOp,
  withMpfRootTransactions,
} from "@/workers/utils/mpf.js";

const EXPLICIT_COMMIT_CONFIRMATION_TIMEOUT_MS = 120_000;
const EXPLICIT_COMMIT_CONFIRMATION_POLL_INTERVAL_MS = 5_000;
const EXPLICIT_COMMIT_BLOCK_VISIBILITY_DELAY = "5 seconds";
const EXPLICIT_COMMIT_BLOCK_VISIBILITY_RETRIES = 18;

class CommitWorkerInvariantError extends Data.TaggedError(
  "CommitWorkerInvariantError",
)<{
  readonly message: string;
}> {}

const provideCommitBlockWorkerServices = <A, E>(
  effect: Effect.Effect<A, E, MidgardContracts | Database | Lucid | NodeConfig>,
): Effect.Effect<A, E | ConfigError | DatabaseInitializationError, never> =>
  pipe(
    effect,
    Effect.provide(MidgardContracts.Default),
    Effect.provide(Database.layer),
    Effect.provide(Lucid.Default),
    Effect.provide(NodeConfig.layer),
  );

const establishEndTimeFromTxRequests = (
  candidateTxs: readonly TxTable.EntryWithTimeStamp[],
): Option.Option<Date> =>
  candidateTxs.length > 0
    ? Option.some(candidateTxs[0][TxColumns.TIMESTAMPTZ])
    : Option.none();

const pendingUserEventCountUpTo = (
  effectiveEndTime: Date,
): Effect.Effect<number, DatabaseError, Database> =>
  Effect.gen(function* () {
    const [depositEntries, forcedTransactionEntries, withdrawalEntries] =
      yield* Effect.all(
        [
          DepositsDB.retrievePendingHeaderEntriesUpTo(effectiveEndTime),
          ForcedTransactionsDB.retrievePendingHeaderEntriesUpTo(
            effectiveEndTime,
          ),
          WithdrawalsDB.retrievePendingHeaderEntriesUpTo(effectiveEndTime),
        ],
        { concurrency: "unbounded" },
      );
    return (
      depositEntries.length +
      forcedTransactionEntries.length +
      withdrawalEntries.length
    );
  });

type ResolvedCommitBaseLedgerEntries = {
  readonly source: string;
  readonly entries: readonly Ledger.MinimalEntry[];
  readonly root: string;
};

const resolveCommitBaseLedgerEntries = (
  availableConfirmedBlock: "" | SerializedStateQueueUTxO,
): Effect.Effect<
  ResolvedCommitBaseLedgerEntries,
  unknown,
  Database | NodeConfig
> =>
  Effect.gen(function* () {
    const confirmedEntries = yield* ConfirmedLedgerDB.retrieve;
    if (availableConfirmedBlock !== "") {
      const latestBlock = yield* deserializeStateQueueUTxO(
        availableConfirmedBlock,
      );
      if (latestBlock.datum.key === "Empty") {
        yield* Effect.logInfo(
          "🔹 Commit base state_queue tip is the confirmed-state root; using confirmed_ledger as base.",
        );
        if (confirmedEntries.length === 0) {
          const nodeConfig = yield* NodeConfig;
          const genesisEntries = yield* Effect.forEach(
            nodeConfig.GENESIS_UTXOS,
            (utxo) =>
              utxoToInsertBatchOp(utxo).pipe(
                Effect.map((op) => ({
                  [Ledger.Columns.OUTREF]: op.key,
                  [Ledger.Columns.OUTPUT]: op.value,
                })),
              ),
          );
          if (genesisEntries.length > 0) {
            const root =
              yield* computeLedgerMpfRootFromLedgerEntries(genesisEntries);
            yield* Effect.logInfo(
              `🔹 Commit base ledger snapshot resolved from configured genesis UTxOs (entries=${genesisEntries.length.toString()}).`,
            );
            return {
              source: "genesis",
              entries: genesisEntries,
              root,
            } satisfies ResolvedCommitBaseLedgerEntries;
          }
        }
      } else {
        const header = yield* SDK.getHeaderFromStateQueueDatum(
          latestBlock.datum,
        );
        const headerHash = yield* SDK.hashBlockHeader(header);
        const journal = yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
          Buffer.from(headerHash, "hex"),
        );

        if (Option.isSome(journal)) {
          const snapshot = yield* materializeConfirmedLedgerSnapshot(
            journal.value,
          );
          if (snapshot.root !== header.utxosRoot) {
            return yield* Effect.fail(
              new DatabaseError({
                table: PendingBlockFinalizationsDB.tableName,
                message:
                  "Refusing to use pending-finalization journal as commit base because its UTxO snapshot root does not match the state-queue tip",
                cause: `header_hash=${headerHash},journal_root=${snapshot.root},state_queue_root=${header.utxosRoot}`,
              }),
            );
          }
          yield* Effect.logInfo(
            `🔹 Commit base ledger snapshot resolved from pending-finalization journal ${headerHash} (entries=${snapshot.entries.length.toString()}).`,
          );
          return {
            source: `pending-finalization:${headerHash}`,
            entries: snapshot.entries,
            root: snapshot.root,
          } satisfies ResolvedCommitBaseLedgerEntries;
        }
      }
    }

    const confirmedRoot =
      yield* computeLedgerMpfRootFromLedgerEntries(confirmedEntries);
    yield* Effect.logInfo(
      `🔹 Commit base ledger snapshot resolved from confirmed_ledger (entries=${confirmedEntries.length.toString()}).`,
    );
    return {
      source: "confirmed_ledger",
      entries: confirmedEntries,
      root: confirmedRoot,
    } satisfies ResolvedCommitBaseLedgerEntries;
  });

const alignCommitMpfsToBase = ({
  ledgerMpf,
  transactionsMpf,
  base,
}: {
  readonly ledgerMpf: MidgardMpf;
  readonly transactionsMpf: MidgardMpf;
  readonly base: ResolvedCommitBaseLedgerEntries;
}): Effect.Effect<readonly Ledger.MinimalEntry[], unknown, never> =>
  Effect.gen(function* () {
    const currentLedgerRoot = yield* ledgerMpf.rootHex();
    if (currentLedgerRoot !== base.root) {
      const hydratedRoot = yield* hydrateLedgerMpfFromLedgerEntries(
        ledgerMpf,
        base.entries,
      );
      if (hydratedRoot !== base.root) {
        return yield* Effect.fail(
          new DatabaseError({
            table: PendingBlockFinalizationsDB.tableName,
            message:
              "Refusing to build a block because hydrating the commit ledger MPF did not reproduce the selected base root",
            cause: `source=${base.source},expected_root=${base.root},hydrated_root=${hydratedRoot}`,
          }),
        );
      }
      yield* Effect.logInfo(
        `🔹 Hydrated commit ledger MPF from ${base.source}: previous_root=${currentLedgerRoot},root=${hydratedRoot},entries=${base.entries.length.toString()}.`,
      );
    }

    const transactionsRootIsEmpty = yield* transactionsMpf.rootIsEmpty();
    if (!transactionsRootIsEmpty) {
      yield* transactionsMpf.resetToEmpty();
      yield* Effect.logInfo(
        `🔹 Reset per-block transactions MPF before building on ${base.source}.`,
      );
    }

    return base.entries;
  });

const shouldPreserveCommitMpfRoots = (output: WorkerOutput): boolean => {
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
    case "NothingToCommitOutput":
      return false;
  }
};

export type ExplicitBlockHeaderCommitParams = {
  readonly utxosRoot: string;
  readonly transactionsRoot: string;
  readonly depositsRoot: string;
  readonly withdrawalsRoot: string;
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
      new Promise<void>((resolve, reject) => {
        const timeoutId = setTimeout(() => {
          reject(
            new Error(
              `timed out waiting for explicit block-header commit confirmation after ${EXPLICIT_COMMIT_CONFIRMATION_TIMEOUT_MS}ms`,
            ),
          );
        }, EXPLICIT_COMMIT_CONFIRMATION_TIMEOUT_MS);
        lucid
          .awaitTx(txHash, EXPLICIT_COMMIT_CONFIRMATION_POLL_INTERVAL_MS)
          .then((confirmed) => {
            clearTimeout(timeoutId);
            if (confirmed) {
              resolve();
            } else {
              reject(new Error(`provider returned unconfirmed for ${txHash}`));
            }
          })
          .catch((error) => {
            clearTimeout(timeoutId);
            reject(error);
          });
      }),
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
  | SDK.HashingError
  | TxSignError
  | TxSubmitError,
  Lucid | MidgardContracts
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
      },
      {
        withdrawalCount: 0n,
        forcedTransactionCount: 0n,
        l2TransactionCount: 0n,
        depositCount: 0n,
      },
    );
    const { newHeaderHash, blockEndTimeMs, signAndSubmitProgram, txSize } =
      yield* buildUnsignedCommitTx(
        contracts,
        latestBlock,
        params.utxosRoot,
        params.transactionsRoot,
        params.depositsRoot,
        params.withdrawalsRoot,
        transitionCommitments,
        endTime,
      );

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

const databaseOperationsProgram = (
  workerInput: WorkerInput,
  ledgerMpf: MidgardMpf,
  transactionsMpf: MidgardMpf,
): Effect.Effect<
  WorkerOutput,
  unknown,
  MidgardContracts | Database | Lucid | NodeConfig
> =>
  Effect.gen(function* () {
    const mempoolTxs = yield* MempoolDB.retrieve;
    const currentBlockStartTime = new Date(
      workerInput.data.currentBlockStartTimeMs,
    );
    const processedPendingTxs = yield* ProcessedMempoolDB.retrieve;
    const candidateSelection = selectCommitTxCandidates({
      mempoolTxs,
      processedMempoolTxs: processedPendingTxs,
    });
    const depositIngestionBarrierTime =
      yield* fetchAndInsertDepositUTxOsForCommitBarrier(new Date());
    const withdrawalIngestionBarrierTime =
      yield* fetchAndInsertWithdrawalUTxOsForCommitBarrier(
        depositIngestionBarrierTime,
      );
    const txOrderIngestionBarrierTime =
      yield* fetchAndInsertTxOrderUTxOsForCommitBarrier(
        withdrawalIngestionBarrierTime,
      );
    const userEventOnlyEndTime = [
      depositIngestionBarrierTime,
      withdrawalIngestionBarrierTime,
      txOrderIngestionBarrierTime,
    ].reduce((earliest, candidate) =>
      candidate.getTime() < earliest.getTime() ? candidate : earliest,
    );

    const availableConfirmedBlock = workerInput.data.availableConfirmedBlock;
    const availableLocalFinalizationBlock =
      workerInput.data.availableLocalFinalizationBlock;
    const hasAvailableConfirmedBlock = availableConfirmedBlock !== "";
    const hasAvailableLocalFinalizationBlock =
      availableLocalFinalizationBlock !== "";
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
      return yield* recoverLocalFinalizationAgainstConfirmedBlock({
        latestBlock: recoverableConfirmedBlock,
        transactionsMpf,
        processedMempoolTxs: [],
        mempoolTxHashes: [],
        workerInput,
        sizeOfProcessedTxs: 0,
      });
    }

    const pendingUserEventCount =
      yield* pendingUserEventCountUpTo(userEventOnlyEndTime);
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
      return {
        type: "NothingToCommitOutput",
      } satisfies WorkerOutput;
    }

    const canBuildOnConfirmedBlock =
      hasAvailableConfirmedBlock && !workerInput.data.localFinalizationPending;
    const commitBase = yield* resolveCommitBaseLedgerEntries(
      availableConfirmedBlock,
    );
    const initialLedgerEntries = yield* alignCommitMpfsToBase({
      ledgerMpf,
      transactionsMpf,
      base: commitBase,
    });
    const processed = yield* processMpfs(
      ledgerMpf,
      transactionsMpf,
      candidateSelection.candidateTxs,
      {
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
          ? txOrderIngestionBarrierTime
          : undefined,
        depositOnlyEndTime: canBuildOnConfirmedBlock
          ? userEventOnlyEndTime
          : undefined,
        initialLedgerEntries,
      },
    );

    const {
      utxoRoot,
      txRoot,
      transitionTraceRoot,
      eventToStepRoot,
      transitionTraceMembers,
      eventToStepMembers,
      transitionStepCount,
      utxoPayloadEntries,
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
    } = processed;

    const processedMempoolTxs = processed.processedMempoolTxs;
    const mempoolTxHashes = processed.mempoolTxHashes;
    const sizeOfProcessedTxs = processed.sizeOfProcessedTxs;
    const mempoolTxSourceTable =
      candidateSelection.sourceTable === "processed_mempool"
        ? ProcessedMempoolDB.tableName
        : candidateSelection.sourceTable === "mempool"
          ? MempoolDB.tableName
          : "none";
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

    const contracts = yield* MidgardContracts;

    if (availableConfirmedBlock === "") {
      // The tx confirmation worker has not yet confirmed a previously
      // submitted tx, so the root we have found can not be used yet.
      // However, it is stored on disk in our LevelDB mempool. Therefore,
      // the processed txs must be transferred to `ProcessedMempoolDB` from
      // `MempoolDB`.
      if (mempoolTxSourceTable === ProcessedMempoolDB.tableName) {
        yield* Effect.logInfo(
          "🔹 No confirmed block available and selected tx payload is already durable in ProcessedMempoolDB; preserving it for the next commit attempt.",
        );
        return {
          type: "SkippedSubmissionOutput",
          mempoolTxsCount: 0,
          sizeOfProcessedTxs: 0,
        } satisfies WorkerOutput;
      }
      return yield* deferProcessedCommitPayloadUntilConfirmation({
        processedMempoolTxs,
        mempoolTxHashes,
        mempoolTxsCount,
        sizeOfProcessedTxs,
      });
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
        return yield* submitDepositOnlyCommit({
          contracts,
          latestBlock,
          endTime: userEventOnlyEndTime,
          includedDepositEntries,
          includedDepositEventIds,
          includedForcedTransactionEntries,
          includedForcedTransactionEventIds,
          includedWithdrawalEntries,
          includedWithdrawalEventIds,
          workerInput,
          utxoRoot,
          txRoot,
          transitionTraceRoot,
          eventToStepRoot,
          transitionTraceMembers,
          eventToStepMembers,
          transitionStepCount,
          utxoPayloadEntries,
        });
      } else {
        // One or more transactions found in either `ProcessedMempoolDB` or
        // `MempoolDB`. We use the latest transaction's timestamp as the upper
        // bound of the block we are about to submit.
        const endTime = optEndTime.value;

        yield* Effect.logInfo("🔹 Checking for user events...");
        return yield* submitTxBackedCommit({
          contracts,
          latestBlock,
          endTime,
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
          transitionTraceMembers,
          eventToStepMembers,
          transitionStepCount,
          utxoPayloadEntries,
          transactionsMpf,
          processedMempoolTxs,
          mempoolTxHashes,
          mempoolTxSourceTable,
          workerInput,
          sizeOfProcessedTxs,
        });
      }
    }
  });

// Export the production commit worker core so emulator tests can exercise the
// exact same effect graph without going through a worker-thread bootstrap.
export const runCommitBlockHeaderWorkerProgram = (
  workerInput: WorkerInput,
): Effect.Effect<
  WorkerOutput,
  unknown,
  MidgardContracts | Database | Lucid | NodeConfig
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("🔹 Retrieving all mempool transactions...");

    const { ledgerMpf, transactionsMpf } = yield* makeMpfs;
    const closeMpfs = Effect.all(
      [
        ledgerMpf.close().pipe(Effect.catchAll(() => Effect.void)),
        transactionsMpf.close().pipe(Effect.catchAll(() => Effect.void)),
      ],
      { discard: true },
    );

    const result = yield* withMpfRootTransactions(
      [ledgerMpf, transactionsMpf],
      databaseOperationsProgram(workerInput, ledgerMpf, transactionsMpf),
      shouldPreserveCommitMpfRoots,
    ).pipe(Effect.ensuring(closeMpfs));
    if (result === undefined) {
      return yield* Effect.fail(
        new CommitWorkerInvariantError({
          message:
            "Block commitment worker completed without producing a worker output",
        }),
      );
    }
    return result;
  });

if (parentPort !== null) {
  const inputData = workerData as WorkerInput;

  const program = provideCommitBlockWorkerServices(
    runCommitBlockHeaderWorkerProgram(inputData),
  );

  Effect.runPromise(
    program.pipe(
      Effect.catchAllCause((cause) =>
        Effect.succeed({
          type: "FailureOutput",
          error: `Block commitment worker failure: ${Cause.pretty(cause)}`,
        }),
      ),
    ),
  ).then((output) => {
    Effect.runSync(
      Effect.logInfo(
        `👷 Block commitment work completed (${JSON.stringify(output)}).`,
      ),
    );
    parentPort?.postMessage(output);
  });
}
