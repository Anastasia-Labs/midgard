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
  MempoolDB,
  ProcessedMempoolDB,
  TxUtils as TxTable,
} from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { Columns as TxColumns } from "@/database/utils/tx.js";
import { fetchAndInsertDepositUTxOsForCommitBarrier } from "@/fibers/fetch-and-insert-deposit-utxos.js";
import { fetchAndInsertWithdrawalUTxOsForCommitBarrier } from "@/fibers/fetch-and-insert-withdrawal-utxos.js";
import {
  ConfigError,
  Database,
  DatabaseInitializationError,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import { type TxSignError, type TxSubmitError } from "@/transactions/utils.js";
import { buildUnsignedCommitTx } from "@/workers/commit-block-header/build-unsigned-tx.js";
import { fetchLatestCommittedBlockLocal } from "@/workers/commit-block-header/state-queue.js";
import {
  deferProcessedCommitPayloadUntilConfirmation,
  recoverLocalFinalizationAgainstConfirmedBlock,
  submitDepositOnlyCommit,
  submitTxBackedCommit,
} from "@/workers/commit-block-header/submission.js";
import {
  deserializeStateQueueUTxO,
  WorkerInput,
  WorkerOutput,
} from "@/workers/utils/commit-block-header.js";
import { shouldDeferCommitSubmission } from "@/workers/utils/commit-block-planner.js";
import { resolveExplicitCommitCandidateEndTimeMs } from "@/workers/utils/commit-end-time.js";
import {
  makeMpfs,
  type MidgardMpf,
  processMpfs,
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
  mempoolTxs: readonly TxTable.EntryWithTimeStamp[],
): Effect.Effect<Option.Option<Date>, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (mempoolTxs.length > 0) {
      yield* Effect.logInfo(`🔹 ${mempoolTxs.length} retrieved.`);
      return Option.some(mempoolTxs[0][TxColumns.TIMESTAMPTZ]);
    }

    yield* Effect.logInfo(
      "🔹 No transactions were found in MempoolDB, checking ProcessedMempoolDB...",
    );
    const processedMempoolTxs = yield* ProcessedMempoolDB.retrieve;
    if (processedMempoolTxs.length <= 0) {
      // No transaction requests are available for inclusion in a block. By
      // setting `endTime` to `undefined` here, the code below can decide
      // whether it can stop if no
      return Option.none();
    }

    // No new transactions received, but there are uncommitted transactions
    // in the transactions MPF. So its root must be used to submit a new block, and if
    // successful, `ProcessedMempoolDB` must be cleared. Following functions
    // should work fine with 0 mempool txs.
    return Option.some(processedMempoolTxs[0][TxColumns.TIMESTAMPTZ]);
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
    const { newHeaderHash, blockEndTimeMs, signAndSubmitProgram, txSize } =
      yield* buildUnsignedCommitTx(
        contracts,
        latestBlock,
        params.utxosRoot,
        params.transactionsRoot,
        params.depositsRoot,
        params.withdrawalsRoot,
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
    const nodeConfig = yield* NodeConfig;
    const mempoolTxs = yield* MempoolDB.retrieve;
    const currentBlockStartTime = new Date(
      workerInput.data.currentBlockStartTimeMs,
    );
    const processedPendingTxs =
      mempoolTxs.length > 0 ? [] : yield* ProcessedMempoolDB.retrieve;
    const processedPendingTxHashes = processedPendingTxs.map((entry) =>
      Buffer.from(entry[TxColumns.TX_ID]),
    );
    const processedPendingTxsSize = processedPendingTxs.reduce(
      (total, entry) => total + entry[TxColumns.TX].length,
      0,
    );
    const depositIngestionBarrierTime =
      yield* fetchAndInsertDepositUTxOsForCommitBarrier(new Date());
    const withdrawalIngestionBarrierTime =
      yield* fetchAndInsertWithdrawalUTxOsForCommitBarrier(
        depositIngestionBarrierTime,
      );
    const userEventOnlyEndTime =
      depositIngestionBarrierTime.getTime() <=
      withdrawalIngestionBarrierTime.getTime()
        ? depositIngestionBarrierTime
        : withdrawalIngestionBarrierTime;

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

    const canBuildOnConfirmedBlock =
      hasAvailableConfirmedBlock && !workerInput.data.localFinalizationPending;
    const processed = yield* processMpfs(
      ledgerMpf,
      transactionsMpf,
      mempoolTxs,
      {
        currentBlockStartTime: canBuildOnConfirmedBlock
          ? currentBlockStartTime
          : undefined,
        processedOnlyEndTime: processedPendingTxs[0]?.[TxColumns.TIMESTAMPTZ],
        depositVisibilityBarrierTime: canBuildOnConfirmedBlock
          ? depositIngestionBarrierTime
          : undefined,
        withdrawalVisibilityBarrierTime: canBuildOnConfirmedBlock
          ? withdrawalIngestionBarrierTime
          : undefined,
        depositOnlyEndTime: canBuildOnConfirmedBlock
          ? userEventOnlyEndTime
          : undefined,
        tolerateMissingLedgerDeletes: nodeConfig.SKIP_TX_VALIDATION,
      },
    );

    const {
      utxoRoot,
      txRoot,
      rejectedMempoolTxsCount,
      includedDepositEntriesCount,
      includedDepositEntries,
      includedDepositEventIds,
      includedWithdrawalEntriesCount,
      includedWithdrawalEntries,
      includedWithdrawalEventIds,
    } = processed;

    const useDeferredProcessedPayload =
      mempoolTxs.length === 0 &&
      processed.processedMempoolTxs.length === 0 &&
      processedPendingTxs.length > 0 &&
      (availableConfirmedBlock !== "" ||
        workerInput.data.localFinalizationPending);
    const processedMempoolTxs = useDeferredProcessedPayload
      ? processedPendingTxs
      : processed.processedMempoolTxs;
    const mempoolTxHashes = useDeferredProcessedPayload
      ? processedPendingTxHashes
      : processed.mempoolTxHashes;
    const sizeOfProcessedTxs = useDeferredProcessedPayload
      ? processedPendingTxsSize
      : processed.sizeOfProcessedTxs;
    const mempoolTxSourceTable = useDeferredProcessedPayload
      ? ProcessedMempoolDB.tableName
      : MempoolDB.tableName;
    if (useDeferredProcessedPayload) {
      yield* Effect.logWarning(
        `🔹 Reusing ${processedPendingTxs.length.toString()} deferred processed tx(s) as the tx-backed commit journal payload.`,
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

    const mempoolTxsCount = processedMempoolTxs.length;
    const optEndTime: Option.Option<Date> =
      yield* establishEndTimeFromTxRequests(processedMempoolTxs);

    const contracts = yield* MidgardContracts;

    if (availableConfirmedBlock === "") {
      // The tx confirmation worker has not yet confirmed a previously
      // submitted tx, so the root we have found can not be used yet.
      // However, it is stored on disk in our LevelDB mempool. Therefore,
      // the processed txs must be transferred to `ProcessedMempoolDB` from
      // `MempoolDB`.
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
          includedWithdrawalEntries,
          includedWithdrawalEventIds,
          workerInput,
          utxoRoot,
          txRoot,
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
          includedWithdrawalEntries,
          includedWithdrawalEventIds,
          utxoRoot,
          txRoot,
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
