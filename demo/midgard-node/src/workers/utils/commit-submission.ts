/**
 * Commit submission and local-finalization side effects for the block worker.
 * This module owns the database and trie transitions that happen after a
 * commit transaction is submitted, recovered, deferred, or finalized locally.
 */
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Option, Schedule } from "effect";
import {
  BlocksDB,
  DepositsDB,
  ImmutableDB,
  MempoolLedgerDB,
  MempoolDB,
  MutationJobsDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  TxUtils as TxTable,
  WithdrawalsDB,
} from "@/database/index.js";
import { Columns as TxColumns } from "@/database/utils/tx.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { formatUnknownError } from "@/error-format.js";
import { Lucid, type Database } from "@/services/index.js";
import { SqlClient } from "@effect/sql";
import type { TxSubmitError } from "@/transactions/utils.js";
import { batchProgram, type FileSystemError } from "@/utils.js";
import {
  buildSuccessfulCommitBatches,
  type SuccessfulCommitBatch,
} from "@/workers/utils/commit-block-planner.js";
import type {
  WorkerInput,
  WorkerOutput,
} from "@/workers/utils/commit-block-header.js";
import type { MidgardMpt } from "@/workers/utils/mpt.js";
import { fromHex } from "@lucid-evolution/lucid";

const BATCH_SIZE = 100;
const SKIPPED_SUBMISSION_TRANSFER_RETRIES = 2;
const SKIPPED_SUBMISSION_TRANSFER_INITIAL_BACKOFF = "250 millis";

const localBlockFinalizationJobId = (headerHash: string): string =>
  `local_block_finalization:${headerHash}`;

const withLocalBlockFinalizationJob = <A, E, R>(
  input: {
    readonly headerHash: string;
    readonly mempoolTxCount: number;
    readonly includedDepositCount: number;
    readonly includedWithdrawalCount: number;
  },
  program: Effect.Effect<A, E, R>,
): Effect.Effect<A, E | DatabaseError, R | Database> => {
  const jobId = localBlockFinalizationJobId(input.headerHash);
  return Effect.gen(function* () {
    yield* MutationJobsDB.start({
      jobId,
      kind: MutationJobsDB.Kind.LocalBlockFinalization,
      payload: {
        headerHash: input.headerHash,
        mempoolTxCount: input.mempoolTxCount,
        includedDepositCount: input.includedDepositCount,
        includedWithdrawalCount: input.includedWithdrawalCount,
      },
    });
    const result = yield* program;
    yield* MutationJobsDB.markCompleted(jobId);
    return result;
  }).pipe(
    Effect.tapError((error) =>
      MutationJobsDB.markFailed(jobId, formatUnknownError(error)).pipe(
        Effect.catchAll(() => Effect.void),
      ),
    ),
    Effect.tapError((error) =>
      Effect.logError(
        `🔹 Local block finalization job failed (job=${jobId},error=${formatUnknownError(error)})`,
      ),
    ),
  );
};

const applyFinalizedWithdrawalLedgerEffects = (
  includedWithdrawalEventIds: readonly Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (includedWithdrawalEventIds.length <= 0) {
      return;
    }
    const uniqueEventIdHexes = Array.from(
      new Set(includedWithdrawalEventIds.map((id) => id.toString("hex"))),
    );
    const withdrawals = yield* WithdrawalsDB.retrieveByEventIds(
      uniqueEventIdHexes.map((hex) => Buffer.from(hex, "hex")),
    );
    if (withdrawals.length !== uniqueEventIdHexes.length) {
      return yield* Effect.fail(
        new DatabaseError({
          table: WithdrawalsDB.tableName,
          message:
            "Failed to apply finalized withdrawal ledger effects because at least one withdrawal row is missing",
          cause: `requested=${uniqueEventIdHexes.length},found=${withdrawals.length}`,
        }),
      );
    }

    const validWithdrawals = withdrawals.filter(
      (entry) =>
        entry[WithdrawalsDB.Columns.VALIDITY] ===
        WithdrawalsDB.Validity.WithdrawalIsValid,
    );
    if (validWithdrawals.length <= 0) {
      return;
    }

    const consumedOutRefs = yield* Effect.forEach(
      validWithdrawals,
      WithdrawalsDB.toLedgerOutRef,
    );
    const consumedDepositEventIds =
      yield* MempoolLedgerDB.clearUTxOs(consumedOutRefs);
    yield* DepositsDB.markConsumedByEventIds(consumedDepositEventIds);
  });

export const finalizeCommittedBlockLocally = (
  mempoolTrie: MidgardMpt,
  mempoolTxs: readonly TxTable.EntryWithTimeStamp[],
  mempoolTxHashes: Buffer[],
  includedDepositEventIds: readonly Buffer[],
  newHeaderHash: string,
  includedWithdrawalEventIds: readonly Buffer[] = [],
): Effect.Effect<void, DatabaseError | FileSystemError, Database> =>
  Effect.gen(function* () {
    const filterAlreadyCommittedTxs = (
      candidateBatches: readonly SuccessfulCommitBatch[],
    ): Effect.Effect<
      readonly SuccessfulCommitBatch[],
      DatabaseError,
      Database
    > =>
      Effect.gen(function* () {
        const candidateHashes = Array.from(
          new Set(
            candidateBatches
              .flatMap((batch) => batch.blockTxHashes)
              .map((hash) => hash.toString("hex")),
          ),
        ).map((hex) => Buffer.from(hex, "hex"));

        if (candidateHashes.length <= 0) {
          return candidateBatches;
        }

        const existing =
          yield* ImmutableDB.retrieveTxEntriesByHashes(candidateHashes);
        if (existing.length <= 0) {
          return candidateBatches;
        }

        const alreadyCommitted = new Set(
          existing.map((entry) => entry[TxColumns.TX_ID].toString("hex")),
        );
        yield* Effect.logWarning(
          `🔹 Filtering ${alreadyCommitted.size} already-committed tx id(s) from local finalization payload before BlocksDB insertion.`,
        );

        return candidateBatches.map((batch) => {
          const filteredTxs: TxTable.EntryWithTimeStamp[] = [];
          const filteredHashes: Buffer[] = [];

          for (let i = 0; i < batch.blockTxHashes.length; i += 1) {
            const txHash = batch.blockTxHashes[i];
            if (alreadyCommitted.has(txHash.toString("hex"))) {
              continue;
            }
            filteredHashes.push(txHash);
            if (i < batch.txsToInsertImmutable.length) {
              filteredTxs.push(batch.txsToInsertImmutable[i]);
            }
          }

          return {
            txsToInsertImmutable: filteredTxs,
            blockTxHashes: filteredHashes,
            clearMempoolTxHashes: batch.clearMempoolTxHashes,
          };
        });
      });

    const newHeaderHashBuffer = Buffer.from(fromHex(newHeaderHash));

    const processedMempoolTxs = yield* ProcessedMempoolDB.retrieve;
    const batches = buildSuccessfulCommitBatches(
      mempoolTxs,
      mempoolTxHashes,
      processedMempoolTxs,
      Math.floor(BATCH_SIZE / 2),
    );
    const filteredBatches = yield* filterAlreadyCommittedTxs(batches);

    yield* Effect.logInfo(
      "🔹 Inserting included transactions into ImmutableDB and BlocksDB, clearing all the processed txs from MempoolDB and ProcessedMempoolDB, and deleting mempool LevelDB...",
    );
    const sql = yield* SqlClient.SqlClient;
    yield* sql
      .withTransaction(
        Effect.gen(function* () {
          yield* Effect.forEach(
            filteredBatches,
            (batch, i) =>
              Effect.gen(function* () {
                const clearMempoolProgram =
                  batch.clearMempoolTxHashes.length === 0
                    ? Effect.void
                    : MempoolDB.clearTxs([...batch.clearMempoolTxHashes]).pipe(
                        Effect.withSpan(`mempool-db-clear-txs-batch-${i}`),
                      );
                yield* ImmutableDB.insertTxsValidatedNative([
                  ...batch.txsToInsertImmutable,
                ]).pipe(Effect.withSpan(`immutable-db-insert-batch-${i}`));
                yield* BlocksDB.insert(newHeaderHashBuffer, [
                  ...batch.blockTxHashes,
                ]).pipe(Effect.withSpan(`blocks-db-insert-batch-${i}`));
                yield* clearMempoolProgram;
              }),
            {
              concurrency: 1,
            },
          );
          yield* ProcessedMempoolDB.clear;
          yield* applyFinalizedWithdrawalLedgerEffects(
            includedWithdrawalEventIds,
          );
        }),
      )
      .pipe(
        sqlErrorToDatabaseError(
          "local_block_finalization",
          "Failed to finalize committed block locally",
        ),
      );
    yield* mempoolTrie.delete();
  }).pipe(
    Effect.tapError((error) =>
      Effect.gen(function* () {
        yield* Effect.logError(
          `🔹 Local commit finalization failed (header=${newHeaderHash},error=${formatUnknownError(error)})`,
        );
      }),
    ),
  );

export const successfulSubmissionProgram = (
  mempoolTrie: MidgardMpt,
  mempoolTxs: readonly TxTable.EntryWithTimeStamp[],
  mempoolTxHashes: Buffer[],
  includedDepositEventIds: readonly Buffer[],
  newHeaderHash: string,
  workerInput: WorkerInput,
  txSize: number,
  sizeOfProcessedTxs: number,
  txHash: string,
  blockEndTimeMs: number,
  includedWithdrawalEventIds: readonly Buffer[] = [],
): Effect.Effect<WorkerOutput, DatabaseError | FileSystemError, Database> =>
  withLocalBlockFinalizationJob(
    {
      headerHash: newHeaderHash,
      mempoolTxCount: mempoolTxs.length,
      includedDepositCount: includedDepositEventIds.length,
      includedWithdrawalCount: includedWithdrawalEventIds.length,
    },
    Effect.gen(function* () {
      yield* finalizeCommittedBlockLocally(
        mempoolTrie,
        mempoolTxs,
        mempoolTxHashes,
        includedDepositEventIds,
        newHeaderHash,
        includedWithdrawalEventIds,
      );
      yield* DepositsDB.markProjectedByEventIds(
        includedDepositEventIds,
        Buffer.from(fromHex(newHeaderHash)),
      );
      yield* WithdrawalsDB.markProjectedByEventIds(
        includedWithdrawalEventIds,
        Buffer.from(fromHex(newHeaderHash)),
      );
      yield* PendingBlockFinalizationsDB.markLocalFinalizationComplete(
        Buffer.from(fromHex(newHeaderHash)),
      );

      return {
        type: "SuccessfulSubmissionOutput",
        submittedTxHash: txHash,
        txSize,
        mempoolTxsCount:
          mempoolTxs.length + workerInput.data.mempoolTxsCountSoFar,
        sizeOfBlocksTxs:
          sizeOfProcessedTxs + workerInput.data.sizeOfProcessedTxsSoFar,
        blockEndTimeMs,
      };
    }),
  );

export const successfulLocalFinalizationRecoveryProgram = (
  mempoolTrie: MidgardMpt,
  mempoolTxs: readonly TxTable.EntryWithTimeStamp[],
  mempoolTxHashes: Buffer[],
  includedDepositEventIds: readonly Buffer[],
  confirmedHeaderHash: string,
  workerInput: WorkerInput,
  sizeOfProcessedTxs: number,
  includedWithdrawalEventIds: readonly Buffer[] = [],
): Effect.Effect<WorkerOutput, DatabaseError | FileSystemError, Database> =>
  Effect.gen(function* () {
    const confirmedHeaderHashBuffer = Buffer.from(fromHex(confirmedHeaderHash));
    const pendingRecord =
      yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
        confirmedHeaderHashBuffer,
      );
    const finalizedDepositEventIds = Option.isSome(pendingRecord)
      ? pendingRecord.value.depositEventIds
      : includedDepositEventIds;
    const finalizedWithdrawalEventIds = Option.isSome(pendingRecord)
      ? pendingRecord.value.withdrawalEventIds
      : includedWithdrawalEventIds;

    return yield* withLocalBlockFinalizationJob(
      {
        headerHash: confirmedHeaderHash,
        mempoolTxCount: mempoolTxs.length,
        includedDepositCount: finalizedDepositEventIds.length,
        includedWithdrawalCount: finalizedWithdrawalEventIds.length,
      },
      Effect.gen(function* () {
        yield* finalizeCommittedBlockLocally(
          mempoolTrie,
          mempoolTxs,
          mempoolTxHashes,
          finalizedDepositEventIds,
          confirmedHeaderHash,
          finalizedWithdrawalEventIds,
        );
        yield* WithdrawalsDB.markFinalizedByEventIds(
          finalizedWithdrawalEventIds,
          confirmedHeaderHashBuffer,
        );
        yield* PendingBlockFinalizationsDB.markFinalized(
          confirmedHeaderHashBuffer,
        );
        return {
          type: "SuccessfulLocalFinalizationRecoveryOutput" as const,
          mempoolTxsCount:
            mempoolTxs.length + workerInput.data.mempoolTxsCountSoFar,
          sizeOfBlocksTxs:
            sizeOfProcessedTxs + workerInput.data.sizeOfProcessedTxsSoFar,
        } satisfies WorkerOutput;
      }),
    );
  });

export const skippedSubmissionProgram = (
  mempoolTxs: readonly TxTable.EntryWithTimeStamp[],
  mempoolTxHashes: Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (mempoolTxs.length !== mempoolTxHashes.length) {
      return yield* Effect.fail(
        new DatabaseError({
          message:
            "Failed to transfer deferred commit payload: tx metadata length mismatch",
          cause: `mempool_txs=${mempoolTxs.length},mempool_tx_hashes=${mempoolTxHashes.length}`,
          table: "mempool,processed_mempool",
        }),
      );
    }
    yield* batchProgram(
      BATCH_SIZE,
      mempoolTxs.length,
      "skipped-submission-db-transfer",
      (startIndex: number, endIndex: number) =>
        Effect.gen(function* () {
          const batchTxs = mempoolTxs.slice(startIndex, endIndex);
          const batchHashes = mempoolTxHashes.slice(startIndex, endIndex);
          yield* ProcessedMempoolDB.insertTxs(batchTxs).pipe(
            Effect.withSpan(`processed-mempool-db-insert-${startIndex}`),
          );
          yield* MempoolDB.clearTxs(batchHashes).pipe(
            Effect.withSpan(`mempool-db-clear-txs-${startIndex}`),
          );
        }),
      1,
    );
  }).pipe(
    Effect.retry(
      Schedule.compose(
        Schedule.exponential(SKIPPED_SUBMISSION_TRANSFER_INITIAL_BACKOFF),
        Schedule.recurs(SKIPPED_SUBMISSION_TRANSFER_RETRIES),
      ),
    ),
  );

export const failedSubmissionProgram = (
  mempoolTrie: MidgardMpt,
  mempoolTxsCount: number,
  sizeOfProcessedTxs: number,
  err: TxSubmitError,
): Effect.Effect<WorkerOutput> =>
  Effect.gen(function* () {
    yield* Effect.logError(`🔹 ⚠️  Tx submit failed: ${err}`);
    yield* Effect.logError(
      "🔹 ⚠️  Mempool trie will be preserved, but db will be cleared.",
    );
    yield* Effect.logInfo("🔹 Mempool Trie stats:");
    console.dir(mempoolTrie.databaseStats(), { depth: null });
    return {
      type: "SkippedSubmissionOutput",
      mempoolTxsCount,
      sizeOfProcessedTxs,
    };
  });

export const recoverSubmittedTxHashByHeaderProgram = (
  stateQueueAuthValidator: SDK.AuthenticatedValidator,
  expectedHeaderHash: string,
): Effect.Effect<Option.Option<string>, never, Lucid> =>
  Effect.gen(function* () {
    const lucid = yield* Lucid;
    const fetchConfig: SDK.StateQueueFetchConfig = {
      stateQueueAddress: stateQueueAuthValidator.spendingScriptAddress,
      stateQueuePolicyId: stateQueueAuthValidator.policyId,
    };
    const sortedBlocks = yield* SDK.fetchSortedStateQueueUTxOsProgram(
      lucid.api,
      fetchConfig,
    );
    for (const block of sortedBlocks) {
      if (block.datum.key === "Empty") {
        continue;
      }
      const header = yield* SDK.getHeaderFromStateQueueDatum(block.datum);
      const headerHash = yield* SDK.hashBlockHeader(header);
      if (headerHash === expectedHeaderHash) {
        yield* Effect.logWarning(
          `🔹 Submit errored but on-chain header ${expectedHeaderHash} is already present in canonical state_queue; recovering submission state.`,
        );
        return Option.some(block.utxo.txHash);
      }
    }
    return Option.none();
  }).pipe(
    Effect.catchAll((error) =>
      Effect.gen(function* () {
        yield* Effect.logWarning(
          `🔹 Could not verify submit recovery on-chain: ${formatUnknownError(error)}`,
        );
        return Option.none<string>();
      }),
    ),
  );
