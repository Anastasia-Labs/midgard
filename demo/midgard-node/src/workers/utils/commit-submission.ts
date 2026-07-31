/**
 * Commit submission and local-finalization side effects for the block worker.
 * This module owns the database and MPF transitions that happen after a
 * commit transaction is submitted, recovered, deferred, or finalized locally.
 */
import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { fromHex } from "@lucid-evolution/lucid";
import { Duration, Effect, Metric, Option, Schedule } from "effect";

import { seedDaPayloadPublicationOutboxFromEnv } from "@/da/libp2p-producer.js";
import {
  BlocksDB,
  CekProgramMaterialDB,
  DaPayloadsDB,
  DepositsDB,
  ForcedTransactionsDB,
  ImmutableDB,
  MempoolDB,
  MempoolLedgerDB,
  MutationJobsDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  TxUtils as TxTable,
  WithdrawalsDB,
} from "@/database/index.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { Columns as TxColumns } from "@/database/utils/tx.js";
import { type Database, Lucid } from "@/services/index.js";
import { materializeConfirmedLedgerSnapshot } from "@/transactions/state-queue/confirmed-ledger-snapshot.js";
import type { TxSubmitError } from "@/transactions/utils.js";
import { batchProgram } from "@/utils.js";
import { buildDaPayloadInsert } from "@/workers/commit-block-header/da-payload.js";
import type {
  WorkerInput,
  WorkerOutput,
} from "@/workers/utils/commit-block-header.js";
import {
  buildSuccessfulCommitBatches,
  type SuccessfulCommitBatch,
} from "@/workers/utils/commit-block-planner.js";
import type { MidgardMpf, MpfError } from "@/workers/utils/mpf.js";

const BATCH_SIZE = 100;
const SKIPPED_SUBMISSION_TRANSFER_RETRIES = 2;
const SKIPPED_SUBMISSION_TRANSFER_INITIAL_BACKOFF = "250 millis";

const daPayloadBuildDurationTimer = Metric.timer(
  "da_payload_build_duration_ms",
  "Duration of full-state DA materialization and payload encoding before local-finalization SQL",
);
const localBlockFinalizationTransactionDurationTimer = Metric.timer(
  "local_block_finalization_transaction_duration_ms",
  "Duration of the local-finalization SQL transaction after DA payload bytes are complete",
);

const uniqueBuffersByHex = (buffers: readonly Buffer[]): readonly Buffer[] => [
  ...new Map(
    buffers.map((buffer) => [buffer.toString("hex"), buffer] as const),
  ).values(),
];

const localBlockFinalizationJobId = (headerHash: string): string =>
  `local_block_finalization:${headerHash}`;

const withLocalBlockFinalizationJob = <A, E, R>(
  input: {
    readonly headerHash: string;
    readonly mempoolTxCount: number;
    readonly includedDepositCount: number;
    readonly includedForcedTransactionCount: number;
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
        includedForcedTransactionCount: input.includedForcedTransactionCount,
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
): Effect.Effect<readonly string[], DatabaseError, Database> =>
  Effect.gen(function* () {
    if (includedWithdrawalEventIds.length <= 0) {
      return [];
    }
    const uniqueEventIds = uniqueBuffersByHex(includedWithdrawalEventIds);
    const withdrawals = yield* WithdrawalsDB.retrieveByEventIds(uniqueEventIds);
    if (withdrawals.length !== uniqueEventIds.length) {
      return yield* Effect.fail(
        new DatabaseError({
          table: WithdrawalsDB.tableName,
          message:
            "Failed to apply finalized withdrawal ledger effects because at least one withdrawal row is missing",
          cause: `requested=${uniqueEventIds.length},found=${withdrawals.length}`,
        }),
      );
    }

    const validWithdrawals = withdrawals.filter(
      (entry) =>
        entry[WithdrawalsDB.Columns.VALIDITY] ===
        WithdrawalsDB.Validity.WithdrawalIsValid,
    );
    if (validWithdrawals.length <= 0) {
      return [];
    }

    const consumedOutRefs = yield* Effect.forEach(
      validWithdrawals,
      WithdrawalsDB.toLedgerOutRef,
    );
    const consumedDepositEventIds =
      yield* MempoolLedgerDB.clearUTxOs(consumedOutRefs);
    yield* DepositsDB.markConsumedByEventIds(consumedDepositEventIds);
    return consumedOutRefs.map((outRef) => outRef.toString("hex"));
  });

export const finalizeCommittedBlockLocally = (
  transactionsMpf: MidgardMpf,
  mempoolTxs: readonly TxTable.EntryWithTimeStamp[],
  mempoolTxHashes: Buffer[],
  newHeaderHash: string,
  includedWithdrawalEventIds: readonly Buffer[] = [],
  options: {
    readonly processedMempoolTxsOverride?: readonly TxTable.EntryWithTimeStamp[];
    readonly useAmbientProcessedMempool?: boolean;
    readonly daPayloadRecord?: PendingBlockFinalizationsDB.Record;
    readonly beforeTransactionsMpfReset?: Effect.Effect<
      void,
      DatabaseError,
      Database
    >;
  } = {},
): Effect.Effect<readonly string[], DatabaseError | MpfError, Database> =>
  Effect.gen(function* () {
    const filterAlreadyCommittedTxs = (
      candidateBatches: readonly SuccessfulCommitBatch[],
    ): Effect.Effect<
      readonly SuccessfulCommitBatch[],
      DatabaseError,
      Database
    > =>
      Effect.gen(function* () {
        const candidateHashes = uniqueBuffersByHex(
          candidateBatches.flatMap((batch) => batch.blockTxHashes),
        );

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

    const processedMempoolTxs =
      options.processedMempoolTxsOverride ??
      (options.useAmbientProcessedMempool === false
        ? []
        : yield* ProcessedMempoolDB.retrieve);
    const batches = buildSuccessfulCommitBatches(
      mempoolTxs,
      mempoolTxHashes,
      processedMempoolTxs,
      Math.floor(BATCH_SIZE / 2),
    );
    const finalizedTxHashes = uniqueBuffersByHex(
      batches.flatMap((batch) => batch.blockTxHashes),
    );
    const filteredBatches = yield* filterAlreadyCommittedTxs(batches);
    const daPayloadBuildStartedAt = Date.now();
    const persistedDaPayload =
      options.daPayloadRecord === undefined
        ? undefined
        : yield* Effect.gen(function* () {
            const record = options.daPayloadRecord!;
            const snapshot = yield* materializeConfirmedLedgerSnapshot(record);
            return yield* buildDaPayloadInsert({
              record,
              utxos: snapshot.entries.map((entry) => ({
                outref: entry.outref,
                output: entry.output,
              })),
            });
          });
    if (persistedDaPayload !== undefined) {
      yield* daPayloadBuildDurationTimer(
        Effect.succeed(Duration.millis(Date.now() - daPayloadBuildStartedAt)),
      );
    }

    yield* Effect.logInfo(
      "🔹 Inserting included transactions into ImmutableDB and BlocksDB, clearing included txs from MempoolDB/ProcessedMempoolDB, and resetting the transactions MPF root marker...",
    );
    const sql = yield* SqlClient.SqlClient;
    const transactionStartedAt = Date.now();
    const mempoolLedgerDeletedOutRefHexes = yield* sql
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
          const processedTxHashes = processedMempoolTxs.map((entry) =>
            Buffer.from(entry[TxColumns.TX_ID]),
          );
          if (processedTxHashes.length > 0) {
            yield* ProcessedMempoolDB.clearTxs(processedTxHashes);
          }
          const deletedOutRefHexes =
            yield* applyFinalizedWithdrawalLedgerEffects(
              includedWithdrawalEventIds,
            );
          if (persistedDaPayload !== undefined) {
            yield* DaPayloadsDB.upsertAvailable(persistedDaPayload);
          }
          yield* CekProgramMaterialDB.releaseAdmissionOwnership(
            finalizedTxHashes,
          );
          return deletedOutRefHexes;
        }),
      )
      .pipe(
        sqlErrorToDatabaseError(
          "local_block_finalization",
          "Failed to finalize committed block locally",
        ),
        Effect.ensuring(
          localBlockFinalizationTransactionDurationTimer(
            Effect.succeed(Duration.millis(Date.now() - transactionStartedAt)),
          ),
        ),
      );
    if (persistedDaPayload !== undefined) {
      yield* seedDaPayloadPublicationOutboxFromEnv(persistedDaPayload);
    }
    if (options.beforeTransactionsMpfReset !== undefined) {
      yield* options.beforeTransactionsMpfReset;
    }
    yield* transactionsMpf.resetToEmpty();
    return mempoolLedgerDeletedOutRefHexes;
  }).pipe(
    Effect.tapError((error) =>
      Effect.gen(function* () {
        yield* Effect.logError(
          `🔹 Local commit finalization failed (header=${newHeaderHash},error=${formatUnknownError(error)})`,
        );
      }),
    ),
  );

export const successfulLocalFinalizationRecoveryProgram = (
  transactionsMpf: MidgardMpf,
  _mempoolTxs: readonly TxTable.EntryWithTimeStamp[],
  _mempoolTxHashes: Buffer[],
  confirmedHeaderHash: string,
  workerInput: WorkerInput,
  _sizeOfProcessedTxs: number,
  beforeTransactionsMpfReset?: Effect.Effect<void, DatabaseError, Database>,
): Effect.Effect<WorkerOutput, DatabaseError | MpfError, Database> =>
  Effect.gen(function* () {
    const confirmedHeaderHashBuffer = Buffer.from(fromHex(confirmedHeaderHash));
    const pendingRecord =
      yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
        confirmedHeaderHashBuffer,
      );
    if (Option.isNone(pendingRecord)) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Cannot recover local finalization without a durable pending block journal",
          cause: `header_hash=${confirmedHeaderHash}`,
        }),
      );
    }
    const record = pendingRecord.value;
    const finalizedDepositEventIds = record.depositEventIds;
    const finalizedForcedTransactionEventIds = record.forcedTransactionEventIds;
    const finalizedWithdrawalEventIds = record.withdrawalEventIds;
    const unknownTxMember = record.txMembers.find(
      (member) =>
        member[PendingBlockFinalizationsDB.MemberColumns.SOURCE_TABLE] !==
          MempoolDB.tableName &&
        member[PendingBlockFinalizationsDB.MemberColumns.SOURCE_TABLE] !==
          ProcessedMempoolDB.tableName,
    );
    if (unknownTxMember !== undefined) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Cannot recover local finalization because the journal contains an unknown tx source table",
          cause: `header_hash=${confirmedHeaderHash},source_table=${
            unknownTxMember[
              PendingBlockFinalizationsDB.MemberColumns.SOURCE_TABLE
            ]
          }`,
        }),
      );
    }
    const journalMempoolTxs = record.txMembers
      .filter(
        (member) =>
          member[PendingBlockFinalizationsDB.MemberColumns.SOURCE_TABLE] ===
          MempoolDB.tableName,
      )
      .map(PendingBlockFinalizationsDB.txMemberToEntry);
    const journalProcessedMempoolTxs = record.txMembers
      .filter(
        (member) =>
          member[PendingBlockFinalizationsDB.MemberColumns.SOURCE_TABLE] ===
          ProcessedMempoolDB.tableName,
      )
      .map(PendingBlockFinalizationsDB.txMemberToEntry);
    const journalMempoolTxHashes = journalMempoolTxs.map((entry) =>
      Buffer.from(entry[TxColumns.TX_ID]),
    );
    const journalTxsSize = record.txMembers.reduce(
      (total, member) =>
        total +
        member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR].length,
      0,
    );

    return yield* withLocalBlockFinalizationJob(
      {
        headerHash: confirmedHeaderHash,
        mempoolTxCount: record.txMembers.length,
        includedDepositCount: finalizedDepositEventIds.length,
        includedForcedTransactionCount:
          finalizedForcedTransactionEventIds.length,
        includedWithdrawalCount: finalizedWithdrawalEventIds.length,
      },
      Effect.gen(function* () {
        const mempoolLedgerDeletedOutRefHexes =
          yield* finalizeCommittedBlockLocally(
            transactionsMpf,
            journalMempoolTxs,
            journalMempoolTxHashes,
            confirmedHeaderHash,
            finalizedWithdrawalEventIds,
            {
              processedMempoolTxsOverride: journalProcessedMempoolTxs,
              useAmbientProcessedMempool: false,
              daPayloadRecord: record,
              beforeTransactionsMpfReset,
            },
          );
        yield* WithdrawalsDB.markFinalizedByEventIds(
          finalizedWithdrawalEventIds,
          confirmedHeaderHashBuffer,
        );
        yield* ForcedTransactionsDB.markFinalizedByEventIds(
          finalizedForcedTransactionEventIds,
          confirmedHeaderHashBuffer,
        );
        yield* PendingBlockFinalizationsDB.markFinalized(
          confirmedHeaderHashBuffer,
        );
        return {
          type: "SuccessfulLocalFinalizationRecoveryOutput" as const,
          finalizedHeaderHash: confirmedHeaderHash,
          mempoolTxsCount:
            record.txMembers.length + workerInput.data.mempoolTxsCountSoFar,
          sizeOfBlocksTxs:
            journalTxsSize + workerInput.data.sizeOfProcessedTxsSoFar,
          mempoolLedgerDeletedOutRefHexes,
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
  transactionsMpf: MidgardMpf,
  mempoolTxsCount: number,
  sizeOfProcessedTxs: number,
  err: TxSubmitError,
): Effect.Effect<WorkerOutput> =>
  Effect.gen(function* () {
    yield* Effect.logError(`🔹 ⚠️  Tx submit failed: ${err}`);
    yield* Effect.logError(
      "🔹 ⚠️  Transactions MPF root marker will be preserved for recovery.",
    );
    const diagnostics = yield* transactionsMpf
      .diagnostics()
      .pipe(Effect.catchAll(() => Effect.succeed({ entries: -1 })));
    yield* Effect.logInfo(
      `🔹 Transactions MPF diagnostics: entries=${diagnostics.entries}`,
    );
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
      const header = yield* SDK.getHeaderV1FromStateQueueDatum(block.datum);
      const headerHash = yield* SDK.hashBlockHeaderV1(header);
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
