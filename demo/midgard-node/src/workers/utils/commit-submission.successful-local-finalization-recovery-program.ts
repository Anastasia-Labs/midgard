import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { fromHex } from "@lucid-evolution/lucid";
import { Effect, Option, Schedule } from "effect";

import {
  ForcedTransactionsDB,
  MempoolDB,
  MutationJobsDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  TxUtils as TxTable,
  WithdrawalsDB,
} from "../../database/index.js";
import { DatabaseError } from "../../database/utils/common.js";
import { Columns as TxColumns } from "../../database/utils/tx.js";
import type { MidgardMpf, MpfError } from "../../mpf/index.js";
import { type Database } from "../../services/index.js";
import {
  findLandedBlock,
  readLandedStateQueue,
} from "../../services/landed-state-queue.js";
import type { TxSubmitError } from "../../transactions/utils.js";
import { batchProgram } from "../../utils.js";
import type { WorkerInput, WorkerOutput } from "./commit-block-header.js";
import { finalizeCommittedBlockLocally } from "./commit-submission.finalize-committed-block-locally.js";
import {
  BATCH_SIZE,
  SKIPPED_SUBMISSION_TRANSFER_INITIAL_BACKOFF,
  SKIPPED_SUBMISSION_TRANSFER_RETRIES,
  withLocalBlockFinalizationJob,
} from "./commit-submission.with-local-block-finalization-job.js";

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

    const recoveryOutput = (
      mempoolLedgerDeletedOutRefHexes: readonly string[],
    ): WorkerOutput => ({
      type: "SuccessfulLocalFinalizationRecoveryOutput",
      finalizedHeaderHash: confirmedHeaderHash,
      mempoolTxsCount:
        record.txMembers.length + workerInput.data.mempoolTxsCountSoFar,
      sizeOfBlocksTxs:
        journalTxsSize + workerInput.data.sizeOfProcessedTxsSoFar,
      mempoolLedgerDeletedOutRefHexes,
    });

    // markFinalized is the job's last durable step, so a finalized journal
    // proves the block was applied locally and only the job's completion
    // record (or the ack of either write) was lost. Applying the block again
    // would end in markFinalized refusing a journal that is no longer active,
    // on every tick. The first attempt's ledger deletes already reached the
    // parent as the full reload its failed output triggers.
    if (
      record[PendingBlockFinalizationsDB.Columns.STATUS] ===
      PendingBlockFinalizationsDB.Status.LocallyApplied
    ) {
      yield* MutationJobsDB.markCompleted(
        MutationJobsDB.localBlockFinalizationJobId(confirmedHeaderHash),
      );
      yield* Effect.logWarning(
        `🔹 Pending block journal ${confirmedHeaderHash} is already finalized; closed its local finalization job without applying the block again.`,
      );
      return recoveryOutput([]);
    }

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
        return recoveryOutput(mempoolLedgerDeletedOutRefHexes);
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
    yield* Effect.logError(`🔹 ⚠️  Tx submit failed: ${err.message}`);
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
): Effect.Effect<Option.Option<string>, never, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    const read = yield* readLandedStateQueue(stateQueueAuthValidator);
    if (read.kind !== "ok")
      return yield* Effect.fail(`${read.kind}: ${read.detail}`);
    const block = findLandedBlock(read.queue, expectedHeaderHash);
    if (block === undefined) return Option.none();
    yield* Effect.logWarning(
      `🔹 Submit errored but on-chain header ${expectedHeaderHash} is already present in canonical state_queue; recovering submission state.`,
    );
    return Option.some(block.element.utxo.txHash);
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
