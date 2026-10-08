import { SqlClient } from "@effect/sql";
import { fromHex } from "@lucid-evolution/lucid";
import { Duration, Effect, Option } from "effect";

import { seedDaPayloadPublicationOutboxFromEnv } from "../../da/libp2p-producer.js";
import { currentOwnedTransaction } from "../../database/eventHistoryAuthority.js";
import {
  BlocksDB,
  CekProgramMaterialDB,
  DaPayloadsDB,
  ImmutableDB,
  MempoolInclusionsDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  TxUtils as TxTable,
} from "../../database/index.js";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "../../database/utils/common.js";
import { Columns as TxColumns } from "../../database/utils/tx.js";
import type { MidgardMpf, MpfError } from "../../mpf/index.js";
import { withHistoryWrite } from "../../services/event-history-producer.js";
import { type Database } from "../../services/index.js";
import { materializeConfirmedLedgerSnapshot } from "../../transactions/state-queue/confirmed-ledger-snapshot.js";
import { buildDaPayloadInsert } from "../commit-block-header/da-payload.js";
import { pinFinalizedStateScriptRefs } from "../commit-block-header/da-payload.pin-finalized-state.js";
import {
  buildSuccessfulCommitBatches,
  type SuccessfulCommitBatch,
} from "./commit-block-planner.js";
import {
  applyFinalizedWithdrawalLedgerEffects,
  BATCH_SIZE,
  daPayloadBuildDurationTimer,
  describeLocalFinalizationFailure,
  localBlockFinalizationTransactionDurationTimer,
  uniqueBuffersByHex,
} from "./commit-submission.with-local-block-finalization-job.js";

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
            const insert = yield* buildDaPayloadInsert({
              record,
              utxos: snapshot.entries.map((entry) => ({
                outref: entry.outref,
                output: entry.output,
              })),
            });
            return {
              insert,
              outputs: snapshot.entries.map((entry) => entry.output),
            };
          });
    if (persistedDaPayload !== undefined) {
      yield* daPayloadBuildDurationTimer(
        Effect.succeed(Duration.millis(Date.now() - daPayloadBuildStartedAt)),
      );
    }

    yield* Effect.logInfo(
      "🔹 Inserting included transactions into ImmutableDB and BlocksDB, marking included txs in MempoolDB/ProcessedMempoolDB, and resetting the transactions MPF root marker...",
    );
    const sql = yield* SqlClient.SqlClient;
    const transactionStartedAt = Date.now();
    const mempoolLedgerDeletedOutRefHexes = yield* sql
      .withTransaction(
        Effect.gen(function* () {
          const journal =
            yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
              newHeaderHashBuffer,
            );
          if (
            Option.isNone(journal) &&
            Option.isSome(yield* currentOwnedTransaction)
          )
            return yield* Effect.fail(
              new DatabaseError({
                table: PendingBlockFinalizationsDB.tableName,
                message:
                  "Local finalization requires its durable event journal",
                cause: newHeaderHash,
              }),
            );
          if (Option.isSome(journal))
            yield* PendingBlockFinalizationsDB.assertCanonicalEventMembers(
              journal.value,
            );
          yield* Effect.forEach(
            filteredBatches,
            (batch, i) =>
              Effect.gen(function* () {
                // The block's rows stay, marked by its header, until the
                // block folds (or a rollback clears the mark).
                const clearMempoolProgram =
                  batch.clearMempoolTxHashes.length === 0
                    ? Effect.void
                    : MempoolInclusionsDB.markIncluded(newHeaderHashBuffer, [
                        ...batch.clearMempoolTxHashes,
                      ]).pipe(
                        Effect.withSpan(`mempool-db-mark-included-batch-${i}`),
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
          yield* MempoolInclusionsDB.markIncluded(
            newHeaderHashBuffer,
            processedTxHashes,
          );
          const deletedOutRefHexes =
            yield* applyFinalizedWithdrawalLedgerEffects(
              includedWithdrawalEventIds,
            );
          if (persistedDaPayload !== undefined) {
            yield* DaPayloadsDB.upsertAvailable(persistedDaPayload.insert);
            yield* pinFinalizedStateScriptRefs(
              options.daPayloadRecord!,
              persistedDaPayload.outputs,
            );
          }
          yield* CekProgramMaterialDB.releaseAdmissionOwnership(
            finalizedTxHashes,
          );
          return deletedOutRefHexes;
        }),
      )
      .pipe(
        withHistoryWrite,
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
      yield* seedDaPayloadPublicationOutboxFromEnv(persistedDaPayload.insert);
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
          `🔹 Local commit finalization failed (header=${newHeaderHash},error=${describeLocalFinalizationFailure(error)})`,
        );
      }),
    ),
  );
