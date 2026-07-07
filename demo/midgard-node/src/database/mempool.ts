import { SqlClient } from "@effect/sql";
import { Duration, Effect, Metric } from "effect";

import * as AddressHistoryDB from "@/database/addressHistory.js";
import {
  clearTable,
  DatabaseError,
  logDatabaseError,
  retrieveNumberOfEntries,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import * as Ledger from "@/database/utils/ledger.js";
import * as Tx from "@/database/utils/tx.js";
import { Database } from "@/services/database.js";
import { ProcessedTx } from "@/utils.js";

import * as DepositsDB from "./deposits.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import * as MempoolTxDeltasDB from "./mempoolTxDeltas.js";

export const tableName = "mempool";

const mempoolPersistTxRowsDurationTimer = Metric.timer(
  "mempool_persist_tx_rows_duration",
  "Duration of accepted transaction row inserts into mempool",
);

const mempoolPersistProducedDurationTimer = Metric.timer(
  "mempool_persist_produced_duration",
  "Duration of accepted produced UTxO inserts into mempool_ledger",
);

const mempoolPersistDeltasDurationTimer = Metric.timer(
  "mempool_persist_deltas_duration",
  "Duration of accepted mempool tx delta upserts",
);

const mempoolPersistSpentDurationTimer = Metric.timer(
  "mempool_persist_spent_duration",
  "Duration of accepted spent-input deletion from mempool_ledger",
);

const mempoolPersistAddressHistoryDurationTimer = Metric.timer(
  "mempool_persist_address_history_duration",
  "Duration of accepted address-history persistence",
);

const toTxDelta = (processedTx: ProcessedTx): MempoolTxDeltasDB.TxDelta => ({
  txId: processedTx.txId,
  spent: processedTx.spent.map((outRef) => Buffer.from(outRef)),
  produced: processedTx.produced.map((entry) => ({
    [Ledger.Columns.OUTREF]: Buffer.from(entry[Ledger.Columns.OUTREF]),
    [Ledger.Columns.OUTPUT]: Buffer.from(entry[Ledger.Columns.OUTPUT]),
  })),
});

export const insert = (
  processedTx: ProcessedTx,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const { txId, txCbor, spent, produced } = processedTx;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        // Insert the tx itself in `MempoolDB`.
        yield* Tx.insertEntry(tableName, {
          tx_id: txId,
          tx: txCbor,
        });
        // Insert produced UTxOs in `MempoolLedgerDB`.
        yield* MempoolLedgerDB.insert(produced);
        yield* MempoolTxDeltasDB.upsertMany([toTxDelta(processedTx)]);
        // Remove spent inputs from MempoolLedgerDB.
        const consumedDepositEventIds =
          yield* MempoolLedgerDB.clearUTxOs(spent);
        yield* DepositsDB.markConsumedByEventIds(consumedDepositEventIds);
        // Add handled addresses to the lookup table
        yield* AddressHistoryDB.insert(spent, produced);
      }),
    );
  }).pipe(
    Effect.withLogSpan(`insert ${tableName}`),
    Effect.tapError((e) => logDatabaseError(tableName, "insert", e)),
    sqlErrorToDatabaseError(tableName, "Failed to insert mempool transaction"),
  );

export const insertMultiple = (
  processedTxs: ProcessedTx[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (processedTxs.length === 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    const txEntries = processedTxs.map((v) => ({
      tx_id: v.txId,
      tx: v.txCbor,
    }));
    yield* sql.withTransaction(
      Effect.gen(function* () {
        // Insert the tx itself in `MempoolDB`.
        const txRowsStartedAt = Date.now();
        yield* Tx.insertEntries(tableName, txEntries);
        yield* mempoolPersistTxRowsDurationTimer(
          Effect.succeed(Duration.millis(Date.now() - txRowsStartedAt)),
        );

        const allProduced = processedTxs.flatMap((tx) => tx.produced);
        const allSpent = processedTxs.flatMap((tx) => tx.spent);

        // Insert produced UTxOs in `MempoolLedgerDB`.
        const producedStartedAt = Date.now();
        yield* MempoolLedgerDB.insert(allProduced);
        yield* mempoolPersistProducedDurationTimer(
          Effect.succeed(Duration.millis(Date.now() - producedStartedAt)),
        );
        const deltasStartedAt = Date.now();
        yield* MempoolTxDeltasDB.upsertMany(processedTxs.map(toTxDelta));
        yield* mempoolPersistDeltasDurationTimer(
          Effect.succeed(Duration.millis(Date.now() - deltasStartedAt)),
        );
        // Remove spent inputs from MempoolLedgerDB.
        const spentStartedAt = Date.now();
        const consumedDepositEventIds =
          yield* MempoolLedgerDB.clearUTxOs(allSpent);
        yield* DepositsDB.markConsumedByEventIds(consumedDepositEventIds);
        yield* mempoolPersistSpentDurationTimer(
          Effect.succeed(Duration.millis(Date.now() - spentStartedAt)),
        );
        // Update AddressHistoryDB
        const addressHistoryStartedAt = Date.now();
        yield* AddressHistoryDB.insert(allSpent, allProduced);
        yield* mempoolPersistAddressHistoryDurationTimer(
          Effect.succeed(Duration.millis(Date.now() - addressHistoryStartedAt)),
        );
      }),
    );
  }).pipe(
    Effect.withLogSpan(`insert ${tableName}`),
    Effect.tapError((e) => logDatabaseError(tableName, "insert", e)),
    sqlErrorToDatabaseError(tableName, "Failed to insert mempool transactions"),
  );

/**
 * Retrieves mempool transaction CBOR by transaction hash.
 */
export const retrieveTxCborByHash = (txHash: Buffer) =>
  Tx.retrieveValue(tableName, txHash);

/**
 * Retrieves mempool transaction CBOR blobs for a batch of hashes.
 */
export const retrieveTxCborsByHashes = (txHashes: Buffer[]) =>
  Tx.retrieveValues(tableName, txHashes);

export const retrieve: Effect.Effect<
  readonly Tx.EntryWithTimeStamp[],
  DatabaseError,
  Database
> = Effect.gen(function* () {
  yield* Effect.logDebug(`${tableName} db: attempt to retrieve keyValues`);
  const sql = yield* SqlClient.SqlClient;
  return yield* sql<Tx.EntryWithTimeStamp>`SELECT ${sql(
    Tx.Columns.TX_ID,
  )}, ${sql(Tx.Columns.TX)}, ${sql(Tx.Columns.TIMESTAMPTZ)} FROM ${sql(tableName)} ORDER BY ${sql(Tx.Columns.TIMESTAMPTZ)} DESC LIMIT 100000`;
}).pipe(
  Effect.withLogSpan(`retrieve ${tableName}`),
  Effect.tapErrorTag("SqlError", (e) =>
    logDatabaseError(tableName, "retrieve", e),
  ),
  sqlErrorToDatabaseError(tableName, "Failed to retrieve given transactions"),
);

export const retrieveTxCount: Effect.Effect<bigint, DatabaseError, Database> =
  retrieveNumberOfEntries(tableName);

export const clearTxs = (
  txHashes: Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Tx.delMultiple(tableName, txHashes);
    yield* MempoolTxDeltasDB.clearTxs(txHashes);
  });

export const clear: Effect.Effect<void, DatabaseError, Database> = Effect.gen(
  function* () {
    yield* clearTable(tableName);
    yield* MempoolTxDeltasDB.clear;
  },
);
