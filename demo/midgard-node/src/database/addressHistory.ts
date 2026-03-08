import { Database } from "@/services/database.js";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { Address } from "@lucid-evolution/lucid";
import * as SDK from "@al-ft/midgard-sdk";
import {
  DatabaseError,
  NotFoundError,
  clearTable,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import {
  ImmutableDB,
  MempoolDB,
  MempoolLedgerDB,
  Ledger,
  Tx,
  UserEvents,
  WithdrawalsDB,
  DepositsDB,
} from "./index.js";
import { breakDownTx, ProcessedTx } from "@/utils.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";
import { NodeConfig } from "@/services/config.js";

const tableName = "address_history";

export enum Columns {
  EVENT_ID = "event_id",
  ADDRESS = "address",
  EVENT_TYPE = "event_type",
  STATUS = "status",
}

export type Entry = {
  [Columns.EVENT_ID]: Buffer;
  [Columns.ADDRESS]: Address;
  [Columns.EVENT_TYPE]: EventType;
  [Columns.STATUS]: Status;
};

export enum Status {
  SLATED = 0,
  COMMITTED = 1,
  SUBMITTED = 2,
  MERGED = 3,
}

export enum EventType {
  TX = 0,
  WITHDRAWAL = 1,
  DEPOSIT = 2,
}

export const createTable: Effect.Effect<void, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
      ${sql(Columns.EVENT_ID)} BYTEA NOT NULL,
      ${sql(Columns.ADDRESS)} TEXT NOT NULL,
      ${sql(Columns.EVENT_TYPE)} INTEGER NOT NULL,
      ${sql(Columns.STATUS)} INTEGER NOT NULL DEFAULT(${Status.SLATED}),
      UNIQUE (${sql(Ledger.Columns.TX_ID)}, ${sql(Ledger.Columns.ADDRESS)})
    );`;
  }).pipe(
    Effect.withLogSpan(`creating table ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to create the table"),
  );

/**
 * TODO: Make it so that on conflict status column is updated, and rename to
 *       "upsert."
 */
export const insertEntries = (
  entries: Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (entries.length > 0) {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`
        INSERT INTO ${sql(tableName)} ${sql.insert(entries)}
        ON CONFLICT (${sql(Columns.EVENT_ID)}, ${sql(Columns.ADDRESS)}) DO NOTHING`;
    }
  }).pipe(
    Effect.withLogSpan(`entries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insert entries: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to insert given entries"),
  );

/**
 * Returns collective spent outrefs and produced ledger entries, plus Tx.Entry
 * values to allow fewer traversals of the given `processedTxs` when used along
 * other database operations.
 */
export const aggregateProcessedTxs = (
  referenceLedgerTableName: string,
  processedTxs: ProcessedTx[],
  status: Status,
): Effect.Effect<
  {
    allTxEntries: Tx.Entry[];
    addressHistoryEntries: Entry[];
    collectiveSpent: Buffer[];
    collectiveProduced: Ledger.Entry[];
  },
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    // To reduce traversals, we'll also collect `Tx.Entry` equivalents.
    const allTxEntries: Tx.Entry[] = [];
    // Collect all spent UTxOs so that we can make a single SQL query to
    // retrieve their addresses.
    const collectiveSpent: Buffer[] = processedTxs.flatMap(
      (processedTxs) => processedTxs.spent,
    );
    const collectiveProduced: Ledger.Entry[] = [];
    // Retrieve addresses of spent UTxOs from the given ledger table.
    const inputLedgerEntries = yield* Ledger.retrieveByOutRefs(
      referenceLedgerTableName,
      collectiveSpent,
    );
    const addressHistoryEntries: Entry[] = [];
    // Goes through each ProcessedTx value while also exhausting the retrieved
    // ledger entries from MempoolLedgerDB. Therefore the final acc is an empty
    // list, which we are dicarding here.
    yield* Effect.reduce(
      processedTxs,
      inputLedgerEntries,
      (acc, processedTx, _i) =>
        Effect.gen(function* () {
          allTxEntries.push({
            [Tx.Columns.TX_ID]: processedTx.txId,
            [Tx.Columns.TX]: processedTx.txCbor,
          });
          const relevantLedgerEntries = acc.slice(0, processedTx.spent.length);
          const inputEntries: Entry[] = relevantLedgerEntries.map(
            (ledgerEntry) => ({
              [Columns.ADDRESS]: ledgerEntry[Ledger.Columns.ADDRESS],
              [Columns.EVENT_ID]: processedTx.txId,
              [Columns.EVENT_TYPE]: EventType.TX,
              [Columns.STATUS]: status,
            }),
          );
          const outputEntries: Entry[] = processedTx.produced.map((e) => ({
            [Columns.EVENT_ID]: processedTx.txId,
            [Columns.ADDRESS]: e[Ledger.Columns.ADDRESS],
            [Columns.EVENT_TYPE]: EventType.TX,
            [Columns.STATUS]: status,
          }));
          collectiveProduced.push(...processedTx.produced);
          addressHistoryEntries.push(...inputEntries);
          addressHistoryEntries.push(...outputEntries);
          return acc.slice(processedTx.spent.length);
        }),
    );
    return {
      allTxEntries,
      addressHistoryEntries,
      collectiveSpent,
      collectiveProduced,
    };
  }).pipe(
    sqlErrorToDatabaseError(tableName, "processedTxsToAddressHistoryEntries"),
  );

export const resolvedWithdrawalToEntry = (
  withdrawal: WithdrawalsDB.ResolvedWithdrawal,
  status: Status,
): Entry => ({
  [Columns.EVENT_ID]: withdrawal.withdrawalEntry[UserEvents.Columns.ID],
  [Columns.ADDRESS]: withdrawal.ledgerEntry[Ledger.Columns.ADDRESS],
  [Columns.EVENT_TYPE]: EventType.WITHDRAWAL,
  [Columns.STATUS]: status,
});

/**
 * Given a list of withdrawal event entries, this function tries to find their
 * spent UTxOs in `MempoolLedgerDB`. Any missing withdrawn output reference
 * leads to failure, since this function is meant to be used at the time of
 * block commitment and not earlier.
 */
export const insertWithdrwals = (
  withdrawals: WithdrawalsDB.ResolvedWithdrawal[],
  status: Status,
): Effect.Effect<void, DatabaseError, Database> =>
  insertEntries(withdrawals.map((w) => resolvedWithdrawalToEntry(w, status)));

export const depositEntryToEntry = (
  deposit: UserEvents.Entry,
  status: Status,
): Effect.Effect<
  Entry,
  SDK.CmlDeserializationError,
  NodeConfig | AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    const ledgerEntry = yield* DepositsDB.entryToLedgerEntry(deposit);
    return {
      [Columns.EVENT_ID]: deposit[UserEvents.Columns.ID],
      [Columns.ADDRESS]: ledgerEntry[Ledger.Columns.ADDRESS],
      [Columns.EVENT_TYPE]: EventType.DEPOSIT,
      [Columns.STATUS]: status,
    };
  });

export const insertDeposits = (
  deposits: UserEvents.Entry[],
  status: Status,
): Effect.Effect<
  void,
  SDK.CmlDeserializationError | DatabaseError,
  NodeConfig | Database | AlwaysSucceedsContract
> =>
  Effect.all(deposits.map((d) => depositEntryToEntry(d, status))).pipe(
    Effect.andThen(insertEntries),
  );

export const insertTxOrders = (
  txOrders: UserEvents.Entry[],
  status: Status,
): Effect.Effect<
  void,
  SDK.CmlDeserializationError | SDK.DataCoercionError | DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const processedTxs = yield* Effect.all(
      txOrders.map((txOrder) => breakDownTx(txOrder[UserEvents.Columns.INFO])),
    );
    yield* insertProcessedTxs(processedTxs, status);
  });

export const delTxHash = (
  tx_hash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logDebug(
      `${tableName} db: attempt to delete all entries with tx_hash`,
    );
    const result = yield* sql`DELETE FROM ${sql(tableName)} WHERE ${sql(
      Ledger.Columns.TX_ID,
    )} = ${tx_hash}`;
    yield* Effect.logDebug(`${tableName} db: deleted ${result.length} rows`);
  }).pipe(
    Effect.withLogSpan(`delTxHash table ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to delete entries with the given tx hash",
    ),
  );

/**
 * Retrieves all cbors from MempoolDB and ImmutableDB which mention provided
 * address.
 *
 * Works by performing an inner join with tables [tx_id | address] and
 * [tx_id | tx], getting [address | tx] as a result.
 */
export const retrieve = (
  address: Address,
): Effect.Effect<readonly Buffer[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* Effect.logInfo(
      `${tableName} db: attempt to retrieve value with address ${address}`,
    );

    const result = yield* sql<
      Pick<Tx.Entry, Tx.Columns.TX>
    >`SELECT ${sql(Tx.Columns.TX)} FROM (
      SELECT ${sql(Tx.Columns.TX_ID)}, ${sql(Tx.Columns.TX)}
      FROM ${sql(MempoolDB.tableName)}
      UNION
      SELECT ${sql(Tx.Columns.TX_ID)}, ${sql(Tx.Columns.TX)}
      FROM ${sql(ImmutableDB.tableName)}
    ) AS tx_union
    INNER JOIN ${sql(
      tableName,
    )} ON tx_union.${sql(Tx.Columns.TX_ID)} = ${sql(tableName)}.${sql(Ledger.Columns.TX_ID)}
    WHERE ${sql(Ledger.Columns.ADDRESS)} = ${address};`;

    return result.map((r) => r[Tx.Columns.TX]);
  }).pipe(
    Effect.withLogSpan(`retrieve value ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving value error: ${JSON.stringify(e)}`,
      ),
    ),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve entries of the given address",
    ),
  );

export const clear = clearTable(tableName);
