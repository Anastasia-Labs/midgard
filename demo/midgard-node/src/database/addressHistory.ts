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
} from "./index.js";
import { ProcessedTx } from "@/utils.js";

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

export const insertEntries = (
  entries: Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (entries.length > 0) {
      const sql = yield* SqlClient.SqlClient;
      // TODO: Make it so that on conflict that status column is updated.
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
 * Returns collective spent outrefs and produced ledger entries to allow fewer
 * traversals of the given `processedTxs` when used along other database
 * operations.
 */
const processedTxsToEntries = (
  processedTxs: ProcessedTx[],
): Effect.Effect<
  {
    allEntries: Entry[];
    allSpent: Buffer[];
    allProduced: Ledger.Entry[];
  },
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    // Collect all spent UTxOs so that we can make a single SQL query to
    // retrieve their addresses.
    const allSpent: Buffer[] = processedTxs.flatMap(
      (processedTxs) => processedTxs.spent,
    );
    const allProduced: Ledger.Entry[] = [];
    // Retrieve addresses of spent UTxOs from MempoolLedgerDB.
    // TODO: A fallback mechanism to either LatestLedgerDB or ConfirmedLedgerDB
    //       might make sense. However, if insertions to MempoolDB (and
    //       consequently MempoolLedgerDB) are validated by phase 1 and phase 2
    //       validations, looking up MempoolLedgerDB here might be sufficient.
    const inputLedgerEntries =
      yield* MempoolLedgerDB.retrieveByOutRefs(allSpent);
    const allEntries: Entry[] = [];
    // Goes through each ProcessedTx value while also exhausting the retrieved
    // ledger entries from MempoolLedgerDB. Therefore the final acc is an empty
    // list, which we are dicarding here.
    yield* Effect.reduce(
      processedTxs,
      inputLedgerEntries,
      (acc, processedTx, _i) =>
        Effect.gen(function* () {
          const relevantLedgerEntries = acc.slice(0, processedTx.spent.length);
          const inputEntries: Entry[] = relevantLedgerEntries.map(
            (ledgerEntry) => ({
              [Columns.ADDRESS]: ledgerEntry[Ledger.Columns.ADDRESS],
              [Columns.EVENT_ID]: processedTx.txId,
              [Columns.EVENT_TYPE]: EventType.TX,
              [Columns.STATUS]: Status.SLATED,
            }),
          );
          const outputEntries: Entry[] = processedTx.produced.map((e) => ({
            [Columns.EVENT_ID]: processedTx.txId,
            [Columns.ADDRESS]: e[Ledger.Columns.ADDRESS],
            [Columns.EVENT_TYPE]: EventType.TX,
            [Columns.STATUS]: Status.SLATED,
          }));
          allProduced.push(...processedTx.produced);
          allEntries.push(...inputEntries);
          allEntries.push(...outputEntries);
          return acc.slice(processedTx.spent.length);
        }),
    );
    return {
      allEntries,
      allSpent,
      allProduced,
    };
  }).pipe(
    sqlErrorToDatabaseError(tableName, "processedTxsToAddressHistoryEntries"),
  );

export const insertProcessedTxs = (
  processedTxs: ProcessedTx[],
): Effect.Effect<
  {
    insertedEntries: Entry[];
    collectiveSpent: Buffer[];
    collectiveProduced: Ledger.Entry[];
  },
  DatabaseError | SDK.CmlDeserializationError | SDK.DataCoercionError,
  Database
> =>
  Effect.gen(function* () {
    if (processedTxs.length <= 0) {
      yield* Effect.logDebug("No transactions to insert in address history db");
      return {
        insertedEntries: [],
        collectiveSpent: [],
        collectiveProduced: [],
      };
    } else {
      const { allEntries, allSpent, allProduced } =
        yield* processedTxsToEntries(processedTxs);
      yield* insertEntries(allEntries);
      return {
        insertedEntries: allEntries,
        collectiveSpent: allSpent,
        collectiveProduced: allProduced,
      };
    }
  }).pipe(Effect.withLogSpan(`entries ${tableName}`));

/**
 * Given a list of withdrawal event entries, this function tries to find their
 * spent UTxOs in `MempoolLedgerDB`. Any missing withdrawn output reference
 * leads to failure.
 */
export const insertWithdrwals = (
  withdrawals: UserEvents.Entry[],
): Effect.Effect<
  void,
  DatabaseError | NotFoundError | SDK.CmlDeserializationError | SDK.DataCoercionError,
  Database
> =>
  Effect.gen(function* () {
    const withdrawnOutRefs = yield* Effect.all(
      withdrawals.map((w) =>
        WithdrawalsDB.entryToOutRef(w).pipe(
          Effect.map((outRef) => ({
            withdrawalID: w[UserEvents.Columns.ID],
            outRef,
          })),
        ),
      ),
    );
    yield* Effect.all(
      withdrawnOutRefs.map(({ withdrawalID, outRef }) =>
        Effect.gen(function* () {
          const ledgerEntry = yield* MempoolLedgerDB.retrieveByOutRef(
            outRef,
          );
          return {
            [Columns.EVENT_ID]: withdrawalID,
            [Columns.ADDRESS]: ledgerEntry[Ledger.Columns.ADDRESS],
            [Columns.EVENT_TYPE]: EventType.WITHDRAWAL,
            [Columns.STATUS]: Status.SLATED,
          };
        }),
      ),
    );
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
