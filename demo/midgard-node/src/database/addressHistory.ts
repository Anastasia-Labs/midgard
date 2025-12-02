import { Database } from "@/services/database.js";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import {
  DatabaseError,
  clearTable,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { Address } from "@lucid-evolution/lucid";
import * as MempoolDB from "@/database/mempool.js";
import * as ImmutableDB from "@/database/immutable.js";
import * as Tx from "@/database/utils/tx.js";
import * as Ledger from "@/database/utils/ledger.js";
import { MempoolLedgerDB } from "./index.js";

const tableName = "address_history";

export type Entry = {
  [Ledger.Columns.TX_ID]: Buffer;
  [Ledger.Columns.ADDRESS]: Address;
};

export const init: Effect.Effect<void, DatabaseError, Database> = Effect.gen(
  function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
      ${sql(Ledger.Columns.TX_ID)} BYTEA NOT NULL,
      ${sql(Ledger.Columns.ADDRESS)} TEXT NOT NULL,
      UNIQUE (tx_id, address)
    );`;
  },
).pipe(
  Effect.withLogSpan(`creating table ${tableName}`),
  sqlErrorToDatabaseError(tableName, "Failed to create the table"),
);

export const insertEntries = (
  entries: Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (entries.length > 0) {
      const sql = yield* SqlClient.SqlClient;
      yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(entries)}
        ON CONFLICT (${sql(Ledger.Columns.TX_ID)}, ${sql(Ledger.Columns.ADDRESS)}) DO NOTHING`;
    }
  }).pipe(
    Effect.withLogSpan(`entries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insert entries: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to insert given entries"),
  );

export const insert = (
  spent: Buffer[],
  produced: Ledger.Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (spent.length > 0 || produced.length > 0) {
      const sql = yield* SqlClient.SqlClient;

      const inputEntriesProgram = sql<Entry>`SELECT ${sql(Ledger.Columns.TX_ID)}, ${sql(Ledger.Columns.ADDRESS)}
      FROM ${sql(MempoolLedgerDB.tableName)}
      WHERE ${sql(Ledger.Columns.TX_ID)} IN ${sql.in(spent)}`;

      const inputEntries = yield* inputEntriesProgram.pipe(
        Effect.catchAllCause((_) => Effect.succeed([])),
      );

      inputEntries;
      const outputEntries: Entry[] = produced.map((e) => ({
        [Ledger.Columns.TX_ID]: e[Ledger.Columns.TX_ID],
        [Ledger.Columns.ADDRESS]: e[Ledger.Columns.ADDRESS],
      }));

      yield* insertEntries([...inputEntries, ...outputEntries]);
    }
  }).pipe(Effect.withLogSpan(`entries ${tableName}`));

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
