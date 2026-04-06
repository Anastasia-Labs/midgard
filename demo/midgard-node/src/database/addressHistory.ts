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
import * as ProcessedMempoolDB from "@/database/processedMempool.js";
import * as ImmutableDB from "@/database/immutable.js";
import * as Tx from "@/database/utils/tx.js";
import * as Ledger from "@/database/utils/ledger.js";
import { MempoolLedgerDB } from "./index.js";

const tableName = "address_history";

export type Direction = "input" | "output";

export type Entry = {
  [Ledger.Columns.TX_ID]: Buffer;
  [Ledger.Columns.OUTREF]: Buffer;
  [Ledger.Columns.ADDRESS]: Address;
  direction: Direction;
};

export const createTable: Effect.Effect<void, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
      ${sql(Ledger.Columns.TX_ID)} BYTEA NOT NULL,
      ${sql(Ledger.Columns.OUTREF)} BYTEA NOT NULL,
      ${sql(Ledger.Columns.ADDRESS)} TEXT NOT NULL,
      direction TEXT NOT NULL DEFAULT 'output',
      UNIQUE (tx_id, outref, address, direction)
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
      yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(entries)}
        ON CONFLICT (${sql(Ledger.Columns.TX_ID)}, ${sql(Ledger.Columns.OUTREF)}, ${sql(Ledger.Columns.ADDRESS)}, direction) DO NOTHING`;
    }
  }).pipe(
    Effect.withLogSpan(`entries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insert entries: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to insert given entries"),
  );

export const insert = (
  spendingTxId: Buffer,
  spent: Buffer[],
  produced: Ledger.Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (spent.length > 0 || produced.length > 0) {
      const sql = yield* SqlClient.SqlClient;

      // Fix 1: query by outref (not tx_id — outrefs are TransactionInput CBORs).
      // Fix 2: tag results with spendingTxId so retrieve() returns the spending
      //         tx for the input address, not the tx that produced the UTxO.
      const spentRows: readonly { address: string; outref: Buffer }[] =
        spent.length > 0
          ? yield* sql<{ address: string; outref: Buffer }>`
              SELECT ${sql(Ledger.Columns.ADDRESS)}, ${sql(Ledger.Columns.OUTREF)}
              FROM ${sql(MempoolLedgerDB.tableName)}
              WHERE ${sql(Ledger.Columns.OUTREF)} IN ${sql.in(spent)}
            `.pipe(Effect.catchAllCause(() => Effect.succeed([])))
          : [];

      // direction='input': associates the spending tx with the address that owned
      // the spent UTxO — enables /txs to know which addresses were senders.
      const inputEntries: Entry[] = spentRows.map((row) => ({
        [Ledger.Columns.TX_ID]: spendingTxId,
        [Ledger.Columns.OUTREF]: row.outref,
        [Ledger.Columns.ADDRESS]: row.address,
        direction: "input" as Direction,
      }));

      // direction='output': standard — associates the spending tx with each
      // address that receives a new UTxO from it.
      const outputEntries: Entry[] = produced.map((e) => ({
        [Ledger.Columns.TX_ID]: e[Ledger.Columns.TX_ID],
        [Ledger.Columns.OUTREF]: e[Ledger.Columns.OUTREF],
        [Ledger.Columns.ADDRESS]: e[Ledger.Columns.ADDRESS],
        direction: "output" as Direction,
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
 * Retrieves all CBORs from MempoolDB, ProcessedMempoolDB, and ImmutableDB
 * that involve the given address (as either input or output).
 *
 * Uses an IN subquery instead of a JOIN to avoid duplicate rows when the same
 * (tx_id, address) appears with multiple directions (e.g. sender with change).
 * ProcessedMempoolDB covers txs that have been committed to a block but not
 * yet confirmed on L1 — without it those txs would vanish from history during
 * the confirmation window.
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
      FROM ${sql(ProcessedMempoolDB.tableName)}
      UNION
      SELECT ${sql(Tx.Columns.TX_ID)}, ${sql(Tx.Columns.TX)}
      FROM ${sql(ImmutableDB.tableName)}
    ) AS tx_union
    WHERE ${sql(Tx.Columns.TX_ID)} IN (
      SELECT ${sql(Ledger.Columns.TX_ID)}
      FROM ${sql(tableName)}
      WHERE ${sql(Ledger.Columns.ADDRESS)} = ${address}
    )`;

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

/**
 * Returns the exact outref to address mappings for a given txId.
 */
export const retrieveResolvedInputs = (
  txId: Buffer,
): Effect.Effect<
  readonly { outref: Buffer; address: string }[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<{ outref: Buffer; address: string }>`
      SELECT ${sql(Ledger.Columns.OUTREF)}, ${sql(Ledger.Columns.ADDRESS)}
      FROM ${sql(tableName)}
      WHERE ${sql(Ledger.Columns.TX_ID)} = ${txId}
      AND direction = 'input'
    `;
  }).pipe(
    Effect.withLogSpan(`retrieveResolvedInputs ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieveResolvedInputs: ${JSON.stringify(e)}`,
      ),
    ),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve resolved inputs by tx_id",
    ),
  );

export const migrate: Effect.Effect<void, DatabaseError, Database> = Effect.gen(
  function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`ALTER TABLE ${sql(tableName)} ADD COLUMN IF NOT EXISTS direction TEXT NOT NULL DEFAULT 'output'`;
    yield* sql`ALTER TABLE ${sql(tableName)} ADD COLUMN IF NOT EXISTS ${sql(Ledger.Columns.OUTREF)} BYTEA`;
    // Populate outref with dummy bytea for old entries if any (or truncate depending on use case setup)
    // Actually, simply making it not null after populating could be tricky if we don't have backfill logic yet.
    yield* sql`ALTER TABLE ${sql(tableName)} DROP CONSTRAINT IF EXISTS address_history_tx_id_address_direction_key`;
    yield* sql`ALTER TABLE ${sql(tableName)} DROP CONSTRAINT IF EXISTS address_history_tx_id_address_key`;
    yield* sql`ALTER TABLE ${sql(tableName)} ADD CONSTRAINT address_history_tx_id_outref_address_direction_key UNIQUE (${sql(Ledger.Columns.TX_ID)}, ${sql(Ledger.Columns.OUTREF)}, ${sql(Ledger.Columns.ADDRESS)}, direction)`.pipe(
      Effect.ignore,
    );
  },
).pipe(
  Effect.withLogSpan(`migrate ${tableName}`),
  sqlErrorToDatabaseError(tableName, "Failed to migrate the table"),
);

export const clear = clearTable(tableName);
