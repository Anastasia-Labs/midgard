/**
 * The staging table of a ledger take-in (`ledger-snapshot-start.ts`): the
 * entries of the snapshot being taken in, as `confirmed_ledger` rows, until
 * the install moves them into `confirmed_ledger` and `mempool_ledger` in
 * the transaction that stamps the ledger.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { sqlErrorToDatabaseError } from "./utils/common.js";
import * as Ledger from "./utils/ledger.js";

export const tableName = "node_ledger_snapshot_staging";

/** Empties the table: a take-in starts from nothing. */
export const clear = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`DELETE FROM ${sql(tableName)}`;
}).pipe(sqlErrorToDatabaseError(tableName, `Failed to empty ${tableName}`));

/**
 * Stages `rows`. An out-ref staged already keeps its row: the ledger store
 * refuses the stream that repeats it.
 */
export const insert = (rows: readonly Ledger.EntryNoTimeStamp[]) =>
  Effect.gen(function* () {
    if (rows.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert([...rows])}
      ON CONFLICT (outref) DO NOTHING`;
  }).pipe(sqlErrorToDatabaseError(tableName, `Failed to write ${tableName}`));

/**
 * Moves the staged rows into `confirmed_ledger` and `mempool_ledger`, which
 * hold nothing before the install, and empties the table.
 */
export const moveIntoLedgers = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  for (const ledger of ["confirmed_ledger", "mempool_ledger"])
    yield* sql`INSERT INTO ${sql(ledger)} (tx_id, outref, output, address)
      SELECT tx_id, outref, output, address FROM ${sql(tableName)}`;
  yield* sql`DELETE FROM ${sql(tableName)}`;
}).pipe(sqlErrorToDatabaseError(tableName, `Failed to install ${tableName}`));
