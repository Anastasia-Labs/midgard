import { Effect } from "effect";
import { clearTable, delMultiple, mapSqlError } from "./utils.js";
import { SqlClient } from "@effect/sql";

export const tableName = "mempool_ledger";

export type MempoolLedgerRecord = {
  utxo_outref: string;
  utxo_output: string;
};

export const createQuery = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`
  CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
    utxo_outref TEXT NOT NULL,
    utxo_output TEXT NOT NULL,
    PRIMARY KEY (utxo_outref)
  );`;
}).pipe(Effect.withLogSpan(`creating table ${tableName}`), mapSqlError);

export const insert = (utxosCBOR: { outref: string; output: string }[]) =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`${tableName} db: attempt to insert blocks`);
    const sql = yield* SqlClient.SqlClient;

    if (!utxosCBOR.length) {
      yield* Effect.logDebug("No UTxOs provided, skipping block insertion.");
      return;
    }
    const rowsToInsert = utxosCBOR.map(({ outref, output }) => ({
      utxo_outref: outref,
      utxo_output: output,
    }));

    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(rowsToInsert)}`;

    yield* Effect.logInfo(
      `${tableName} db: ${rowsToInsert.length} rows inserted.`,
    );
  }).pipe(
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: inserting error: ${e}`),
    ),
    Effect.withLogSpan(`insert ${tableName}`),
    mapSqlError,
    Effect.asVoid,
  );

export const retrieveAll = () =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`${tableName} db: attempt to retrieve utxos`);
    const sql = yield* SqlClient.SqlClient;
    const rows =
      yield* sql<MempoolLedgerRecord>`SELECT * FROM ${sql(tableName)}`;
    yield* Effect.logDebug(`${tableName} db: retrieved ${rows.length} rows.`);
    return Array.from(rows);
  }).pipe(
    Effect.withLogSpan(`retrieve ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving error: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const clearUTxOs = (refs: string[]) =>
  delMultiple(tableName, "utxo_outref", refs);

export const clear = () => clearTable(tableName);
