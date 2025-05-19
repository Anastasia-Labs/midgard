import { Effect } from "effect";
import { clearTable, mapSqlError } from "./utils.js";
import { SqlClient } from "@effect/sql";

export const tableName = "immutable";

export type ImmutableRecord = {
  tx_hash: string;
  tx_cbor: string;
};

export const createQuery = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`
  CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
    tx_hash TEXT NOT NULL,
    tx_cbor TEXT NOT NULL,
    PRIMARY KEY (tx_hash)
  );`;
}).pipe(Effect.withLogSpan(`creating table ${tableName}`), mapSqlError);

export const insert = (txs: { txHash: string; txCbor: string }[]) =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`${tableName} db: attempt to insert blocks`);
    const sql = yield* SqlClient.SqlClient;

    if (!txs.length) {
      yield* Effect.logDebug("No transactions provided, skipping insertion.");
      return;
    }
    const rowsToInsert = txs.map(({ txHash, txCbor }) => ({
      tx_hash: txHash,
      tx_cbor: txCbor,
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
    const rows = yield* sql<ImmutableRecord>`SELECT * FROM ${sql(tableName)}`;
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

export const retrieveTxCborsByHashes = (txHashes: string[]) =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`${tableName} db: attempt to retrieve transactions`);
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      txCbor: string;
    }>`SELECT tx_cbor FROM ${sql(
      tableName,
    )} WHERE ${sql.in("tx_hash", txHashes)}`;
    const result = rows.map((row) => row.txCbor);
    yield* Effect.logDebug(`${tableName} db: retrieved ${result.length} rows.`);
    return result;
  }).pipe(
    Effect.withLogSpan(`retrieve ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving error: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );
export const clear = () => clearTable(tableName);
