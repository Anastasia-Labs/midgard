import { Effect, Option } from "effect";
import { SqlClient, SqlError } from "@effect/sql";
import {
  clearTable,
  retrieveTxCborByHash as utilsRetrieveTxCborByHash,
  retrieveTxCborsByHashes as utilsRetrieveTxCborsByHashes,
  clearTxs as utilsClearTxs,
  mapSqlError,
} from "./utils.js";

export const tableName = "mempool";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS ${tableName} (
    tx_hash BYTEA NOT NULL UNIQUE,
    tx_cbor BYTEA NOT NULL UNIQUE,
    PRIMARY KEY (tx_hash)
  );
`;

export const insert = (
  txHash: Uint8Array,
  txCbor: Uint8Array,
): Effect.Effect<void, Error, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`${tableName} db: attempt to insert tx`);
    const sql = yield* SqlClient.SqlClient;

    const value = { tx_hash: txHash, tx_cbor: txCbor };
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(value)}`;

    yield* Effect.logInfo(`${tableName} db: tx stored`);
  }).pipe(
    Effect.withLogSpan(`insert ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: error inserting tx: ${JSON.stringify(e)}`,
      ),
    ),
    Effect.asVoid,
    mapSqlError,
  );

export const retrieveTxCborByHash = (
  txHash: Uint8Array,
): Effect.Effect<Option.Option<Uint8Array>, Error, SqlClient.SqlClient> =>
  utilsRetrieveTxCborByHash(tableName, txHash);

export const retrieveTxCborsByHashes = (
  txHashes: Uint8Array[],
): Effect.Effect<Uint8Array[], Error, SqlClient.SqlClient> =>
  utilsRetrieveTxCborsByHashes(tableName, txHashes);

export const retrieve = (): Effect.Effect<
  { txHash: Uint8Array; txCbor: Uint8Array }[],
  Error,
  SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt retrieve all txs`);
    const sql = yield* SqlClient.SqlClient;

    const rows = yield* sql<{
      tx_hash: Uint8Array;
      tx_cbor: Uint8Array;
    }>`SELECT tx_hash, tx_cbor FROM ${sql(tableName)}`;

    const result = rows.map((row) => ({
      txHash: row.tx_hash,
      txCbor: row.tx_cbor,
    }));

    yield* Effect.logDebug(`${tableName} db: retrieved ${result.length} txs`);
    return result;
  }).pipe(
    Effect.withLogSpan(`retrieve all ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving txs error: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const clearTxs = (
  txHashes: Uint8Array[],
): Effect.Effect<void, Error, SqlClient.SqlClient> =>
  utilsClearTxs(tableName, txHashes);

export const clear = (): Effect.Effect<void, Error, SqlClient.SqlClient> =>
  clearTable(tableName);
