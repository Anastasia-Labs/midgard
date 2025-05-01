import { Effect } from "effect";
import { SqlClient, SqlError } from "@effect/sql";
import {
  clearTable,
  insertUTxOsCBOR,
  mapSqlError,
  retrieveUTxOsCBOR,
  clearUTxOs as utilsClearUTxOs,
} from "./utils.js";

export const tableName = "latest_ledger_clone";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS ${tableName} (
    tx_in_cbor BYTEA NOT NULL,
    tx_out_cbor BYTEA NOT NULL,
    PRIMARY KEY (tx_in_cbor)
  );
`;

export const clone = (): Effect.Effect<void, Error, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`${tableName} db: attempt to clone`);
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)} SELECT * FROM ${sql(tableName)}`;
  }).pipe(mapSqlError);

export const insert = (
  utxosCBOR: { outputReference: Uint8Array; output: Uint8Array }[],
): Effect.Effect<void, SqlError.SqlError, SqlClient.SqlClient> =>
  insertUTxOsCBOR(tableName, utxosCBOR);

export const retrieve = (): Effect.Effect<
  { outputReference: Uint8Array; output: Uint8Array }[],
  SqlError.SqlError,
  SqlClient.SqlClient
> => retrieveUTxOsCBOR(tableName);

export const clearUTxOs = (
  refs: Uint8Array[],
): Effect.Effect<void, SqlError.SqlError, SqlClient.SqlClient> =>
  utilsClearUTxOs(tableName, refs);

export const clear = (): Effect.Effect<
  void,
  SqlError.SqlError,
  SqlClient.SqlClient
> => clearTable(tableName);
