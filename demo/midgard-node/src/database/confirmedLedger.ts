import { Effect } from "effect";
import { SqlClient, SqlError } from "@effect/sql";
import {
  clearTable,
  insertUTxOsCBOR,
  retrieveUTxOsCBOR,
  clearUTxOs as utilsClearUTxOs,
} from "./utils.js";

export const tableName = "confirmed_ledger";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS ${tableName} (
    tx_in_cbor BYTEA NOT NULL,
    tx_out_cbor BYTEA NOT NULL,
    PRIMARY KEY (tx_in_cbor)
  );
`;

export const insert = (
  utxosCBOR: { outputReference: Uint8Array; output: Uint8Array }[],
): Effect.Effect<void, Error, SqlClient.SqlClient> =>
  insertUTxOsCBOR(tableName, utxosCBOR);

export const retrieve = (): Effect.Effect<
  { outputReference: Uint8Array; output: Uint8Array }[],
  Error,
  SqlClient.SqlClient
> => retrieveUTxOsCBOR(tableName);

export const clearUTxOs = (
  refs: Uint8Array[],
): Effect.Effect<void, Error, SqlClient.SqlClient> =>
  utilsClearUTxOs(tableName, refs);

export const clear = (): Effect.Effect<void, Error, SqlClient.SqlClient> =>
  clearTable(tableName);
