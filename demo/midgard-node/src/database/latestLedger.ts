import { Sql } from "postgres";
import * as utils from "./utils.js";
import { clearTable, insertUTxOsCBOR, retrieveUTxOsCBOR } from "./utils.js";

export const tableName = "latest_ledger";

export const createQuery = (sql: Sql) => sql`
  CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
    tx_in_cbor BYTEA NOT NULL,
    tx_out_cbor BYTEA NOT NULL,
    PRIMARY KEY (tx_in_cbor)
  );
`;

export const insert = async (
  sql: Sql,
  utxosCBOR: { outputReference: Uint8Array; output: Uint8Array }[],
) => insertUTxOsCBOR(sql, tableName, utxosCBOR);

export const retrieve = async (
  sql: Sql,
): Promise<{ outputReference: Uint8Array; output: Uint8Array }[]> =>
  retrieveUTxOsCBOR(sql, tableName);

export const clearUTxOs = async (sql: Sql, refs: Uint8Array[]) =>
  utils.clearUTxOs(sql, tableName, refs);

export const clear = async (sql: Sql) => clearTable(sql, tableName);
