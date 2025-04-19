import { Option } from "effect";
import { Sql } from "postgres";
import * as utils from "./utils.js";
import { clearTable } from "./utils.js";
import { logAbort } from "../utils.js";

export const tableName = "mempool";

export const createQuery = (sql: Sql) => sql`
  CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
    tx_hash BYTEA NOT NULL UNIQUE,
    tx_cbor BYTEA NOT NULL UNIQUE,
    PRIMARY KEY (tx_hash)
  );
`;

export const insert = async (
  sql: Sql,
  txHash: Uint8Array,
  txCbor: Uint8Array,
): Promise<void> => {
  try {
    const value = { tx_hash: txHash, tx_cbor: txCbor };
    await sql`
    INSERT INTO ${sql(tableName)} ${sql(value)}
  `;
  } catch (e) {
    logAbort(`${tableName} db: error inserting tx: ${e}`);
  }
};

export const retrieveTxCborByHash = async (
  sql: Sql,
  txHash: Uint8Array,
): Promise<Option.Option<Uint8Array>> =>
  utils.retrieveTxCborByHash(sql, tableName, txHash);

export const retrieveTxCborsByHashes = async (
  sql: Sql,
  txHashes: Uint8Array[],
): Promise<Uint8Array[]> =>
  utils.retrieveTxCborsByHashes(sql, tableName, txHashes);

export const retrieve = async (
  sql: Sql,
): Promise<{ txHash: Uint8Array; txCbor: Uint8Array }[]> => {
  const result = await sql`SELECT * FROM ${sql(tableName)}`;
  return result.map((row) => ({
    txHash: row.tx_hash,
    txCbor: row.tx_cbor,
  }));
};

export const clearTxs = async (sql: Sql, txHashes: Uint8Array[]) =>
  utils.clearTxs(sql, tableName, txHashes);

export const clear = async (sql: Sql) => clearTable(sql, tableName);
