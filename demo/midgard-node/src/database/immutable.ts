import { Option } from "effect";
import { Sql } from "postgres";
import { logAbort, logInfo } from "../utils.js";
import * as utils from "./utils.js";
import { clearTable } from "./utils.js";

export const tableName = "immutable";

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
    await sql`INSERT INTO ${sql(tableName)} ${sql(value)}`;
    logInfo(`${tableName} db: tx stored`);
  } catch (err) {
    logAbort(`${tableName} db: error inserting tx: ${err}`);
    throw err;
  }
};

export const insertTxs = async (
  sql: Sql,
  txs: { txHash: Uint8Array; txCbor: Uint8Array }[],
): Promise<void> => {
  try {
    const values = txs.map((tx) => ({ tx_hash: tx.txHash, txCbor: tx.txCbor }));
    await sql`INSERT INTO ${sql(tableName)} ${sql(values)}`;
  } catch (err) {
    logAbort(`${tableName} db: error inserting txs: ${err}`);
    throw err;
  }
};

export const retrieve = async (
  sql: Sql,
): Promise<{ txHash: Uint8Array; txCbor: Uint8Array }[]> => {
  try {
    const result = await sql`SELECT * FROM ${sql(tableName)}`;
    return result.map((row) => ({
      txHash: row.tx_hash,
      txCbor: row.tx_cbor,
    }));
  } catch (err) {
    logAbort(`${tableName} db: error retrieving tx: ${err}`);
    throw err;
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

export const clear = async (sql: Sql) => clearTable(sql, tableName);
