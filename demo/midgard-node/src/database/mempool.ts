import { Option } from "effect";
import { Pool } from "pg";
import * as utils from "./utils.js";
import { clearTable } from "./utils.js";

export const tableName = "mempool";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS ${tableName} (
    tx_hash BYTEA NOT NULL UNIQUE,
    tx_cbor BYTEA NOT NULL UNIQUE,
    PRIMARY KEY (tx_hash)
  );`;

export const insert = async (
  pool: Pool,
  txHash: Uint8Array,
  txCbor: Uint8Array,
): Promise<void> => {
  const query = `INSERT INTO ${tableName} (tx_hash, tx_cbor) VALUES ($1, $2)`;
  try {
    await pool.query(query, [txHash, txCbor]);
    // logInfo(`${tableName} db: tx stored`);
  } catch (err) {
    // logAbort(`${tableName} db: error inserting tx: ${err}`);
    throw err;
  }
};

export const retrieveTxCborByHash = async (
  pool: Pool,
  txHash: Uint8Array,
): Promise<Option.Option<Uint8Array>> =>
  utils.retrieveTxCborByHash(pool, tableName, txHash);

export const retrieveTxCborsByHashes = async (
  pool: Pool,
  txHashes: Uint8Array[],
): Promise<Uint8Array[]> =>
  utils.retrieveTxCborsByHashes(pool, tableName, txHashes);

export const retrieve = async (
  pool: Pool,
): Promise<{ txHash: Uint8Array; txCbor: Uint8Array }[]> => {
  const query = `SELECT * FROM ${tableName}`;
  try {
    const result = await pool.query(query);
    return result.rows.map((row) => ({
      txHash: row.tx_hash,
      txCbor: row.tx_cbor,
    }));
  } catch (err) {
    // logAbort(`${tableName} db: retrieving error: ${err}`);
    throw err;
  }
};

export const clearTxs = async (pool: Pool, txHashes: Uint8Array[]) =>
  utils.clearTxs(pool, tableName, txHashes);

export const clear = async (pool: Pool) => clearTable(pool, tableName);
