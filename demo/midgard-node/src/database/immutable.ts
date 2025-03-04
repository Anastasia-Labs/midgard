import { Option } from "effect";
import { Pool } from "pg";
import { logAbort, logInfo } from "../utils.js";
import * as utils from "./utils.js";
import { clearTable } from "./utils.js";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS immutable (
    tx_hash BYTEA NOT NULL UNIQUE,
    tx_cbor BYTEA NOT NULL UNIQUE,
    PRIMARY KEY (tx_hash)
  );
`;

export const insert = async (
  pool: Pool,
  txHash: string,
  txCbor: string
): Promise<void> => {
  const query = `INSERT INTO immutable (tx_hash, tx_cbor) VALUES ($1, $2)`;
  try {
    await pool.query(query, [
      Buffer.from(txHash, "hex"),
      Buffer.from(txCbor, "hex"),
    ]);
    logInfo(`immutable db: tx stored`);
  } catch (err) {
    logAbort(`immutable db: error inserting tx: ${err}`);
    throw err;
  }
};

export const insertTxs = async (
  pool: Pool,
  txs: { txHash: string; txCbor: string }[]
): Promise<void> => {
  const query = `INSERT INTO immutable (tx_hash, tx_cbor) VALUES ($1, $2)`;

  try {
    for (const { txHash, txCbor } of txs) {
      await pool.query(query, [
        Buffer.from(txHash, "hex"),
        Buffer.from(txCbor, "hex"),
      ]);
      logInfo(`immutable db: tx stored`);
    }
  } catch (err) {
    logAbort(`immutable db: error inserting tx: ${err}`);
    throw err;
  }
};

export const retrieve = async (pool: Pool): Promise<[string, string][]> => {
  const query = `SELECT * FROM immutable`;
  try {
    const result = await pool.query(query);
    return result.rows.map((row) => [
      row.tx_hash.toString("hex"),
      row.tx_cbor.toString("hex"),
    ]);
  } catch (err) {
    logAbort(`immutable db: retrieving error: ${err}`);
    throw err;
  }
};

export const retrieveTxCborByHash = async (
  pool: Pool,
  txHash: string
): Promise<Option.Option<string>> =>
  utils.retrieveTxCborByHash(pool, "immutable", txHash);

export const retrieveTxCborsByHashes = async (
  pool: Pool,
  txHashes: string[]
): Promise<string[]> =>
  utils.retrieveTxCborsByHashes(pool, "immutable", txHashes);

export const clear = async (pool: Pool) => clearTable(pool, "immutable");
