import { Option } from "effect";
import { Pool } from "pg";
import { logAbort, logInfo } from "../utils.js";
import { clearTable } from "./utils.js";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS blocks (
    header_hash BYTEA NOT NULL,
    tx_hash BYTEA NOT NULL UNIQUE
  );`;

export const insert = async (
  pool: Pool,
  headerHash: string,
  txHashes: string[],
): Promise<void> => {
  const query = `
      INSERT INTO blocks (header_hash, tx_hash)
      VALUES
      ${txHashes.map((_, i) => `($${i * 2 + 1}, $${i * 2 + 2})`).join(", ")}`;
  const values = txHashes.flatMap((txHash) => [
    Buffer.from(headerHash, "hex"),
    Buffer.from(txHash, "hex"),
  ]);

  try {
    await pool.query(query, values);
    // logInfo(`blocks db: ${txHashes.length} new tx_hashes added`);
  } catch (err) {
    // logAbort(`blocks db: inserting error: ${err}`);
    throw err;
  }
};

export const retrieveTxHashesByBlockHash = async (
  pool: Pool,
  blockHash: string,
): Promise<string[]> => {
  const query = `SELECT tx_hash FROM blocks WHERE header_hash = $1`;
  try {
    const result = await pool.query(query, [Buffer.from(blockHash, "hex")]);
    return result.rows.map((row) => row.tx_hash.toString("hex"));
  } catch (err) {
    // logAbort(`blocks db: retrieving error: ${err}`);
    throw err;
  }
};

export const retrieveBlockHashByTxHash = async (
  pool: Pool,
  txHash: string,
): Promise<Option.Option<string>> => {
  const query = `SELECT header_hash FROM blocks WHERE tx_hash = $1`;
  try {
    const result = await pool.query(query, [Buffer.from(txHash, "hex")]);
    if (result.rows.length > 0) {
      return Option.some(result.rows[0].header_hash.toString("hex"));
    } else {
      return Option.none();
    }
  } catch (err) {
    // logAbort(`blocks db: retrieving error: ${err}`);
    throw err;
  }
};

export const clearBlock = async (
  pool: Pool,
  blockHash: string,
): Promise<void> => {
  const query = `DELETE FROM blocks WHERE header_hash = $1`;
  try {
    await pool.query(query, [Buffer.from(blockHash, "hex")]);
    // logInfo(`blocks db: cleared`);
  } catch (err) {
    // logAbort(`blocks db: clearing error: ${err}`);
    throw err;
  }
};

export const retrieve = async (pool: Pool): Promise<[string, string][]> => {
  const query = `SELECT * FROM blocks`;
  try {
    const result = await pool.query(query);
    return result.rows.map((row) => [
      row.header_hash.toString("hex"),
      row.tx_hash.toString("hex"),
    ]);
  } catch (err) {
    // logAbort(`blocks db: retrieving error: ${err}`);
    throw err;
  }
};

export const clear = async (pool: Pool) => clearTable(pool, `blocks`);
