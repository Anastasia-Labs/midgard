import { Option } from "effect";
import { Pool } from "pg";
import { logAbort, logInfo } from "../utils.js";
import * as blocks from "./blocks.js";
import * as confirmedLedger from "./confirmedLedger.js";
import * as immutable from "./immutable.js";
import * as latestLedger from "./latestLedger.js";
import * as mempool from "./mempool.js";
import * as mempoolLedger from "./mempoolLedger.js";

export async function initializeDb(pool: Pool) {
  try {
    await pool.query(`
      SET default_transaction_isolation TO 'serializable';
    `);

    await pool.query(blocks.createQuery);
    await pool.query(mempool.createQuery);
    await pool.query(mempoolLedger.createQuery);
    await pool.query(immutable.createQuery);
    await pool.query(confirmedLedger.createQuery);
    await pool.query(latestLedger.createQuery);

    logInfo("Connected to the PostgreSQL database");
    return pool;
  } catch (err) {
    logAbort(`Error initializing database: ${err}`);
    throw err;
  }
}

export const clearUTxOs = async (
  pool: Pool,
  tableName: string,
  refs: Uint8Array[],
): Promise<void> => {
  const query = `DELETE FROM ${tableName} WHERE (tx_in_cbor) IN (${refs
    .map((_, i) => `($${i + 1})`)
    .join(", ")})`;
  // const values = refs.flatMap((r) => [
  //   Buffer.from(r),
  // ]);

  try {
    await pool.query(query, refs);
    // logInfo(`${tableName} db: ${result.rowCount} utxos removed`);
  } catch (err) {
    // logAbort(`${tableName} db: utxos removing error: ${err}`);
    throw err;
  }
};

export const clearTxs = async (
  pool: Pool,
  tableName: string,
  txHashes: Uint8Array[],
): Promise<void> => {
  const query = `DELETE FROM ${tableName} WHERE tx_hash IN (${txHashes
    .map((_, i) => `$${i + 1}`)
    .join(", ")})`;
  try {
    const result = await pool.query(query, txHashes);
    logInfo(`${tableName} db: ${result.rowCount} txs removed`);
  } catch (err) {
    logAbort(`${tableName} db: txs removing error: ${err}`);
    throw err;
  }
};

export const retrieveTxCborByHash = async (
  pool: Pool,
  tableName: string,
  txHash: Uint8Array,
): Promise<Option.Option<Uint8Array>> => {
  const query = `SELECT tx_cbor FROM ${tableName} WHERE tx_hash = $1`;
  try {
    const result = await pool.query(query, [txHash]);
    if (result.rows.length > 0) {
      return Option.some(result.rows[0].tx_cbor);
    } else {
      return Option.none();
    }
  } catch (err) {
    // logAbort(`db: retrieving error: ${err}`);
    throw err;
  }
};

export const retrieveTxCborsByHashes = async (
  pool: Pool,
  tableName: string,
  txHashes: Uint8Array[],
): Promise<Uint8Array[]> => {
  const query = `SELECT tx_cbor FROM ${tableName} WHERE tx_hash = ANY($1)`;
  try {
    const result = await pool.query(query, [txHashes]);
    return result.rows.map((row) => row.tx_cbor);
  } catch (err) {
    // logAbort(`${tableName} db: retrieving error: ${err}`);
    throw err;
  }
};

export const clearTable = async (
  pool: Pool,
  tableName: string,
): Promise<void> => {
  const query = `TRUNCATE TABLE ${tableName} CASCADE;`;

  try {
    await pool.query(query);
    // logInfo(`${tableName} db: cleared`);
  } catch (err) {
    // logAbort(`${tableName} db: clearing error: ${err}`);
    throw err;
  }
};

export const insertUTxOsCBOR = async (
  pool: Pool,
  tableName: string,
  utxosCBOR: { outputReference: Uint8Array; output: Uint8Array }[],
): Promise<void> => {
  const query = `
  INSERT INTO ${tableName} (tx_in_cbor, tx_out_cbor)
  VALUES
  ${utxosCBOR.map((_, i) => `($${i * 2 + 1}, $${i * 2 + 2})`).join(", ")}
  `;
  const values = utxosCBOR.flatMap((u) => [u.outputReference, u.output]);
  await pool.query(query, values);
};

export const retrieveUTxOsCBOR = async (
  pool: Pool,
  tableName: string,
): Promise<{ outputReference: Uint8Array; output: Uint8Array }[]> => {
  const query = `SELECT * FROM ${tableName}`;
  const rows = await pool.query(query);
  const result: { outputReference: Uint8Array; output: Uint8Array }[] =
    rows.rows.map((r) => ({
      outputReference: r.tx_in_cbor,
      output: r.tx_out_cbor,
    }));
  return result;
};
