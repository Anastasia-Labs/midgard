import { Option } from "effect";
import { Sql } from "postgres";
import { logAbort, logInfo } from "../utils.js";

export const clearUTxOs = async (
  sql: Sql,
  tableName: string,
  refs: Uint8Array[],
): Promise<void> => {
  try {
    await sql`DELETE FROM ${sql(tableName)} WHERE tx_in_cbor IN ${sql(refs)}`;
  } catch (err) {
    throw err;
  }
};

export const clearTxs = async (
  sql: Sql,
  tableName: string,
  txHashes: Uint8Array[],
): Promise<void> => {
  try {
    const result = await sql`
      DELETE FROM ${sql(tableName)}
      WHERE tx_hash IN ${sql(txHashes)}
    `;
    logInfo(`${tableName} db: ${result.count} txs removed`);
  } catch (err) {
    logAbort(`${tableName} db: txs removing error: ${err}`);
    throw err;
  }
};

export const retrieveTxCborByHash = async (
  sql: Sql,
  tableName: string,
  txHash: Uint8Array,
): Promise<Option.Option<Uint8Array>> => {
  try {
    const result = await sql`
      SELECT tx_cbor FROM ${sql(tableName)} 
      WHERE tx_hash = ${txHash}
    `;
    return result.length > 0 ? Option.some(result[0].tx_cbor) : Option.none();
  } catch (err) {
    throw err;
  }
};

export const retrieveTxCborsByHashes = async (
  sql: Sql,
  tableName: string,
  txHashes: Uint8Array[],
): Promise<Uint8Array[]> => {
  try {
    const result = await sql`
      SELECT tx_cbor FROM ${sql(tableName)}
      WHERE tx_hash IN ${sql(txHashes)}
    `;
    return result.map((row) => row.tx_cbor);
  } catch (err) {
    throw err;
  }
};

export const clearTable = async (
  sql: Sql,
  tableName: string,
): Promise<void> => {
  try {
    await sql`TRUNCATE TABLE ${sql(tableName)} CASCADE`;
  } catch (err) {
    throw err;
  }
};

export const insertUTxOsCBOR = async (
  sql: Sql,
  tableName: string,
  utxosCBOR: { outputReference: Uint8Array; output: Uint8Array }[],
): Promise<void> => {
  const values = utxosCBOR.map((u) => ({
    tx_in_cbor: u.outputReference,
    tx_out_cbor: u.output,
  }));
  try {
    await sql`
    INSERT INTO ${sql(tableName)} ${sql(values)}
  `;
  } catch (e) {
    logAbort(`${tableName} db: error inserting utxos: ${e}`);
  }
};

export const retrieveUTxOsCBOR = async (
  sql: Sql,
  tableName: string,
): Promise<{ outputReference: Uint8Array; output: Uint8Array }[]> => {
  try {
    const result = await sql`SELECT * FROM ${sql(tableName)}`;
    return result.map((row) => ({
      outputReference: row.tx_in_cbor,
      output: row.tx_out_cbor,
    }));
  } catch (err) {
    throw err;
  }
};
