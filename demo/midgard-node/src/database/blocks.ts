import { Option } from "effect";
import { Sql } from "postgres";
import { clearTable } from "./utils.js";

export const tableName = "blocks";

export const createQuery = (sql: Sql) => sql`
  CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
    header_hash BYTEA NOT NULL,
    tx_hash BYTEA NOT NULL UNIQUE
  );
`;

export const insert = async (
  sql: Sql,
  headerHash: Uint8Array,
  txHashes: Uint8Array[],
): Promise<void> => {
  const values = txHashes.map((txHash) => ({
    header_hash: headerHash,
    tx_hash: txHash,
  }));
  await sql`INSERT INTO ${sql(tableName)} ${sql(values)}`;
};

export const retrieveTxHashesByBlockHash = async (
  sql: Sql,
  blockHash: Uint8Array,
): Promise<Uint8Array[]> => {
  const result = await sql`
    SELECT tx_hash FROM ${sql(tableName)}
    WHERE header_hash = ${blockHash}
  `;
  return result.map((row) => row.tx_hash);
};

export const retrieveBlockHashByTxHash = async (
  sql: Sql,
  txHash: Uint8Array,
): Promise<Option.Option<Uint8Array>> => {
  const result = await sql`
    SELECT header_hash FROM ${sql(tableName)}
    WHERE tx_hash = ${txHash}
  `;
  return result.length > 0 ? Option.some(result[0].header_hash) : Option.none();
};

export const clearBlock = async (
  sql: Sql,
  blockHash: Uint8Array,
): Promise<void> => {
  await sql`
    DELETE FROM ${sql(tableName)} 
    WHERE header_hash = ${blockHash}
  `;
};

export const retrieve = async (
  sql: Sql,
): Promise<[Uint8Array, Uint8Array][]> => {
  const result = await sql`SELECT * FROM ${sql(tableName)}`;
  return result.map((row) => [row.header_hash, row.tx_hash]);
};

export const clear = async (sql: Sql) => clearTable(sql, tableName);
