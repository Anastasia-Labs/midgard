import { Pool } from "pg";
import * as utils from "./utils.js";
import {
  clearTable,
  insertUTxOsCBOR,
  retrieveUTxOsCBOR,
  retrieveUTxOsCBORByTxIns,
} from "./utils.js";

export const tableName = "latest_ledger";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS ${tableName} (
    tx_in_cbor BYTEA NOT NULL,
    tx_out_cbor BYTEA NOT NULL,
    PRIMARY KEY (tx_in_cbor)
  );
  `;

export const insert = async (
  pool: Pool,
  utxosCBOR: { outputReference: Uint8Array; output: Uint8Array }[],
) => insertUTxOsCBOR(pool, tableName, utxosCBOR);

export const retrieve = async (
  pool: Pool,
): Promise<{ outputReference: Uint8Array; output: Uint8Array }[]> =>
  retrieveUTxOsCBOR(pool, tableName);

export const retrieveByTxIns = async (
  pool: Pool,
  txIns: Uint8Array[],
): Promise<{ outputReference: Uint8Array; output: Uint8Array }[]> =>
  retrieveUTxOsCBORByTxIns(pool, "latest_ledger", txIns);

export const clearUTxOs = async (pool: Pool, refs: Uint8Array[]) =>
  utils.clearUTxOs(pool, tableName, refs);

export const clear = async (pool: Pool) => clearTable(pool, tableName);
