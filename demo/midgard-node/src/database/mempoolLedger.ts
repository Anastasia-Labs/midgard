import { Pool } from "pg";
import * as utils from "./utils.js";
import { clearTable, insertUTxOsCBOR, retrieveUTxOsCBOR } from "./utils.js";

export const createQuery = `
CREATE TABLE IF NOT EXISTS mempool_ledger (
    tx_in_cbor BYTEA NOT NULL,
    tx_out_cbor BYTEA NOT NULL,
    PRIMARY KEY (tx_in_cbor)
  );`;

export const insert = async (
  pool: Pool,
  utxosCBOR: { outputReference: Uint8Array; output: Uint8Array }[],
) => insertUTxOsCBOR(pool, "mempool_ledger", utxosCBOR);

export const retrieve = async (
  pool: Pool,
): Promise<{ outputReference: Uint8Array; output: Uint8Array }[]> =>
  retrieveUTxOsCBOR(pool, "mempool_ledger");

export const clearUTxOs = async (pool: Pool, refs: Uint8Array[]) =>
  utils.clearUTxOs(pool, "mempool_ledger", refs);

export const clear = async (pool: Pool) => clearTable(pool, "mempool_ledger");
