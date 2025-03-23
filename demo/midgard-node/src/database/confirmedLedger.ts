import { Pool } from "pg";
import * as utils from "./utils.js";
import { clearTable, insertUTxOsCBOR, retrieveUTxOsCBOR } from "./utils.js";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS confirmed_ledger (
    tx_in_cbor BYTEA NOT NULL,
    tx_out_cbor BYTEA NOT NULL,
    PRIMARY KEY (tx_in_cbor)
  );
  `;

export const insert = async (
  pool: Pool,
  utxosCBOR: { outputReference: Uint8Array; output: Uint8Array }[],
) => insertUTxOsCBOR(pool, "confirmed_ledger", utxosCBOR);

export const retrieve = async (
  pool: Pool,
): Promise<{ outputReference: Uint8Array; output: Uint8Array }[]> =>
  retrieveUTxOsCBOR(pool, "confirmed_ledger");

export const clearUTxOs = async (pool: Pool, refs: Uint8Array[]) =>
  utils.clearUTxOs(pool, "confirmed_ledger", refs);

export const clear = async (pool: Pool): Promise<void> =>
  clearTable(pool, "confirmed_ledger");
