import { OutRef, UTxO } from "@lucid-evolution/lucid";
import { Pool } from "pg";
import { logAbort } from "../utils.js";
import * as utils from "./utils.js";
import {
  clearTable,
  insertUTxOs,
  retrieveUTxOs,
  retrieveUTxOsByOutRef,
  utxoFromRow,
} from "./utils.js";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS mempool_ledger (
    tx_hash BYTEA NOT NULL,
    output_index INTEGER NOT NULL,
    address TEXT NOT NULL,
    datum_hash BYTEA,
    datum BYTEA,
    script_ref_type VARCHAR (8),
    script_ref_script BYTEA,
    PRIMARY KEY (tx_hash, output_index)
  );
  CREATE TABLE IF NOT EXISTS mempool_ledger_assets (
    tx_hash BYTEA NOT NULL,
    output_index INTEGER NOT NULL,
    unit BYTEA,
    quantity NUMERIC NOT NULL,
    FOREIGN KEY (tx_hash, output_index)
      REFERENCES mempool_ledger(tx_hash, output_index)
      ON DELETE CASCADE
  );`;

export const insert = async (pool: Pool, utxos: UTxO[]) =>
  insertUTxOs(pool, "mempool_ledger", "mempool_ledger_assets", utxos);

export const retrieve = async (pool: Pool): Promise<UTxO[]> =>
  retrieveUTxOs(pool, "mempool_ledger", "mempool_ledger_assets");

export const retrieveByOutRef = async (
  pool: Pool,
  outRef: OutRef,
): Promise<UTxO[]> =>
  retrieveUTxOsByOutRef(
    pool,
    outRef,
    "mempool_ledger",
    "mempool_ledger_assets",
  );

export const retrieveUTxOsAtAddress = async (
  pool: Pool,
  address: string,
): Promise<UTxO[]> => {
  const query = `
    SELECT
      t.tx_hash,
      t.output_index,
      address,
      json_agg(json_build_object('unit', encode(a.unit, 'hex'), 'quantity', a.quantity)) AS assets,
      datum_hash,
      datum,
      script_ref_type,
      script_ref_script
    FROM mempool_ledger AS t
      LEFT JOIN mempool_ledger_assets AS a
        ON t.tx_hash = a.tx_hash AND t.output_index = a.output_index
    WHERE address = $1
    GROUP BY
      t.tx_hash,
      t.output_index,
      address,
      datum_hash,
      datum,
      script_ref_type,
      script_ref_script
    ORDER BY
      t.tx_hash,
      t.output_index
  ;`;

  try {
    const result = await pool.query(query, [address]);
    return result.rows.map((r) => utxoFromRow(r));
  } catch (err) {
    // logAbort(`mempool_ledger db: error retrieving utxos: ${err}`);
    throw err;
  }
};

export const clearUTxOs = async (pool: Pool, refs: OutRef[]) =>
  utils.clearUTxOs(pool, "mempool_ledger", refs);

export const clear = async (pool: Pool) => clearTable(pool, "mempool_ledger");
