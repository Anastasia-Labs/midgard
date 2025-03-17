import { OutRef, UTxO } from "@lucid-evolution/lucid";
import { Pool } from "pg";
import * as utils from "./utils.js";
import {
  clearTable,
  insertUTxOs,
  retrieveUTxOs,
  retrieveUTxOsByOutRef,
} from "./utils.js";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS latest_ledger (
    tx_hash BYTEA NOT NULL,
    output_index INTEGER NOT NULL,
    address TEXT NOT NULL,
    datum_hash BYTEA,
    datum BYTEA,
    script_ref_type VARCHAR (8),
    script_ref_script BYTEA,
    PRIMARY KEY (tx_hash, output_index)
  );
  CREATE TABLE IF NOT EXISTS latest_ledger_assets (
    tx_hash BYTEA NOT NULL,
    output_index INTEGER NOT NULL,
    unit BYTEA,
    quantity NUMERIC NOT NULL,
    FOREIGN KEY (tx_hash, output_index)
      REFERENCES latest_ledger(tx_hash, output_index)
      ON DELETE CASCADE
  );
  `;

export const insert = async (pool: Pool, utxos: UTxO[]) =>
  insertUTxOs(pool, "latest_ledger", "latest_ledger_assets", utxos);

export const retrieve = async (pool: Pool): Promise<UTxO[]> =>
  retrieveUTxOs(pool, "latest_ledger", "latest_ledger_assets");

export const retrieveByOutRef = async (
  pool: Pool,
  outRef: OutRef,
): Promise<UTxO[]> =>
  retrieveUTxOsByOutRef(pool, outRef, "latest_ledger", "latest_ledger_assets");

export const clearUTxOs = async (pool: Pool, refs: OutRef[]) =>
  utils.clearUTxOs(pool, "latest_ledger", refs);

export const clear = async (pool: Pool) => clearTable(pool, "latest_ledger");
