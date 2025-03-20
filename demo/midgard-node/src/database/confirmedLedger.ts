import { OutRef, UTxO } from "@lucid-evolution/lucid";
import { Pool } from "pg";
import * as utils from "./utils.js";
import { clearTable, insertUTxOs, retrieveUTxOs } from "./utils.js";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS confirmed_ledger (
    tx_hash BYTEA NOT NULL,
    output_index INTEGER NOT NULL,
    address TEXT NOT NULL,
    datum_hash BYTEA,
    datum BYTEA,
    script_ref_type VARCHAR (8),
    script_ref_script BYTEA,
    PRIMARY KEY (tx_hash, output_index),
    FOREIGN KEY (tx_hash)
    REFERENCES immutable(tx_hash)
      ON DELETE CASCADE
  );
  CREATE TABLE IF NOT EXISTS confirmed_ledger_assets (
    tx_hash BYTEA NOT NULL,
    output_index INTEGER NOT NULL,
    unit BYTEA,
    quantity NUMERIC NOT NULL,
    FOREIGN KEY (tx_hash, output_index)
      REFERENCES confirmed_ledger(tx_hash, output_index)
      ON DELETE CASCADE
  );
  `;

export const insert = async (pool: Pool, utxos: UTxO[]): Promise<void> =>
  insertUTxOs(pool, "confirmed_ledger", "confirmed_ledger_assets", utxos);

export const retrieve = async (pool: Pool): Promise<UTxO[]> =>
  retrieveUTxOs(pool, "confirmed_ledger", "confirmed_ledger_assets");

export const clearUTxOs = async (pool: Pool, refs: OutRef[]) =>
  utils.clearUTxOs(pool, "confirmed_ledger", refs);

export const clear = async (pool: Pool): Promise<void> =>
  clearTable(pool, "confirmed_ledger");
