import { OutRef, UTxO } from "@lucid-evolution/lucid";
import sqlite3 from "sqlite3";
import * as utils from "./utils.js";
import { clearTable, insertUTxOs, retrieveUTxOs } from "./utils.js";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS latest_ledger (
    tx_hash BLOB NOT NULL,
    output_index INTEGER NOT NULL,
    address TEXT NOT NULL,
    datum_hash BLOB,
    datum BLOB,
    script_ref_type VARCHAR (8),
    script_ref_script BLOB,
    PRIMARY KEY (tx_hash, output_index)
  );
  CREATE TABLE IF NOT EXISTS latest_ledger_assets (
    tx_hash BLOB NOT NULL,
    output_index INTEGER NOT NULL,
    unit BLOB,
    quantity BIGINT NOT NULL,
    FOREIGN KEY (tx_hash, output_index)
      REFERENCES latest_ledger(tx_hash, output_index)
      ON DELETE CASCADE
  );
  `;

export const insert = async (db: sqlite3.Database, utxos: UTxO[]) =>
  insertUTxOs(db, "latest_ledger", "latest_ledger_assets", utxos);

export const retrieve = async (db: sqlite3.Database): Promise<UTxO[]> =>
  retrieveUTxOs(db, "latest_ledger", "latest_ledger_assets");

export const clearUTxOs = async (db: sqlite3.Database, refs: OutRef[]) =>
  utils.clearUTxOs(db, "latest_ledger", refs);

export const clear = async (db: sqlite3.Database) =>
  clearTable(db, "latest_ledger");
