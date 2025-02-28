import { Address, OutRef, UTxO } from "@lucid-evolution/lucid";
import sqlite3 from "sqlite3";
import * as utils from "./utils.js";
import {
  clearTable,
  insertUTxOs,
  retrieveUTxOs,
  utxoFromRow,
  UTxOFromRow,
} from "./utils.js";
import { Effect } from "effect"

export const createQuery = `
  CREATE TABLE IF NOT EXISTS mempool_ledger (
    tx_hash BLOB NOT NULL,
    output_index INTEGER NOT NULL,
    address TEXT NOT NULL,
    datum_hash BLOB,
    datum BLOB,
    script_ref_type VARCHAR (8),
    script_ref_script BLOB,
    PRIMARY KEY (tx_hash, output_index)
  );
  CREATE TABLE IF NOT EXISTS mempool_ledger_assets (
    tx_hash BLOB NOT NULL,
    output_index INTEGER NOT NULL,
    unit BLOB,
    quantity BIGINT NOT NULL,
    FOREIGN KEY (tx_hash, output_index)
      REFERENCES mempool_ledger(tx_hash, output_index)
      ON DELETE CASCADE
  );`;

export const insert = async (db: sqlite3.Database, utxos: UTxO[]) =>
  insertUTxOs(db, "mempool_ledger", "mempool_ledger_assets", utxos);

export const retrieve = async (db: sqlite3.Database): Promise<UTxO[]> =>
  retrieveUTxOs(db, "mempool_ledger", "mempool_ledger_assets");

export const retrieveUTxOsAtAddress = async (
  db: sqlite3.Database,
  address: Address,
): Promise<UTxO[]> => {
  const query = `
      SELECT
        t.tx_hash,
        t.output_index,
        address,
        json_group_array(json_object('unit', hex(a.unit), 'quantity', a.quantity)) AS assets,
        datum_hash,
        datum,
        script_ref_type,
        script_ref_script
      FROM mempool_ledger AS t
        LEFT JOIN mempool_ledger_assets AS a
          ON t.tx_hash = a.tx_hash AND t.output_index = a.output_index
      WHERE address = ?
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
      ;
      `;
  return new Promise((resolve, reject) => {
    db.all(query, [address], (err, rows: UTxOFromRow[]) => {
      if (err) {
        Effect.logError(`mempool_ledger db: error retrieving utxos: ${err.message}`);
        return reject(err);
      }
      resolve(rows.map((r) => utxoFromRow(r)));
    });
  });
};

export const clearUTxOs = async (db: sqlite3.Database, refs: OutRef[]) =>
  utils.clearUTxOs(db, "mempool_ledger", refs);

export const clear = async (db: sqlite3.Database) =>
  clearTable(db, "mempool_ledger");
