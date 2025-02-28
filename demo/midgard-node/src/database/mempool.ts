import { fromHex, toHex } from "@lucid-evolution/lucid";
import { Option } from "effect";
import sqlite3 from "sqlite3";
import { logAbort, logInfo } from "../utils.js";
import * as utils from "./utils.js";
import { clearTable } from "./utils.js";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS mempool (
    tx_hash BLOB NOT NULL UNIQUE,
    tx_cbor BLOB NOT NULL UNIQUE,
    PRIMARY KEY (tx_hash)
  );`;

export const insert = async (
  db: sqlite3.Database,
  tx_hash: string,
  tx_cbor: string,
) => {
  const query = `INSERT INTO mempool (tx_hash, tx_cbor) VALUES (?, ?)`;
  await new Promise<void>((resolve, reject) => {
    db.run(query, [fromHex(tx_hash), fromHex(tx_cbor)], function (err) {
      if (err) {
        logAbort(`mempool db: error inserting tx: ${err.message}`);
        reject(err);
      } else {
        logInfo(`mempool db: tx stored with rowid ${this.lastID}`);
        resolve();
      }
    });
  });
};

export const retrieveTxCborByHash = async (
  db: sqlite3.Database,
  txHash: string,
): Promise<Option.Option<string>> =>
  utils.retrieveTxCborByHash(db, "mempool", txHash);

export const retrieveTxCborsByHashes = async (
  db: sqlite3.Database,
  txHashes: string[],
): Promise<string[]> => utils.retrieveTxCborsByHashes(db, "mempool", txHashes);

export const retrieve = async (db: sqlite3.Database) => {
  const query = `SELECT * FROM mempool`;
  const mempool = await new Promise<[string, string][]>((resolve, reject) => {
    db.all(query, (err, rows: { tx_hash: Buffer; tx_cbor: Buffer }[]) => {
      if (err) {
        logAbort(`mempool db: retrieving error: ${err.message}`);
        reject(err);
      }
      const result: [string, string][] = rows.map((row) => [
        toHex(new Uint8Array(row.tx_hash)),
        toHex(new Uint8Array(row.tx_cbor)),
      ]);
      resolve(result);
    });
  });
  return mempool;
};

export const clear = async (db: sqlite3.Database) => clearTable(db, "mempool");
