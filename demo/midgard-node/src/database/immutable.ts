import { fromHex, toHex } from "@lucid-evolution/lucid";
import { Option } from "effect";
import sqlite3 from "sqlite3";
import { logAbort, logInfo } from "../utils.js";
import * as utils from "./utils.js";
import { clearTable } from "./utils.js";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS immutable (
    tx_hash BLOB NOT NULL UNIQUE,
    tx_cbor BLOB NOT NULL UNIQUE,
    PRIMARY KEY (tx_hash)
  );`;

export const insert = async (
  db: sqlite3.Database,
  tx_hash: string,
  tx_cbor: string,
) => {
  const query = `INSERT INTO immutable (tx_hash, tx_cbor) VALUES (?, ?)`;
  await new Promise<void>((resolve, reject) => {
    db.run(query, [fromHex(tx_hash), fromHex(tx_cbor)], function (err) {
      if (err) {
        logAbort(`immutable db: error inserting tx: ${err.message}`);
        reject(err);
      } else {
        logInfo(`immutable db: tx stored with rowid ${this.lastID}`);
        resolve();
      }
    });
  });
};

export const insertTxs = async (
  db: sqlite3.Database,
  txs: { txHash: string; txCbor: string }[],
): Promise<void> => {
  const query = `INSERT INTO immutable (tx_hash, tx_cbor) VALUES (?, ?)`;

  await new Promise<void>((resolve, reject) => {
    const stmt = db.prepare(query);

    for (const { txHash, txCbor } of txs) {
      stmt.run([txHash, txCbor], function (err) {
        if (err) {
          logAbort(`immutable db: error inserting tx: ${err.message}`);
          reject(err);
          return;
        }
        logInfo(`immutable db: tx stored with rowid ${this.lastID}`);
      });
    }

    stmt.finalize((err) => {
      if (err) {
        reject(err);
      } else {
        resolve();
      }
    });
  });
};

export const retrieve = async (db: sqlite3.Database) => {
  const query = `SELECT * FROM immutable`;
  const result = await new Promise<[string, string][]>((resolve, reject) => {
    db.all(query, (err, rows: { tx_hash: Buffer; tx_cbor: Buffer }[]) => {
      if (err) {
        logAbort(`immutable db: retrieving error: ${err.message}`);
        reject(err);
      }
      const result: [string, string][] = rows.map((row) => [
        toHex(new Uint8Array(row.tx_hash)),
        toHex(new Uint8Array(row.tx_cbor)),
      ]);
      resolve(result);
    });
  });
  return result;
};

export const retrieveTxCborByHash = async (
  db: sqlite3.Database,
  txHash: string,
): Promise<Option.Option<string>> =>
  utils.retrieveTxCborByHash(db, "immutable", txHash);

export const retrieveTxCborsByHashes = async (
  db: sqlite3.Database,
  txHashes: string[],
): Promise<string[]> =>
  utils.retrieveTxCborsByHashes(db, "immutable", txHashes);

export const clear = async (db: sqlite3.Database) =>
  clearTable(db, "immutable");
