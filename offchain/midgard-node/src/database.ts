import sqlite3 from "sqlite3";
import * as blocks from "./database/blocks.js";
import * as confirmedLedger from "./database/confirmedLedger.js";
import * as immutable from "./database/immutable.js";
import * as latestLedger from "./database/latestLedger.js";
import * as mempool from "./database/mempool.js";
import * as mempoolLedger from "./database/mempoolLedger.js";
import { logAbort, logInfo } from "./utils.js";

export async function initializeDb(dbFilePath: string) {
  const db = new sqlite3.Database(dbFilePath, (err) => {
    if (err) {
      logAbort(`Error opening database: ${err.message}`);
    } else {
      logInfo("Connected to the SQLite database");
    }
  });
  db.exec(`
      PRAGMA foreign_keys = ON;
      PRAGMA read_uncommitted=false;
  `);
  db.exec(blocks.createQuery);
  db.exec(mempool.createQuery);
  db.exec(mempoolLedger.createQuery);
  db.exec(immutable.createQuery);
  db.exec(confirmedLedger.createQuery);
  db.exec(latestLedger.createQuery);
  return db;
}
