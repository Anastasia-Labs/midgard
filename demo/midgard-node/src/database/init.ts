import * as BlocksDB from "./blocks.js";
import * as ConfirmedLedgerDB from "./confirmedLedger.js";
import * as ImmutableDB from "./immutable.js";
import * as LatestLedgerDB from "./latestLedger.js";
import * as LatestLedgerCloneDB from "./latestLedgerClone.js";
import * as MempoolDB from "./mempool.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import { Pool } from "pg";
import { logAbort, logInfo } from "../utils.js";

export const initializeDb = async (pool: Pool) => {
  try {
    await pool.query(`
      SET default_transaction_isolation TO 'serializable';
    `);

    await pool.query(BlocksDB.createQuery);
    await pool.query(MempoolDB.createQuery);
    await pool.query(MempoolLedgerDB.createQuery);
    await pool.query(ImmutableDB.createQuery);
    await pool.query(ConfirmedLedgerDB.createQuery);
    await pool.query(LatestLedgerDB.createQuery);
    await pool.query(LatestLedgerCloneDB.createQuery);

    logInfo("Connected to the PostgreSQL database");
    return pool;
  } catch (err) {
    logAbort(`Error initializing database: ${err}`);
    throw err;
  }
};
