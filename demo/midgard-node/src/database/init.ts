import * as BlocksDB from "./blocks.js";
import * as ConfirmedLedgerDB from "./confirmedLedger.js";
import * as ImmutableDB from "./immutable.js";
import * as LatestLedgerDB from "./latestLedger.js";
import * as LatestLedgerCloneDB from "./latestLedgerClone.js";
import * as MempoolDB from "./mempool.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import { logAbort, logInfo } from "../utils.js";
import { Sql } from "postgres";

export const initializeDb = async (sql: Sql) => {
  try {
    // Set transaction isolation level
    await sql`SET default_transaction_isolation TO 'serializable'`;

    // Create tables
    await BlocksDB.createQuery(sql);
    await MempoolDB.createQuery(sql);
    await MempoolLedgerDB.createQuery(sql);
    await ImmutableDB.createQuery(sql);
    await ConfirmedLedgerDB.createQuery(sql);
    await LatestLedgerDB.createQuery(sql);
    await LatestLedgerCloneDB.createQuery(sql);

    logInfo("Connected to the PostgreSQL database");
    return sql;
  } catch (err) {
    logAbort(
      `Error initializing database: ${err instanceof Error ? err.message : err}`,
    );
    throw err;
  }
};
