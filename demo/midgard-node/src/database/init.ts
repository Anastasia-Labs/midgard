import { SqlClient } from "@effect/sql";
import * as BlocksDB from "./blocks.js";
import * as ConfirmedLedgerDB from "./confirmedLedger.js";
import * as ImmutableDB from "./immutable.js";
import * as LatestLedgerDB from "./latestLedger.js";
import * as MempoolDB from "./mempool.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import { createKeyValueTable, createLedgerTable } from "./utils.js";
import { Effect } from "effect";
import { Database } from "@/services/database.js";

export const initializeDb: () => Effect.Effect<void, Error, Database> = () =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    // yield* sql`SET default_transaction_read_only TO 'off'`;
    yield* sql`SET client_min_messages = 'error'`;
    yield* sql`SET default_transaction_isolation TO 'serializable'`;

    yield* BlocksDB.init;
    yield* MempoolDB.init;
    yield* createLedgerTable(MempoolLedgerDB.tableName);
    yield* createKeyValueTable(ImmutableDB.tableName);
    yield* createLedgerTable(ConfirmedLedgerDB.tableName);
    yield* createLedgerTable(LatestLedgerDB.tableName);

    Effect.logInfo("Connected to the PostgreSQL database");
  });
