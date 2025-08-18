import { SqlClient } from "@effect/sql";
import * as BlocksDB from "./blocks.js";
import * as ConfirmedLedgerDB from "./confirmedLedger.js";
import * as ImmutableDB from "./immutable.js";
import * as LatestLedgerDB from "./latestLedger.js";
import * as MempoolDB from "./mempool.js";
import * as ProcessedMempoolDB from "./processedMempool.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import * as Tx from "@/database/utils/tx.js";
import * as Ledger from "@/database/utils/ledger.js";
import { Effect } from "effect";
import { Database } from "@/services/database.js";
import { insertGenesisUtxos } from "./genesis.js";
import { NodeConfig } from "@/config.js";

export const initializeDb: () => Effect.Effect<
  void,
  Error,
  Database | NodeConfig
> = () =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    // yield* sql`SET default_transaction_read_only TO 'off'`;
    yield* sql`SET client_min_messages = 'error'`;
    yield* sql`SET default_transaction_isolation TO 'serializable'`;

    yield* BlocksDB.init;
    yield* Tx.createTable(MempoolDB.tableName);
    yield* Tx.createTable(ProcessedMempoolDB.tableName);
    yield* Ledger.createTable(MempoolLedgerDB.tableName);
    yield* Tx.createTable(ImmutableDB.tableName);
    yield* Ledger.createTable(ConfirmedLedgerDB.tableName);
    yield* Ledger.createTable(LatestLedgerDB.tableName);

    yield* insertGenesisUtxos;

    yield* Effect.logInfo("PostgreSQL database initialized Successfully.");
  });
