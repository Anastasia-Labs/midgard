import { SqlClient, SqlError } from "@effect/sql";
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
import { insertGenesisUtxos } from "./genesis.js";
import { Database, NodeConfig } from "@/services/index.js";
import { DBCreateError, DBOtherError } from "./utils/common.js";

export const initializeDb: () => Effect.Effect<
  void,
  DBCreateError | DBOtherError,
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
  }).pipe(
    Effect.mapError((error: unknown) =>
      error instanceof SqlError.SqlError
        ? new DBOtherError({
            message: `Failed to initialize database`,
            cause: error,
          })
        : new DBOtherError({
            message: `Unknown error during database initialization: ${error}`,
            cause: error,
          }),
    ),
  );
