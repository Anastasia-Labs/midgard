import { Effect } from "effect";
import { SqlClient } from "@effect/sql";
import {
  AddressHistoryDB,
  BlocksDB,
  BlocksTxsDB,
  ConfirmedLedgerDB,
  ImmutableDB,
  LatestLedgerDB,
  MempoolDB,
  ProcessedMempoolDB,
  MempoolLedgerDB,
  DepositsDB,
  TxOrdersDB,
  WithdrawalsDB,
  Tx,
  Ledger,
  UserEvents,
} from "./index.js";
import { Database, NodeConfig } from "@/services/index.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

export const program: Effect.Effect<
  void,
  DatabaseError,
  Database | NodeConfig
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  // yield* sql`SET default_transaction_read_only TO 'off'`;
  yield* sql`SET client_min_messages = 'error'`;
  yield* sql`SET default_transaction_isolation TO 'serializable'`;

  yield* AddressHistoryDB.createTable;
  yield* BlocksDB.createTable;
  yield* BlocksTxsDB.createTable;
  yield* Ledger.createTable(ConfirmedLedgerDB.tableName);
  yield* Ledger.createTable(LatestLedgerDB.tableName);
  yield* Ledger.createTable(MempoolLedgerDB.tableName);
  yield* Tx.createTable(ImmutableDB.tableName);
  yield* Tx.createTable(MempoolDB.tableName);
  yield* Tx.createTable(ProcessedMempoolDB.tableName);
  yield* UserEvents.createTable(DepositsDB.tableName);
  yield* UserEvents.createTable(TxOrdersDB.tableName);
  yield* UserEvents.createTable(WithdrawalsDB.tableName);

  yield* Effect.logInfo("PostgreSQL database initialized Successfully.");
}).pipe(
  sqlErrorToDatabaseError("all tables", "Failed to initialize the tables"),
);
