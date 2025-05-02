import * as BlocksDB from "./blocks.js";
import * as ConfirmedLedgerDB from "./confirmedLedger.js";
import * as ImmutableDB from "./immutable.js";
import * as LatestLedgerDB from "./latestLedger.js";
import * as LatestLedgerCloneDB from "./latestLedgerClone.js";
import * as MempoolDB from "./mempool.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";

import { Effect } from "effect";
import { SqlClient, SqlError } from "@effect/sql";
import { mapSqlError } from "./utils.js";

const executeCreateQuery = (
  sql: SqlClient.SqlClient,
  module: { tableName: string; createQuery: string },
) =>
  Effect.flatMap(Effect.logDebug(`Creating table ${module.tableName}...`), () =>
    sql.unsafe(module.createQuery).pipe(
      Effect.tapBoth({
        onFailure: (e) =>
          Effect.logError(`Failed to create table ${module.tableName}`, e),
        onSuccess: () =>
          Effect.logDebug(
            `Table ${module.tableName} created or already exists.`,
          ),
      }),
    ),
  );

export const initializeDb = (): Effect.Effect<
  void,
  Error,
  SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo("Initializing database schema...");
    const sql = yield* SqlClient.SqlClient;

    yield* executeCreateQuery(sql, BlocksDB);
    yield* executeCreateQuery(sql, MempoolDB);
    yield* executeCreateQuery(sql, MempoolLedgerDB);
    yield* executeCreateQuery(sql, ImmutableDB);
    yield* executeCreateQuery(sql, ConfirmedLedgerDB);
    yield* executeCreateQuery(sql, LatestLedgerDB);
    yield* executeCreateQuery(sql, LatestLedgerCloneDB);

    yield* Effect.logInfo("Database schema initialization completed.");
  }).pipe(
    Effect.withLogSpan("initializeDb"),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError("Database initialization failed", e),
    ),
    Effect.asVoid,
    mapSqlError,
  );
