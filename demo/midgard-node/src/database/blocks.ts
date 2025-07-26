import { Effect } from "effect";
import { clearTable, mapSqlError } from "./utils.js";
import { SqlClient, SqlError } from "@effect/sql";
import { Database } from "@/services/database.js";

export const tableName = "blocks";

export const init = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`
  CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
    headerHash BYTEA NOT NULL,
    txHash BYTEA NOT NULL UNIQUE
  );`;
});

export const insertBlock = (
  headerHash: Buffer,
  txHashes: Buffer[],
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    // yield* Effect.logInfo(`${tableName} db: attempt to insert blocks`);
    const sql = yield* SqlClient.SqlClient;

    if (txHashes.length <= 0) {
      yield* Effect.logDebug("No txHashes provided, skipping block insertion.");
      return;
    }
    const rowsToInsert = txHashes.map((txHash: Uint8Array) => ({
      headerHash: headerHash,
      txHash: txHash,
    }));

    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(rowsToInsert)}`;

    // yield* Effect.logInfo(
    //   `${tableName} db: ${rowsToInsert.length} block rows inserted.`,
    // );
  }).pipe(
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: inserting error: ${e}`),
    ),
    Effect.withLogSpan(`insert ${tableName}`),
    mapSqlError,
    Effect.asVoid,
  );

export const retrieveTxHashesByHeaderHash = (
  headerHash: Buffer,
): Effect.Effect<readonly Buffer[], Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(
      `${tableName} db: attempt retrieve txHashes for block ${headerHash}`,
    );
    const sql = yield* SqlClient.SqlClient;

    const result = yield* sql<Buffer>`SELECT txHash FROM ${sql(
      tableName,
    )} WHERE headerHash = ${headerHash}`;

    yield* Effect.logDebug(
      `${tableName} db: retrieved ${result.length} txHashes for block ${headerHash}`,
    );
    return result;
  }).pipe(
    Effect.withLogSpan(`retrieveTxHashesByBlockHash ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving txHashes error: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const retrieveHeaderHashByTxHash = (
  txHash: Buffer,
): Effect.Effect<Uint8Array, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(
      `${tableName} db: attempt retrieve headerHash for txHash ${txHash}`,
    );
    const sql = yield* SqlClient.SqlClient;

    const rows = yield* sql<Buffer>`SELECT headerHash FROM ${sql(
      tableName,
    )} WHERE txHash = ${txHash} LIMIT 1`;

    if (rows.length <= 0) {
      const msg = `No headerHash found for ${txHash} txHash`;
      const err = new SqlError.SqlError({ cause: msg, message: msg });
      yield* Effect.logDebug(msg);
      yield* Effect.fail(err);
    }
    const result = rows[0];
    yield* Effect.logDebug(
      `${tableName} db: retrieved headerHash for tx ${txHash}: ${result}`,
    );
    return result;
  }).pipe(
    Effect.withLogSpan(`retrieveBlockHashByTxHash ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving headerHash error: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const clearBlock = (
  headerHash: Buffer,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(
      `${tableName} db: attempt clear block ${headerHash}`,
    );
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM ${sql(tableName)} WHERE headerHash = ${headerHash}`;
    // yield* Effect.logInfo(
    //   `${tableName} db: cleared ${result.entries()} rows for block ${toHex(headerHash)}`,
    // );
    return Effect.void;
  }).pipe(
    Effect.withLogSpan(`clearBlock ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: clearing block error: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

type BlockRow = {
  readonly headerHash: Buffer;
  readonly txHash: Buffer;
};

export const retrieve = (): Effect.Effect<
  readonly BlockRow[],
  Error,
  Database
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`${tableName} db: attempt to retrieve blocks`);
    const sql = yield* SqlClient.SqlClient;
    const result = yield* sql<BlockRow>`SELECT * FROM ${sql(tableName)}`;
    yield* Effect.logDebug(`${tableName} db: retrieved ${result.length} rows.`);
    return result;
  }).pipe(
    Effect.withLogSpan(`retrieve ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving error: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const clear = (): Effect.Effect<void, Error, Database> =>
  clearTable(tableName).pipe(Effect.withLogSpan(`clear ${tableName}`));
