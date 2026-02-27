import { Effect } from "effect";
import {
  clearTable,
  DatabaseError,
  NotFoundError,
} from "@/database/utils/common.js";
import { SqlClient } from "@effect/sql";
import { Database } from "@/services/database.js";
import { sqlErrorToDatabaseError } from "@/database/utils/common.js";

export const tableName = "blocks";

export enum Columns {
  HEIGHT = "height",
  HEADER_HASH = "header_hash",
  TX_ID = "tx_id",
  TIMESTAMPTZ = "time_stamp_tz",
}

export enum ColumnsIndices {
  HEADER_HASH = "idx_blocks_header_hash",
  TX_ID = "idx_blocks_tx_id",
}

type EntryNoHeightAndTS = {
  [Columns.HEADER_HASH]: Buffer;
  [Columns.TX_ID]: Buffer;
};

type Entry = EntryNoHeightAndTS & {
  [Columns.HEIGHT]: number;
  [Columns.TIMESTAMPTZ]: Date;
};

export const createTable: Effect.Effect<void, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
      ${sql(Columns.HEIGHT)} SERIAL PRIMARY KEY,
      ${sql(Columns.HEADER_HASH)} BYTEA NOT NULL,
      ${sql(Columns.TX_ID)} BYTEA NOT NULL UNIQUE,
      ${sql(Columns.TIMESTAMPTZ)} TIMESTAMPTZ NOT NULL DEFAULT(NOW())
    );`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          ColumnsIndices.HEADER_HASH,
        )} ON ${sql(tableName)} (${sql(Columns.HEADER_HASH)});`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          ColumnsIndices.TX_ID,
        )} ON ${sql(tableName)} (${sql(Columns.TX_ID)});`;
      }),
    );
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to create the table"));

export const insert = (
  headerHash: Buffer,
  txHashes: Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    if (txHashes.length <= 0) {
      yield* Effect.logDebug("No txHashes provided, skipping block insertion.");
      return;
    }
    const rowsToInsert: EntryNoHeightAndTS[] = txHashes.map(
      (txHash: Buffer) => ({
        [Columns.HEADER_HASH]: headerHash,
        [Columns.TX_ID]: txHash,
      }),
    );
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(rowsToInsert)}`;
  }).pipe(
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: inserting error: ${e}`),
    ),
    Effect.withLogSpan(`insert ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to insert the given block"),
  );

export const retrieveTxHashesByHeaderHash = (
  headerHash: Buffer,
): Effect.Effect<readonly Buffer[], DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(
      `${tableName} db: attempt retrieve txHashes for block ${headerHash}`,
    );
    const sql = yield* SqlClient.SqlClient;

    const result = yield* sql<
      Pick<Entry, Columns.TX_ID>
    >`SELECT ${sql(Columns.TX_ID)} FROM ${sql(
      tableName,
    )} WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}`;

    yield* Effect.logDebug(
      `${tableName} db: retrieved ${result.length} txHashes for block ${headerHash}`,
    );
    return result.map((row) => row[Columns.TX_ID]);
  }).pipe(
    Effect.withLogSpan(`retrieveTxHashesByHeaderHash ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving txHashes error: ${JSON.stringify(e)}`,
      ),
    ),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve transactions of the given block",
    ),
  );

export const retrieveHeaderHashByTxHash = (
  txHash: Buffer,
): Effect.Effect<Buffer, DatabaseError | NotFoundError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(
      `${tableName} db: attempt retrieve headerHash for txHash ${txHash}`,
    );
    const sql = yield* SqlClient.SqlClient;

    const rows = yield* sql<Pick<Entry, Columns.HEADER_HASH>>`SELECT ${sql(
      Columns.HEADER_HASH,
    )} FROM ${sql(tableName)} WHERE ${sql(Columns.TX_ID)} = ${txHash} LIMIT 1`;

    if (rows.length <= 0) {
      const msg = `No headerHash found for ${txHash} txHash`;
      yield* Effect.logDebug(msg);
      yield* Effect.fail(
        new NotFoundError({
          message: "No header hash found for tx hash",
          cause: msg,
          table: tableName,
          txIdHex: txHash.toString("hex"),
        }),
      );
    }
    const result = rows[0][Columns.HEADER_HASH];
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
    Effect.mapError((error): DatabaseError | NotFoundError =>
      error._tag === "NotFoundError"
        ? error
        : new DatabaseError({
            message: "Failed to retrieve header hash of the given block",
            table: tableName,
            cause: error,
          }),
    ),
  );

export const clearBlock = (
  headerHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(
      `${tableName} db: attempt clear block ${headerHash}`,
    );
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM ${sql(
      tableName,
    )} WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}`;
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
    sqlErrorToDatabaseError(
      tableName,
      "Failed to delete transactions of the given block",
    ),
  );

export const retrieve: Effect.Effect<
  readonly Entry[],
  DatabaseError,
  Database
> = Effect.gen(function* () {
  yield* Effect.logInfo(`${tableName} db: attempt to retrieve blocks`);
  const sql = yield* SqlClient.SqlClient;
  const result = yield* sql<Entry>`SELECT * FROM ${sql(tableName)}`;
  yield* Effect.logDebug(`${tableName} db: retrieved ${result.length} rows.`);
  return result;
}).pipe(
  Effect.withLogSpan(`retrieve ${tableName}`),
  Effect.tapErrorTag("SqlError", (e) =>
    Effect.logError(`${tableName} db: retrieving error: ${JSON.stringify(e)}`),
  ),
  sqlErrorToDatabaseError(
    tableName,
    "Failed to retrieve transactions of all the blocks",
  ),
);

export const clear: Effect.Effect<void, DatabaseError, Database> =
  clearTable(tableName);
