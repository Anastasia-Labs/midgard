import { Effect } from "effect";
import { clearTable, mapSqlError } from "./utils.js";
import { SqlClient } from "@effect/sql";
import { Database } from "@/services/database.js";

export const tableName = "blocks";

export type BlockRecord = {
  block_header_hash: string;
  tx_hash: string;
};

export const createQuery = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`
  CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
    block_header_hash TEXT NOT NULL,
    tx_hash TEXT NOT NULL UNIQUE
  );`;
}).pipe(Effect.withLogSpan(`creating table ${tableName}`), mapSqlError);

export const insert = (headerHash: string, txHashes: string[]) =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`${tableName} db: attempt to insert blocks`);
    const sql = yield* SqlClient.SqlClient;

    if (!txHashes.length) {
      yield* Effect.logDebug("No txHashes provided, skipping block insertion.");
      return;
    }
    const rowsToInsert = txHashes.map((txHash: string) => ({
      block_header_hash: headerHash,
      tx_hash: txHash,
    }));

    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(rowsToInsert)}`;

    yield* Effect.logInfo(
      `${tableName} db: ${rowsToInsert.length} block rows inserted.`,
    );
  }).pipe(
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: inserting error: ${e}`),
    ),
    Effect.withLogSpan(`insert ${tableName}`),
    mapSqlError,
    Effect.asVoid,
  );

export const retrieveTxHashesByBlockHash = (blockHash: string) =>
  Effect.gen(function* () {
    yield* Effect.logDebug(
      `${tableName} db: attempt retrieve tx_hashes for block ${blockHash}`,
    );
    const sql = yield* SqlClient.SqlClient;

    const rows = yield* sql<{
      tx_hash: string;
    }>`SELECT tx_hash FROM ${sql(tableName)} WHERE block_header_hash = ${blockHash}`;

    const result = rows.map((row) => row.tx_hash);

    yield* Effect.logDebug(
      `${tableName} db: retrieved ${result.length} tx_hashes for block ${blockHash}`,
    );
    return result;
  }).pipe(
    Effect.withLogSpan(`retrieveTxHashesByBlockHash ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving tx_hashes error: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const retrieveBlockHashByTxHash = (
  txHash: Uint8Array,
): Effect.Effect<Uint8Array, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(
      `${tableName} db: attempt retrieve block_hash for tx_hash ${txHash}`,
    );
    const sql = yield* SqlClient.SqlClient;

    const rows = yield* sql<{
      block_header_hash: string;
    }>`SELECT block_header_hash FROM ${sql(tableName)} WHERE tx_hash = ${txHash} LIMIT 1`;

    if (rows.length <= 0) {
      const msg = `No block_hash found for ${txHash} tx_hash`;
      yield* Effect.logDebug(msg);
      yield* Effect.fail(new Error(msg));
    }
    const result = rows[0].block_header_hash;
    yield* Effect.logDebug(
      `${tableName} db: retrieved block_hash for tx ${txHash}: ${result}`,
    );
    return result;
  }).pipe(
    Effect.withLogSpan(`retrieveBlockHashByTxHash ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving block_hash error: ${JSON.stringify(e)}`,
      ),
    ),
    mapSqlError,
  );

export const clearBlock = (
  blockHash: Uint8Array,
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt clear block ${blockHash}`);
    const sql = yield* SqlClient.SqlClient;

    const result =
      yield* sql`DELETE FROM ${sql(tableName)} WHERE block_header_hash = ${blockHash}`;

    yield* Effect.logInfo(
      `${tableName} db: cleared ${result.length} rows for block ${blockHash}`,
    );
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

export const retrieve = (): Effect.Effect<BlockRecord[], Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`${tableName} db: attempt to retrieve blocks`);
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<BlockRecord>`SELECT * FROM ${sql(tableName)}`;
    yield* Effect.logDebug(`${tableName} db: retrieved ${rows.length} rows.`);
    return Array.from(rows);
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
