import { Effect } from "effect";
import { clearTable, mapSqlError } from "./utils.js";
import { SqlClient } from "@effect/sql";
import { Database } from "@/services/database.js";
import { toHex } from "@lucid-evolution/lucid";

export const tableName = "blocks";

export const createQuery = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* sql`
  CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
    header_hash BYTEA NOT NULL,
    tx_hash BYTEA NOT NULL UNIQUE
  );`;
});

export const insert = (
  headerHash: Uint8Array,
  txHashes: Uint8Array[],
): Effect.Effect<void, Error, Database> =>
  Effect.gen(function* () {
    // yield* Effect.logInfo(`${tableName} db: attempt to insert blocks`);
    const sql = yield* SqlClient.SqlClient;

    if (!txHashes.length) {
      yield* Effect.logDebug("No txHashes provided, skipping block insertion.");
      return;
    }
    const headerHashBuffer = Buffer.from(headerHash);
    const rowsToInsert = txHashes.map((txHash: Uint8Array) => ({
      header_hash: headerHashBuffer,
      tx_hash: txHash,
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

export const retrieveTxHashesByBlockHash = (
  blockHash: Uint8Array,
): Effect.Effect<Uint8Array[], Error, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(
      `${tableName} db: attempt retrieve tx_hashes for block ${blockHash}`,
    );
    const sql = yield* SqlClient.SqlClient;

    const rows = yield* sql<{
      tx_hash: Uint8Array;
    }>`SELECT tx_hash FROM ${sql(tableName)} WHERE header_hash = ${blockHash}`;

    const result = rows.map((row) => Uint8Array.from(row.tx_hash));

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
      header_hash: Uint8Array;
    }>`SELECT header_hash FROM ${sql(tableName)} WHERE tx_hash = ${Buffer.from(txHash)} LIMIT 1`;

    if (rows.length <= 0) {
      const msg = `No block_hash found for ${txHash} tx_hash`;
      yield* Effect.logDebug(msg);
      yield* Effect.fail(new Error(msg));
    }
    const result = rows[0].header_hash;
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
    yield* sql`DELETE FROM ${sql(tableName)} WHERE header_hash = ${Buffer.from(blockHash)}`;
    // yield* Effect.logInfo(
    //   `${tableName} db: cleared ${result.entries()} rows for block ${toHex(blockHash)}`,
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

interface BlockRow {
  readonly header_hash: Uint8Array;
  readonly tx_hash: Uint8Array;
}

export const retrieve = (): Effect.Effect<
  (readonly [Uint8Array, Uint8Array])[],
  Error,
  Database
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`${tableName} db: attempt to retrieve blocks`);
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<BlockRow>`SELECT * FROM ${sql(tableName)}`;
    const result: (readonly [Uint8Array, Uint8Array])[] = rows.map(
      (row: BlockRow) =>
        [
          Uint8Array.from(row.header_hash),
          Uint8Array.from(row.tx_hash),
        ] as const,
    );
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
