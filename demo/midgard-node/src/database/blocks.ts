import { Effect, Option } from "effect";
import { SqlClient, SqlError } from "@effect/sql";
import { clearTable } from "./utils.js";

export const tableName = "blocks";

export const createQuery = `
  CREATE TABLE IF NOT EXISTS ${tableName} (
    header_hash BYTEA NOT NULL,
    tx_hash BYTEA NOT NULL UNIQUE
  );`;

interface BlockRow {
  readonly header_hash: Uint8Array;
  readonly tx_hash: Uint8Array;
}

export const insert = (
  headerHash: Uint8Array,
  txHashes: Uint8Array[],
): Effect.Effect<void, SqlError.SqlError, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`${tableName} db: attempt to insert blocks`);
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

    yield* Effect.logInfo(
      `${tableName} db: ${rowsToInsert.length} block rows inserted.`,
    );
  }).pipe(
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: inserting error: ${JSON.stringify(e)}`),
    ),
    Effect.withLogSpan(`insert ${tableName}`),
    Effect.asVoid,
  );

export const retrieveTxHashesByBlockHash = (
  blockHash: Uint8Array,
): Effect.Effect<Uint8Array[], SqlError.SqlError, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(
      `${tableName} db: attempt retrieve tx_hashes for block ${blockHash}`,
    );
    const sql = yield* SqlClient.SqlClient;

    const rows = yield* sql<{
      tx_hash: Uint8Array;
    }>`SELECT tx_hash FROM ${sql(tableName)} WHERE header_hash = ${blockHash}`;

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
  );

export const retrieveBlockHashByTxHash = (
  txHash: Uint8Array,
): Effect.Effect<
  Option.Option<Uint8Array>,
  SqlError.SqlError,
  SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(
      `${tableName} db: attempt retrieve block_hash for tx_hash ${txHash}`,
    );
    const sql = yield* SqlClient.SqlClient;

    const rows = yield* sql<{
      header_hash: Uint8Array;
    }>`SELECT header_hash FROM ${sql(tableName)} WHERE tx_hash = ${txHash} LIMIT 1`;

    const result =
      rows.length > 0
        ? Option.some(rows[0].header_hash)
        : Option.none<Uint8Array>();

    yield* Effect.logDebug(
      `${tableName} db: retrieved block_hash for tx ${txHash}: ${Option.match(result, { onNone: () => "None", onSome: () => "Some(...)})" })}`,
    );
    return result;
  }).pipe(
    Effect.withLogSpan(`retrieveBlockHashByTxHash ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving block_hash error: ${JSON.stringify(e)}`,
      ),
    ),
  );

export const clearBlock = (
  blockHash: Uint8Array,
): Effect.Effect<void, SqlError.SqlError, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt clear block ${blockHash}`);
    const sql = yield* SqlClient.SqlClient;

    const result =
      yield* sql`DELETE FROM ${sql(tableName)} WHERE header_hash = ${blockHash}`;

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
  );

export const retrieve = (): Effect.Effect<
  [Uint8Array, Uint8Array][],
  SqlError.SqlError,
  SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`${tableName} db: attempt to retrieve blocks`);
    const sql = yield* SqlClient.SqlClient;
    const rows =
      yield* sql<BlockRow>`SELECT header_hash, tx_hash FROM ${sql(tableName)}`;
    const result: [Uint8Array, Uint8Array][] = rows.map(
      (row: BlockRow) => [row.header_hash, row.tx_hash] as const,
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
  );

export const clear = (): Effect.Effect<
  void,
  SqlError.SqlError,
  SqlClient.SqlClient
> => clearTable(tableName).pipe(Effect.withLogSpan(`clear ${tableName}`));
