import { Option } from "effect";
import { Effect } from "effect";
import { SqlClient, SqlError } from "@effect/sql";

export const mapSqlError = Effect.mapError(
  (sqlError: SqlError.SqlError) =>
    new Error(`SQL Error (${sqlError._tag}): ${JSON.stringify(sqlError)}`),
);

export const clearUTxOs = (
  tableName: string,
  refs: Uint8Array[],
): Effect.Effect<void, SqlError.SqlError, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to clear UTXOs by refs`);
    const sql = yield* SqlClient.SqlClient;

    if (refs.length === 0) {
      yield* Effect.logDebug("No refs provided, skipping UTXO clearing.");
      return;
    }

    const result =
      yield* sql`DELETE FROM ${sql(tableName)} WHERE ${sql.in("tx_in_cbor", refs)}`;

    yield* Effect.logInfo(
      `${tableName} db: cleared ${result.length} UTXO rows by refs`,
    );
  }).pipe(
    Effect.withLogSpan(`clearUTxOs ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: clearing UTXOs error: ${JSON.stringify(e)}`,
      ),
    ),
  );

export const clearTxs = (
  tableName: string,
  txHashes: Uint8Array[],
): Effect.Effect<void, SqlError.SqlError, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to clear txs by hashes`);
    const sql = yield* SqlClient.SqlClient;

    if (txHashes.length === 0) {
      yield* Effect.logDebug("No txHashes provided, skipping tx clearing.");
      return;
    }

    const result =
      yield* sql`DELETE FROM ${sql(tableName)} WHERE ${sql.in("tx_hash", txHashes)}`;

    yield* Effect.logInfo(`${tableName} db: ${result.length} txs removed`);
  }).pipe(
    Effect.withLogSpan(`clearTxs ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: txs removing error: ${JSON.stringify(e)}`,
      ),
    ),
  );

export const retrieveTxCborByHash = (
  tableName: string,
  txHash: Uint8Array,
): Effect.Effect<
  Option.Option<Uint8Array>,
  SqlError.SqlError,
  SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(
      `${tableName} db: attempt retrieve tx_cbor for hash ${txHash}`,
    );
    const sql = yield* SqlClient.SqlClient;

    const rows = yield* sql<{
      tx_cbor: Uint8Array;
    }>`SELECT tx_cbor FROM ${sql(tableName)} WHERE tx_hash = ${txHash} LIMIT 1`;

    const result =
      rows.length > 0
        ? Option.some(rows[0].tx_cbor)
        : Option.none<Uint8Array>();

    yield* Effect.logDebug(
      `${tableName} db: retrieved tx_cbor for hash ${txHash}: ${Option.match(result, { onNone: () => "None", onSome: () => "Some(...)})" })}`,
    );
    return result;
  }).pipe(
    Effect.withLogSpan(`retrieveTxCborByHash ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving tx_cbor error: ${JSON.stringify(e)}`,
      ),
    ),
  );

export const retrieveTxCborsByHashes = (
  tableName: string,
  txHashes: Uint8Array[],
): Effect.Effect<Uint8Array[], SqlError.SqlError, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(
      `${tableName} db: attempt retrieve tx_cbors for hashes`,
    );
    const sql = yield* SqlClient.SqlClient;

    if (txHashes.length === 0) {
      yield* Effect.logDebug(
        "No txHashes provided, skipping tx_cbor retrieval.",
      );
      return [];
    }

    const rows = yield* sql<{
      tx_cbor: Uint8Array;
    }>`SELECT tx_cbor FROM ${sql(tableName)} WHERE ${sql.in("tx_hash", txHashes)}`;

    const result = rows.map((row) => row.tx_cbor);

    yield* Effect.logDebug(
      `${tableName} db: retrieved ${result.length} tx_cbors`,
    );
    return result;
  }).pipe(
    Effect.withLogSpan(`retrieveTxCborsByHashes ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving tx_cbors error: ${JSON.stringify(e)}`,
      ),
    ),
  );

export const insertUTxOsCBOR = (
  tableName: string,
  utxosCBOR: { outputReference: Uint8Array; output: Uint8Array }[],
): Effect.Effect<void, SqlError.SqlError, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`${tableName} db: attempt to insert utxos`);
    const sql = yield* SqlClient.SqlClient;

    if (utxosCBOR.length === 0) {
      yield* Effect.logDebug("No utxos provided, skipping insertion.");
      return;
    }

    const values = utxosCBOR.map((u) => ({
      tx_in_cbor: u.outputReference,
      tx_out_cbor: u.output,
    }));

    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(values)}`;

    yield* Effect.logInfo(
      `${tableName} db: ${values.length} utxos potentially inserted.`,
    );
  }).pipe(
    Effect.withLogSpan(`insertUTxOsCBOR ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: error inserting utxos: ${JSON.stringify(e)}`,
      ),
    ),
    Effect.asVoid, // Ensure the final effect is void
  );

export const retrieveUTxOsCBOR = (
  tableName: string,
): Effect.Effect<
  { outputReference: Uint8Array; output: Uint8Array }[],
  SqlError.SqlError,
  SqlClient.SqlClient
> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt retrieve all utxos`);
    const sql = yield* SqlClient.SqlClient;

    const rows = yield* sql<{
      tx_in_cbor: Uint8Array;
      tx_out_cbor: Uint8Array;
    }>`SELECT tx_in_cbor, tx_out_cbor FROM ${sql(tableName)}`;

    const result = rows.map((row) => ({
      outputReference: row.tx_in_cbor,
      output: row.tx_out_cbor,
    }));

    yield* Effect.logDebug(`${tableName} db: retrieved ${result.length} utxos`);
    return result;
  }).pipe(
    Effect.withLogSpan(`retrieveUTxOsCBOR ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieving utxos error: ${JSON.stringify(e)}`,
      ),
    ),
  );

export const clearTable = (
  tableName: string,
): Effect.Effect<void, SqlError.SqlError, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attermt to clear table`);
    const sql = yield* SqlClient.SqlClient;

    yield* sql`TRUNCATE TABLE ${sql(tableName)} CASCADE`;

    yield* Effect.logInfo(`${tableName} db: Successfully cleared table`);
  }).pipe(
    Effect.withLogSpan(`clear ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: clearing error: ${JSON.stringify(e)}`),
    ),
  );
