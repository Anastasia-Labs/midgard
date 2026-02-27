import { Data, Effect } from "effect";
import { Database } from "@/services/database.js";
import { SqlClient, SqlError } from "@effect/sql";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToUtxo, UTxO, utxoToCore } from "@lucid-evolution/lucid";

export const retrieveNumberOfEntries = (
  tableName: string,
): Effect.Effect<bigint, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to get number of entries`);
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      // sql treats COUNT(*) as a `string`, regardless of any type annotations.
      count: string;
    }>`SELECT COUNT(*) FROM ${sql(tableName)}`;
    return BigInt(rows[0].count) ?? 0;
  }).pipe(
    Effect.withLogSpan(`retrieveNumberOfEntries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieveNumberOfEntries: ${JSON.stringify(e)}`,
      ),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve row count"),
  );

export const clearTable = (
  tableName: string,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to clear table`);
    const sql = yield* SqlClient.SqlClient;

    yield* sql`TRUNCATE TABLE ${sql(tableName)} CASCADE`;

    yield* Effect.logInfo(`${tableName} db: Successfully cleared table`);
  }).pipe(
    Effect.withLogSpan(`clear ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: truncate error: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDatabaseError(tableName, "Failed at truncating table"),
  );

export class DatabaseError extends Data.TaggedError("DatabaseError")<
  SDK.GenericErrorFields & { readonly table: string }
> {}

// Use when a specific row is expected but missing. For expected empty states
// (e.g. polling queues), prefer `Option.none` over throwing this error.
export class NotFoundError extends Data.TaggedError("NotFoundError")<
  SDK.GenericErrorFields & {
    readonly table: string;
    readonly txIdHex?: string;
  }
> {}

type SqlErrorToDatabaseError = <A, R>(
  effect: Effect.Effect<
    A,
    SqlError.SqlError | DatabaseError | NotFoundError,
    R
  >,
) => Effect.Effect<A, DatabaseError, R>;

export const sqlErrorToDatabaseError = (
  tableName: string,
  message: string,
): SqlErrorToDatabaseError =>
  Effect.mapError(
    (
      error: SqlError.SqlError | DatabaseError | NotFoundError,
    ): DatabaseError =>
      error._tag === "DatabaseError"
        ? error
        : new DatabaseError({
            message,
            table: tableName,
            cause: error,
          }),
  );

/**
 * Serializes a UTxO list by converting each UTxO to core and storing its CBOR
 * bytes as hex inside a JSON array.
 */
export const serializeUTxOsForStorage = (
  utxos: readonly UTxO[],
): Effect.Effect<Buffer, SDK.CborSerializationError | SDK.CmlUnexpectedError> =>
  Effect.gen(function* () {
    const serializedEach = yield* Effect.forEach(
      utxos,
      (utxo) =>
        Effect.try({
          try: () =>
            Buffer.from(utxoToCore(utxo).to_cbor_bytes()).toString("hex"),
          catch: (e) =>
            new SDK.CmlUnexpectedError({
              message: `Failed to serialize UTxO to core CBOR`,
              cause: e,
            }),
        }),
      { concurrency: "unbounded" },
    );
    return yield* Effect.try({
      try: () => Buffer.from(JSON.stringify(serializedEach), "utf8"),
      catch: (e) =>
        new SDK.CborSerializationError({
          message: `Failed to serialize UTxO list payload`,
          cause: e,
        }),
    });
  });

/**
 * Deserializes UTxO list payload produced by `serializeUTxOsForStorage`.
 */
export const deserializeUTxOsFromStorage = (
  serialized: Buffer,
): Effect.Effect<
  readonly UTxO[],
  SDK.CborDeserializationError | SDK.CmlUnexpectedError
> =>
  Effect.gen(function* () {
    const parsed = yield* Effect.try({
      try: () => JSON.parse(serialized.toString("utf8")) as unknown,
      catch: (e) =>
        new SDK.CborDeserializationError({
          message: `Failed to deserialize UTxO list payload`,
          cause: e,
        }),
    });
    if (
      !Array.isArray(parsed) ||
      parsed.some((entry) => typeof entry !== "string")
    ) {
      return yield* Effect.fail(
        new SDK.CborDeserializationError({
          message: `Invalid UTxO list payload`,
          cause: parsed,
        }),
      );
    }
    return yield* Effect.forEach(
      parsed,
      (cborHex) =>
        Effect.try({
          try: () =>
            coreToUtxo(
              CML.TransactionUnspentOutput.from_cbor_bytes(
                Buffer.from(cborHex, "hex"),
              ),
            ),
          catch: (e) =>
            new SDK.CmlUnexpectedError({
              message: `Failed to deserialize UTxO from CBOR payload`,
              cause: e,
            }),
        }),
      { concurrency: "unbounded" },
    );
  });
