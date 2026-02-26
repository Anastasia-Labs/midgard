import { Database } from "@/services/database.js";
import { Effect } from "effect";
import { SqlClient } from "@effect/sql";
import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, coreToUtxo, UTxO, utxoToCore } from "@lucid-evolution/lucid";

export const tableName = "unsubmitted_blocks";

export enum Columns {
  SEQUENCE = "sequence",
  BLOCK = "block",
  NEW_WALLET_UTXOS = "new_wallet_utxos",
  L1_CBOR = "l1_cbor",
  // Corresponds to `.chain()` second tuple value (`derivedOutputs`).
  PRODUCED_UTXOS = "produced_utxos",
  TIMESTAMPTZ = "time_stamp_tz",
}

export type EntryNoMeta = {
  [Columns.BLOCK]: Buffer;
  // Corresponds to `.chain()` first tuple value (`newWalletUTxOs`).
  [Columns.NEW_WALLET_UTXOS]: Buffer;
  // Corresponds to `.chain()` third tuple value (transaction CBOR).
  [Columns.L1_CBOR]: Buffer;
  // Corresponds to `.chain()` second tuple value (`derivedOutputs`).
  [Columns.PRODUCED_UTXOS]: Buffer;
};

export type Entry = EntryNoMeta & {
  [Columns.SEQUENCE]: bigint;
  [Columns.TIMESTAMPTZ]: Date;
};

export const createTable: Effect.Effect<void, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
      ${sql(Columns.SEQUENCE)} BIGSERIAL PRIMARY KEY,
      ${sql(Columns.BLOCK)} BYTEA NOT NULL UNIQUE,
      ${sql(Columns.NEW_WALLET_UTXOS)} BYTEA NOT NULL,
      ${sql(Columns.L1_CBOR)} BYTEA NOT NULL,
      ${sql(Columns.PRODUCED_UTXOS)} BYTEA NOT NULL,
      ${sql(Columns.TIMESTAMPTZ)} TIMESTAMPTZ NOT NULL DEFAULT(NOW())
    );`;
  }).pipe(
    Effect.withLogSpan(`creating table ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to create the table"),
  );

export const upsert = (
  entry: EntryNoMeta,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(entry)}
      ON CONFLICT (${sql(Columns.BLOCK)}) DO UPDATE SET
        ${sql(Columns.NEW_WALLET_UTXOS)} = ${entry[Columns.NEW_WALLET_UTXOS]},
        ${sql(Columns.L1_CBOR)} = ${entry[Columns.L1_CBOR]},
        ${sql(Columns.PRODUCED_UTXOS)} = ${entry[Columns.PRODUCED_UTXOS]},
        ${sql(Columns.TIMESTAMPTZ)} = NOW()`;
  }).pipe(
    Effect.withLogSpan(`upsert ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to upsert the given unsubmitted block",
    ),
  );

export const retrieve: Effect.Effect<
  readonly Entry[],
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  return yield* sql<Entry>`SELECT * FROM ${sql(tableName)} ORDER BY ${sql(
    Columns.SEQUENCE,
  )} ASC`;
}).pipe(
  Effect.withLogSpan(`retrieve ${tableName}`),
  sqlErrorToDatabaseError(tableName, "Failed to retrieve unsubmitted blocks"),
);

export const deleteByBlocks = (
  blocks: Buffer[] | readonly Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (blocks.length <= 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM ${sql(tableName)} WHERE ${sql.in(
      Columns.BLOCK,
      blocks,
    )}`;
  }).pipe(
    Effect.withLogSpan(`deleteByBlocks ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to remove unsubmitted blocks by header hash",
    ),
  );

export const deleteUpToAndIncludingBlock = (
  block: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`WITH ${sql("target")} AS (
      SELECT ${sql(Columns.SEQUENCE)}
      FROM ${sql(tableName)}
      WHERE ${sql(Columns.BLOCK)} = ${block}
      LIMIT 1
    )
    DELETE FROM ${sql(tableName)}
    WHERE ${sql(Columns.SEQUENCE)} <= (
      SELECT ${sql(Columns.SEQUENCE)} FROM ${sql("target")}
    )`;
  }).pipe(
    Effect.withLogSpan(`deleteUpToAndIncludingBlock ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to delete unsubmitted blocks up to the given block",
    ),
  );

export const clear = clearTable(tableName);

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
