import { Database } from "@/services/database.js";
import { Effect, Option } from "effect";
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
  HEADER_HASH = "header_hash",
  NEW_WALLET_UTXOS = "new_wallet_utxos",
  // Corresponds to `.chain()` second tuple value (`derivedOutputs`).
  PRODUCED_UTXOS = "produced_utxos",
  L1_CBOR = "l1_cbor",
  TIMESTAMPTZ = "time_stamp_tz",
}

export type EntryNoMeta = {
  [Columns.HEADER_HASH]: Buffer;
  // Corresponds to `.chain()` first tuple value (`newWalletUTxOs`).
  [Columns.NEW_WALLET_UTXOS]: Buffer;
  // Corresponds to `.chain()` second tuple value (`derivedOutputs`).
  [Columns.PRODUCED_UTXOS]: Buffer;
  // Corresponds to `.chain()` third tuple value (transaction CBOR).
  [Columns.L1_CBOR]: Buffer;
};

export type Entry = EntryNoMeta & {
  [Columns.SEQUENCE]: bigint;
  [Columns.TIMESTAMPTZ]: Date;
};

export type LatestUnsubmittedBlockWithTxs = {
  [Columns.HEADER_HASH]: Buffer;
  [Columns.NEW_WALLET_UTXOS]: readonly UTxO[];
  [Columns.PRODUCED_UTXOS]: readonly UTxO[];
  txHashes: readonly Buffer[];
  txCbors: readonly Buffer[];
};

type LatestUnsubmittedBlockJoinRow = {
  header_hash: Buffer;
  new_wallet_utxos: Buffer;
  produced_utxos: Buffer;
  tx_id: Buffer | null;
  tx: Buffer | null;
};

export const createTable: Effect.Effect<void, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
      ${sql(Columns.SEQUENCE)} BIGSERIAL PRIMARY KEY,
      ${sql(Columns.HEADER_HASH)} BYTEA NOT NULL UNIQUE,
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
      ON CONFLICT (${sql(Columns.HEADER_HASH)}) DO UPDATE SET
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

/**
 * Retrieves the latest unsubmitted block and all corresponding transaction
 * hashes and CBORs via a single SQL call.
 */
export const retrieveLatestWithBlockTxs: Effect.Effect<
  Option.Option<LatestUnsubmittedBlockWithTxs>,
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<LatestUnsubmittedBlockJoinRow>`
    WITH latest_unsubmitted AS (
      SELECT header_hash, new_wallet_utxos, produced_utxos
      FROM unsubmitted_blocks
      ORDER BY sequence DESC
      LIMIT 1
    )
    SELECT
      latest_unsubmitted.header_hash,
      latest_unsubmitted.new_wallet_utxos,
      latest_unsubmitted.produced_utxos,
      blocks.tx_id,
      immutable.tx
    FROM latest_unsubmitted
    LEFT JOIN blocks
      ON blocks.header_hash = latest_unsubmitted.header_hash
    LEFT JOIN immutable
      ON immutable.tx_id = blocks.tx_id
    ORDER BY blocks.height ASC NULLS LAST
  `;
  if (rows.length <= 0) {
    return Option.none();
  }

  const firstRow = rows[0];
  const newWalletUtxos = yield* deserializeUTxOsFromStorage(
    firstRow.new_wallet_utxos,
  );
  const producedUtxos = yield* deserializeUTxOsFromStorage(
    firstRow.produced_utxos,
  );
  const txHashes: Buffer[] = [];
  const txCbors: Buffer[] = [];
  for (const row of rows) {
    if (row.tx_id === null && row.tx === null) {
      continue;
    }
    if (row.tx_id === null || row.tx === null) {
      return yield* Effect.fail(
        new DatabaseError({
          message: "Inconsistent transaction row for latest unsubmitted block",
          cause: row,
          table: tableName,
        }),
      );
    }
    txHashes.push(row.tx_id);
    txCbors.push(row.tx);
  }

  return Option.some({
    [Columns.HEADER_HASH]: firstRow.header_hash,
    [Columns.NEW_WALLET_UTXOS]: newWalletUtxos,
    [Columns.PRODUCED_UTXOS]: producedUtxos,
    txHashes,
    txCbors,
  });
}).pipe(
  Effect.withLogSpan(`retrieveLatestWithBlockTxs ${tableName}`),
  Effect.catchTags({
    CborDeserializationError: (error) =>
      Effect.fail(
        new DatabaseError({
          message:
            "Failed to retrieve latest unsubmitted block with transactions",
          cause: error,
          table: tableName,
        }),
      ),
    CmlUnexpectedError: (error) =>
      Effect.fail(
        new DatabaseError({
          message:
            "Failed to retrieve latest unsubmitted block with transactions",
          cause: error,
          table: tableName,
        }),
      ),
  }),
  sqlErrorToDatabaseError(
    tableName,
    "Failed to retrieve latest unsubmitted block with transactions",
  ),
);

export const deleteByBlocks = (
  headerHashes: Buffer[] | readonly Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (headerHashes.length <= 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM ${sql(tableName)} WHERE ${sql.in(
      Columns.HEADER_HASH,
      headerHashes,
    )}`;
  }).pipe(
    Effect.withLogSpan(`deleteByBlocks ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to remove unsubmitted blocks by header hash",
    ),
  );

export const deleteUpToAndIncludingBlock = (
  headerHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`WITH ${sql("target")} AS (
      SELECT ${sql(Columns.SEQUENCE)}
      FROM ${sql(tableName)}
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
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
