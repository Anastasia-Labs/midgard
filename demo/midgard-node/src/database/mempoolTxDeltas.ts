import {
  asArray,
  asBytes,
  decodeSingleCbor,
  encodeCbor,
} from "@al-ft/midgard-core/codec";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import {
  clearTable,
  DatabaseError,
  logDatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import * as Ledger from "@/database/utils/ledger.js";
import { Database } from "@/services/database.js";

export const tableName = "mempool_tx_deltas";

enum Columns {
  TX_ID = "tx_id",
  SPENT_CBOR = "spent_cbor",
  PRODUCED_CBOR = "produced_cbor",
}

type Entry = {
  [Columns.TX_ID]: Buffer;
  [Columns.SPENT_CBOR]: Buffer;
  [Columns.PRODUCED_CBOR]: Buffer;
};

export type TxDelta = {
  readonly txId: Buffer;
  readonly spent: readonly Buffer[];
  readonly produced: readonly Ledger.MinimalEntry[];
};

const encodeSpentCbor = (spent: readonly Buffer[]): Buffer =>
  encodeCbor(spent.map((item) => Buffer.from(item)));

const decodeSpentCbor = (bytes: Uint8Array): Buffer[] => {
  const decoded = decodeSingleCbor(bytes);
  const arr = asArray(decoded, "spent_cbor");
  return arr.map((item, index) => asBytes(item, `spent_cbor[${index}]`));
};

const encodeProducedCbor = (produced: readonly Ledger.MinimalEntry[]): Buffer =>
  encodeCbor(
    produced.map((entry) => [
      Buffer.from(entry[Ledger.Columns.OUTREF]),
      Buffer.from(entry[Ledger.Columns.OUTPUT]),
    ]),
  );

const decodeProducedCbor = (
  bytes: Uint8Array,
): readonly Ledger.MinimalEntry[] => {
  const decoded = decodeSingleCbor(bytes);
  const arr = asArray(decoded, "produced_cbor");
  return arr.map((item, index) => {
    const pair = asArray(item, `produced_cbor[${index}]`);
    if (pair.length !== 2) {
      throw new Error(`produced_cbor[${index}] must be [outref, output]`);
    }
    return {
      [Ledger.Columns.OUTREF]: asBytes(pair[0], `produced_cbor[${index}][0]`),
      [Ledger.Columns.OUTPUT]: asBytes(pair[1], `produced_cbor[${index}][1]`),
    };
  });
};

const toEntry = (delta: TxDelta): Entry => ({
  [Columns.TX_ID]: Buffer.from(delta.txId),
  [Columns.SPENT_CBOR]: encodeSpentCbor(delta.spent),
  [Columns.PRODUCED_CBOR]: encodeProducedCbor(delta.produced),
});

const fromEntry = (entry: Entry): TxDelta => ({
  txId: Buffer.from(entry[Columns.TX_ID]),
  spent: decodeSpentCbor(entry[Columns.SPENT_CBOR]),
  produced: decodeProducedCbor(entry[Columns.PRODUCED_CBOR]),
});

export const createTable: Effect.Effect<void, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
      ${sql(Columns.TX_ID)} BYTEA NOT NULL,
      ${sql(Columns.SPENT_CBOR)} BYTEA NOT NULL,
      ${sql(Columns.PRODUCED_CBOR)} BYTEA NOT NULL,
      PRIMARY KEY (${sql(Columns.TX_ID)})
    );`;
  }).pipe(
    Effect.withLogSpan(`creating table ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to create the table"),
  );

export const upsertMany = (
  deltas: readonly TxDelta[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (deltas.length === 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    const entries = deltas.map(toEntry);
    const txIds = entries.map((entry) => entry[Columns.TX_ID]);
    yield* sql`DELETE FROM ${sql(tableName)} WHERE ${sql(
      Columns.TX_ID,
    )} IN ${sql.in(txIds)}`;
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(entries)}`;
  }).pipe(
    Effect.withLogSpan(`upsertMany ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      logDatabaseError(tableName, "upsertMany", e),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to upsert tx deltas"),
  );

export const retrieveByTxIds = (
  txIds: readonly Buffer[],
): Effect.Effect<ReadonlyMap<string, TxDelta>, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (txIds.length === 0) {
      return new Map();
    }
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Entry>`SELECT ${sql(
      Columns.TX_ID,
    )}, ${sql(Columns.SPENT_CBOR)}, ${sql(Columns.PRODUCED_CBOR)}
      FROM ${sql(tableName)}
      WHERE ${sql(Columns.TX_ID)} IN ${sql.in(txIds)}`;

    const decodedRows = yield* Effect.try({
      try: () => rows.map(fromEntry),
      catch: (cause) =>
        new DatabaseError({
          message: "Failed to decode tx deltas",
          table: tableName,
          cause,
        }),
    });

    return new Map(
      decodedRows.map((decoded) => [decoded.txId.toString("hex"), decoded]),
    );
  }).pipe(
    Effect.withLogSpan(`retrieveByTxIds ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      logDatabaseError(tableName, "retrieveByTxIds", e),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve tx deltas"),
  );

export const clearTxs = (
  txIds: readonly Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (txIds.length === 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM ${sql(tableName)} WHERE ${sql(
      Columns.TX_ID,
    )} IN ${sql.in(txIds)}`;
  }).pipe(
    Effect.withLogSpan(`clearTxs ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      logDatabaseError(tableName, "clearTxs", e),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to clear tx deltas"),
  );

export const clear = clearTable(tableName);
