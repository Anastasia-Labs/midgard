import {
  asArray,
  asBytes,
  decodeSingleCbor,
  encodeCborArrayRaw,
  encodeCborBytes,
} from "@al-ft/midgard-core/codec";
import { SqlClient } from "@effect/sql";
import { Duration, Effect, Metric } from "effect";

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

export const mempoolTxDeltasPreparationDurationTimer = Metric.timer(
  "mempool_tx_deltas_preparation_duration",
  "Duration of deterministic tx-delta row preparation and CBOR encoding",
);

export const mempoolTxDeltasSqlDurationTimer = Metric.timer(
  "mempool_tx_deltas_sql_duration",
  "Duration of the tx-delta upsert SQL statement",
);

const encodeSpentCbor = (spent: readonly Buffer[]): Buffer =>
  encodeCborArrayRaw(spent.map(encodeCborBytes));

const decodeSpentCbor = (bytes: Uint8Array): Buffer[] => {
  const decoded = decodeSingleCbor(bytes);
  const arr = asArray(decoded, "spent_cbor");
  return arr.map((item, index) => asBytes(item, `spent_cbor[${index}]`));
};

const encodeProducedCbor = (produced: readonly Ledger.MinimalEntry[]): Buffer =>
  encodeCborArrayRaw(
    produced
      .map((entry) => [
        encodeCborBytes(entry[Ledger.Columns.OUTREF]),
        encodeCborBytes(entry[Ledger.Columns.OUTPUT]),
      ])
      .map(encodeCborArrayRaw),
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
    yield* sql`CREATE UNLOGGED TABLE IF NOT EXISTS ${sql(tableName)} (
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
    const preparationStartedAt = performance.now();
    const entries = deltas.map(toEntry);
    yield* mempoolTxDeltasPreparationDurationTimer(
      Effect.succeed(Duration.millis(performance.now() - preparationStartedAt)),
    );
    const sqlStartedAt = performance.now();
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(entries)}
      ON CONFLICT (${sql(Columns.TX_ID)}) DO UPDATE SET
        ${sql(Columns.SPENT_CBOR)} = EXCLUDED.${sql(Columns.SPENT_CBOR)},
        ${sql(Columns.PRODUCED_CBOR)} = EXCLUDED.${sql(Columns.PRODUCED_CBOR)}`;
    yield* mempoolTxDeltasSqlDurationTimer(
      Effect.succeed(Duration.millis(performance.now() - sqlStartedAt)),
    );
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

export const deleteOrphans: Effect.Effect<number, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const deleted = yield* sql<Pick<Entry, Columns.TX_ID>>`
      DELETE FROM ${sql(tableName)} AS delta
      WHERE NOT EXISTS (
        SELECT 1
        FROM ${sql("mempool")} AS mempool_tx
        WHERE mempool_tx.${sql(Columns.TX_ID)} = delta.${sql(Columns.TX_ID)}
      )
      RETURNING delta.${sql(Columns.TX_ID)}`;
    return deleted.length;
  }).pipe(
    Effect.withLogSpan(`deleteOrphans ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to delete orphan tx deltas"),
  );

export const clear = clearTable(tableName);
