import { Database } from "@/services/database.js";
import { Effect, Option } from "effect";
import { SqlClient } from "@effect/sql";
import * as SDK from "@al-ft/midgard-sdk";
import {
  clearTable,
  deserializeUTxOsFromStorage,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { UTxO } from "@lucid-evolution/lucid";
import {
  BlocksTxsDB,
  DepositsDB,
  ImmutableDB,
  MempoolDB,
  Tx,
  TxOrdersDB,
  UserEvents,
  WithdrawalsDB,
} from "./index.js";
import { AlwaysSucceedsContract } from "@/services/always-succeeds.js";

export const tableName = "unsubmitted_blocks";

export enum Columns {
  HEIGHT = "height",
  TIMESTAMPTZ = "time_stamp_tz",
  HEADER_HASH = "header_hash",
  EVENT_START_TIME = "event_start_time",
  EVENT_END_TIME = "event_end_time",
  NEW_WALLET_UTXOS = "new_wallet_utxos",
  PRODUCED_UTXOS = "produced_utxos",
  L1_CBOR = "l1_cbor",
  STATUS = "status",
  DEPOSITS_COUNT = "deposits_count",
  TX_REQUESTS_COUNT = "tx_requests_count",
  TX_ORDERS_COUNT = "tx_orders_count",
  WITHDRAWALS_COUNT = "withdrawals_count",
  TOTAL_EVENTS_SIZE = "total_events_size",
}

export type Stats = {
  [Columns.DEPOSITS_COUNT]: number;
  [Columns.TX_REQUESTS_COUNT]: number;
  [Columns.TX_ORDERS_COUNT]: number;
  [Columns.WITHDRAWALS_COUNT]: number;
  [Columns.TOTAL_EVENTS_SIZE]: number;
};

export type EntryNoMeta = Stats & {
  [Columns.HEADER_HASH]: Buffer;
  [Columns.EVENT_START_TIME]: Date;
  [Columns.EVENT_END_TIME]: Date;
  // Corresponds to `.chain()` first tuple value.
  [Columns.NEW_WALLET_UTXOS]: Buffer;
  // Corresponds to `.chain()` second tuple value.
  [Columns.PRODUCED_UTXOS]: Buffer;
  // Corresponds to `.chain()` third tuple value.
  [Columns.L1_CBOR]: Buffer;
  [Columns.STATUS]: Status;
};

export enum Status {
  UNSUBMITTED = 0,
  SUBMITTED = 1,
  CONFIRMED = 2,
  MERGED = 3,
}

export type Entry = EntryNoMeta & {
  [Columns.HEIGHT]: bigint;
  [Columns.TIMESTAMPTZ]: Date;
};

export type Events = {
  withdrawals: readonly UserEvents.Entry[];
  txOrders: readonly UserEvents.Entry[];
  txRequests: readonly Tx.Entry[];
  deposits: readonly UserEvents.Entry[];
};

export type LatestUnsubmittedBlockWithTxs = Omit<
  Entry,
  Columns.NEW_WALLET_UTXOS | Columns.PRODUCED_UTXOS
> & {
  [Columns.NEW_WALLET_UTXOS]: readonly UTxO[];
  [Columns.PRODUCED_UTXOS]: readonly UTxO[];
  txHashes: readonly Buffer[];
  txCbors: readonly Buffer[];
};

type LatestUnsubmittedBlockJoinRow = Entry & {
  [BlocksTxsDB.Columns.TX_ID]: Buffer | null;
  [Tx.Columns.TX]: Buffer | null;
};

export const createTable: Effect.Effect<void, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
      ${sql(Columns.HEIGHT)} BIGSERIAL PRIMARY KEY,
      ${sql(Columns.HEADER_HASH)} BYTEA NOT NULL UNIQUE,
      ${sql(Columns.EVENT_START_TIME)} TIMESTAMPTZ NOT NULL,
      ${sql(Columns.EVENT_END_TIME)} TIMESTAMPTZ NOT NULL,
      ${sql(Columns.NEW_WALLET_UTXOS)} BYTEA NOT NULL,
      ${sql(Columns.L1_CBOR)} BYTEA NOT NULL,
      ${sql(Columns.PRODUCED_UTXOS)} BYTEA NOT NULL,
      ${sql(Columns.DEPOSITS_COUNT)} INTEGER NOT NULL,
      ${sql(Columns.TX_REQUESTS_COUNT)} INTEGER NOT NULL,
      ${sql(Columns.TX_ORDERS_COUNT)} INTEGER NOT NULL,
      ${sql(Columns.WITHDRAWALS_COUNT)} INTEGER NOT NULL,
      ${sql(Columns.TOTAL_EVENTS_SIZE)} INTEGER NOT NULL,
      ${sql(Columns.STATUS)} INTEGER NOT NULL DEFAULT(${Status.UNSUBMITTED}),
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
        ${sql(Columns.EVENT_START_TIME)} = ${entry[Columns.EVENT_START_TIME]},
        ${sql(Columns.EVENT_END_TIME)} = ${entry[Columns.EVENT_END_TIME]},
        ${sql(Columns.NEW_WALLET_UTXOS)} = ${entry[Columns.NEW_WALLET_UTXOS]},
        ${sql(Columns.L1_CBOR)} = ${entry[Columns.L1_CBOR]},
        ${sql(Columns.PRODUCED_UTXOS)} = ${entry[Columns.PRODUCED_UTXOS]},
        ${sql(Columns.DEPOSITS_COUNT)} = ${entry[Columns.DEPOSITS_COUNT]},
        ${sql(Columns.TX_REQUESTS_COUNT)} = ${entry[Columns.TX_REQUESTS_COUNT]},
        ${sql(Columns.TX_ORDERS_COUNT)} = ${entry[Columns.TX_ORDERS_COUNT]},
        ${sql(Columns.WITHDRAWALS_COUNT)} = ${entry[Columns.WITHDRAWALS_COUNT]},
        ${sql(Columns.TOTAL_EVENTS_SIZE)} = ${entry[Columns.TOTAL_EVENTS_SIZE]},
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
    Columns.HEIGHT,
  )} ASC`;
}).pipe(
  Effect.withLogSpan(`retrieve ${tableName}`),
  sqlErrorToDatabaseError(tableName, "Failed to retrieve unsubmitted blocks"),
);

export const retrieveEvents = (
  startDate: Date,
  endDate: Date,
): Effect.Effect<Events, DatabaseError, Database> =>
  Effect.gen(function* () {
    const [withdrawals, txOrders, txRequests, deposits] = yield* Effect.all(
      [
        WithdrawalsDB.retrieveTimeBoundEntries(startDate, endDate),
        TxOrdersDB.retrieveTimeBoundEntries(startDate, endDate),
        MempoolDB.retrieveTimeBoundEntries(startDate, endDate),
        DepositsDB.retrieveTimeBoundEntries(startDate, endDate),
      ],
      { concurrency: "unbounded" },
    );
    return {
      withdrawals,
      txOrders,
      txRequests,
      deposits,
    };
  });

export const retrieveEarliestUnsubmittedEntry: Effect.Effect<
  Option.Option<Entry>,
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<Entry>`
    SELECT * FROM ${sql(tableName)}
    WHERE ${sql(Columns.STATUS)} = ${Status.UNSUBMITTED}
    ORDER BY ${sql(Columns.HEIGHT)} ASC`;
  if (rows.length <= 0) {
    return Option.none();
  } else {
    return Option.some(rows[0]);
  }
}).pipe(sqlErrorToDatabaseError(tableName, "retrieveEarliestUnsubmittedEntry"));

export const retrieveLatestEntry: Effect.Effect<
  Option.Option<Entry>,
  DatabaseError,
  Database
> = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<LatestUnsubmittedBlockJoinRow>`
    SELECT * FROM ${sql(tableName)} ORDER BY ${sql(Columns.HEIGHT)} DESC`;
  if (rows.length <= 0) {
    return Option.none();
  } else {
    return Option.some(rows[0]);
  }
}).pipe(
  sqlErrorToDatabaseError(
    tableName,
    "Failed retrieve latest unsubmitted block",
  ),
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
      SELECT
        ${sql(Columns.HEIGHT)},
        ${sql(Columns.HEADER_HASH)},
        ${sql(Columns.EVENT_START_TIME)},
        ${sql(Columns.EVENT_END_TIME)},
        ${sql(Columns.NEW_WALLET_UTXOS)},
        ${sql(Columns.PRODUCED_UTXOS)},
        ${sql(Columns.L1_CBOR)},
        ${sql(Columns.DEPOSITS_COUNT)},
        ${sql(Columns.TX_REQUESTS_COUNT)},
        ${sql(Columns.TX_ORDERS_COUNT)},
        ${sql(Columns.WITHDRAWALS_COUNT)},
        ${sql(Columns.TOTAL_EVENTS_SIZE)},
        ${sql(Columns.TIMESTAMPTZ)},
      FROM ${sql(tableName)}
      ORDER BY ${sql(Columns.HEIGHT)} DESC
      LIMIT 1
    )
    SELECT
      latest_unsubmitted.${sql(Columns.HEIGHT)},
      latest_unsubmitted.${sql(Columns.HEADER_HASH)},
      latest_unsubmitted.${sql(Columns.EVENT_START_TIME)},
      latest_unsubmitted.${sql(Columns.EVENT_END_TIME)},
      latest_unsubmitted.${sql(Columns.NEW_WALLET_UTXOS)},
      latest_unsubmitted.${sql(Columns.PRODUCED_UTXOS)},
      latest_unsubmitted.${sql(Columns.L1_CBOR)},
      latest_unsubmitted.${sql(Columns.DEPOSITS_COUNT)},
      latest_unsubmitted.${sql(Columns.TX_REQUESTS_COUNT)},
      latest_unsubmitted.${sql(Columns.TX_ORDERS_COUNT)},
      latest_unsubmitted.${sql(Columns.WITHDRAWALS_COUNT)},
      latest_unsubmitted.${sql(Columns.TOTAL_EVENTS_SIZE)},
      latest_unsubmitted.${sql(Columns.TIMESTAMPTZ)},
      b.${sql(BlocksTxsDB.Columns.TX_ID)},
      i.${sql(Tx.Columns.TX)}
    FROM latest_unsubmitted
    LEFT JOIN ${sql(BlocksTxsDB.tableName)} AS b
      ON b.${sql(BlocksTxsDB.Columns.HEADER_HASH)} = latest_unsubmitted.${sql(Columns.HEADER_HASH)}
    LEFT JOIN ${sql(ImmutableDB.tableName)} AS i
      ON i.${sql(Tx.Columns.TX_ID)} = b.${sql(BlocksTxsDB.Columns.TX_ID)}
    ORDER BY b.${sql(BlocksTxsDB.Columns.HEIGHT)} ASC NULLS LAST
  `;
  if (rows.length <= 0) {
    return Option.none();
  }

  const firstRow = rows[0];
  const newWalletUtxos = yield* deserializeUTxOsFromStorage(
    firstRow[Columns.NEW_WALLET_UTXOS],
  );
  const producedUtxos = yield* deserializeUTxOsFromStorage(
    firstRow[Columns.PRODUCED_UTXOS],
  );
  const initialTxAcc: { txHashes: Buffer[]; txCbors: Buffer[] } = {
    txHashes: [],
    txCbors: [],
  };
  const { txHashes, txCbors } = yield* Effect.reduce(
    rows,
    initialTxAcc,
    (acc, row) => {
      if (
        row[BlocksTxsDB.Columns.TX_ID] === null &&
        row[Tx.Columns.TX] === null
      ) {
        return Effect.succeed(acc);
      }
      if (
        row[BlocksTxsDB.Columns.TX_ID] === null ||
        row[Tx.Columns.TX] === null
      ) {
        return Effect.fail(
          new DatabaseError({
            message:
              "Inconsistent transaction row for latest unsubmitted block",
            cause: row,
            table: tableName,
          }),
        );
      }
      acc.txHashes.push(row[BlocksTxsDB.Columns.TX_ID]);
      acc.txCbors.push(row[Tx.Columns.TX]);
      return Effect.succeed(acc);
    },
  );

  return Option.some({
    ...firstRow,
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

/**
 * A `BlocksDB` entry contains the list of produced UTxOs from its signed
 * transaction. Since this transaciton is a linked list append operation, one of
 * the elements is the appended node (i.e. block header).
 *
 * TODO: This function attempts conversion on all the produced UTxOs and returns
 *       the one that succeeds. However, if we guarantee the position of the
 *       appended block in the UTxO array, we can have a more optimized function
 *       here.
 */
export const getAppendedStateQueueUTxOFromEntry = (
  entry: Entry,
): Effect.Effect<
  SDK.StateQueueUTxO,
  SDK.CborDeserializationError | SDK.CmlUnexpectedError | SDK.StateQueueError,
  AlwaysSucceedsContract
> =>
  Effect.gen(function* () {
    const { stateQueue } = yield* AlwaysSucceedsContract;
    const producedUTxOs = yield* deserializeUTxOsFromStorage(
      entry[Columns.PRODUCED_UTXOS],
    );
    // Keeps only UTxOs that successfully map to a StateQueueUTxO with no links
    // (i.e. last element in the linked list).
    const foundMatches: SDK.StateQueueUTxO[] = yield* Effect.allSuccesses(
      producedUTxOs.map((utxo) =>
        Effect.gen(function* () {
          const stateQueueUTxO = yield* SDK.utxoToStateQueueUTxO(
            utxo,
            stateQueue.policyId,
          );
          if (stateQueueUTxO.datum.next === "Empty") {
            const headerHash =
              yield* SDK.headerHashFromStateQueueUTxO(stateQueueUTxO);
            if (SDK.bufferToHex(entry[Columns.HEADER_HASH]) === headerHash) {
              return stateQueueUTxO;
            } else {
              // Doesn't matter what error is raised here (or below) as errors are
              // ignored in `allSuccesses`.
              return yield* new SDK.StateQueueError({ message: "", cause: "" });
            }
          } else {
            return yield* new SDK.StateQueueError({ message: "", cause: "" });
          }
        }),
      ),
    );
    if (foundMatches.length === 1) {
      return foundMatches[0];
    } else {
      return yield* new SDK.StateQueueError({
        message:
          "Failed to get the appended StateQueueUTxO from the given BlocksDB entry",
        cause:
          "The list of produced UTxOs stored in the entry did not contain exactly one UTxO such that it could be mapped to a StateQueueUTxO with no links",
      });
    }
  });

export const setStatusOfEntry = (
  entry: Entry,
  newStatus: Status,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE ${sql(tableName)} SET ${sql(Columns.STATUS)} = ${newStatus} WHERE
    ${sql(Columns.HEADER_HASH)} = ${entry[Columns.HEADER_HASH]}
  `;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to update status of given block entry",
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
    yield* sql`WITH target AS (
      SELECT ${sql(Columns.HEIGHT)}
      FROM ${sql(tableName)}
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
      LIMIT 1
    )
    DELETE FROM ${sql(tableName)}
    WHERE ${sql(Columns.HEIGHT)} <= (
      SELECT ${sql(Columns.HEIGHT)} FROM target
    )`;
  }).pipe(
    Effect.withLogSpan(`deleteUpToAndIncludingBlock ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to delete unsubmitted blocks up to the given block",
    ),
  );

export const clear = clearTable(tableName);
