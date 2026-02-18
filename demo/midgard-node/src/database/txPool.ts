import { Database } from "@/services/database.js";
import { SqlClient } from "@effect/sql";
import { Data, Effect } from "effect";
import {
  clearTable,
  DatabaseError,
  NotFoundError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";

export const tableName = "tx_pool";

export enum Columns {
  TX_ID = "tx_id",
  TX = "tx",
  STATUS = "status",
  REJECT_CODE = "reject_code",
  REJECT_DETAIL = "reject_detail",
  ARRIVAL_SEQ = "arrival_seq",
  CREATED_AT = "created_at",
  UPDATED_AT = "updated_at",
}

export const statusLiterals = [
  "queued",
  "accepted",
  "rejected",
  "committed",
] as const;

export type Status = (typeof statusLiterals)[number];

export type Entry = {
  [Columns.TX_ID]: Buffer;
  [Columns.TX]: Buffer;
  [Columns.STATUS]: Status;
  [Columns.REJECT_CODE]: string | null;
  [Columns.REJECT_DETAIL]: string | null;
  [Columns.ARRIVAL_SEQ]: bigint;
  [Columns.CREATED_AT]: Date;
  [Columns.UPDATED_AT]: Date;
};

export type StatusView = Pick<
  Entry,
  | Columns.TX_ID
  | Columns.STATUS
  | Columns.REJECT_CODE
  | Columns.REJECT_DETAIL
  | Columns.CREATED_AT
  | Columns.UPDATED_AT
>;

export type QueuedEntry = Pick<
  Entry,
  Columns.TX_ID | Columns.TX | Columns.ARRIVAL_SEQ | Columns.CREATED_AT
>;

export class TxPoolStatusError extends Data.TaggedError(
  "TxPoolStatusError",
)<{
  readonly status: string;
}> {}

const ensureKnownStatus = (status: string): status is Status =>
  statusLiterals.includes(status as Status);

export const createTable: Effect.Effect<void, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
          ${sql(Columns.TX_ID)} BYTEA NOT NULL PRIMARY KEY,
          ${sql(Columns.TX)} BYTEA NOT NULL,
          ${sql(Columns.STATUS)} TEXT NOT NULL,
          ${sql(Columns.REJECT_CODE)} TEXT,
          ${sql(Columns.REJECT_DETAIL)} TEXT,
          ${sql(Columns.ARRIVAL_SEQ)} BIGSERIAL NOT NULL,
          ${sql(Columns.CREATED_AT)} TIMESTAMPTZ NOT NULL DEFAULT NOW(),
          ${sql(Columns.UPDATED_AT)} TIMESTAMPTZ NOT NULL DEFAULT NOW()
        );`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.STATUS}_${Columns.ARRIVAL_SEQ}`,
        )} ON ${sql(tableName)} (${sql(Columns.STATUS)}, ${sql(
          Columns.ARRIVAL_SEQ,
        )});`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.UPDATED_AT}`,
        )} ON ${sql(tableName)} (${sql(Columns.UPDATED_AT)});`;
      }),
    );
  }).pipe(
    Effect.withLogSpan(`creating table ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to create tx_pool table"),
  );

export const insertQueued = (
  txId: Buffer,
  txCbor: Buffer,
): Effect.Effect<StatusView, DatabaseError | TxPoolStatusError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const inserted = yield* sql<StatusView>`INSERT INTO ${sql(
      tableName,
    )} (${sql(Columns.TX_ID)}, ${sql(Columns.TX)}, ${sql(Columns.STATUS)})
      VALUES (${txId}, ${txCbor}, ${"queued"})
      ON CONFLICT (${sql(Columns.TX_ID)}) DO NOTHING
      RETURNING ${sql(Columns.TX_ID)},
        ${sql(Columns.STATUS)},
        ${sql(Columns.REJECT_CODE)},
        ${sql(Columns.REJECT_DETAIL)},
        ${sql(Columns.CREATED_AT)},
        ${sql(Columns.UPDATED_AT)}`;
    if (inserted.length > 0) {
      return inserted[0];
    }

    const found = yield* sql<StatusView>`SELECT ${sql(
      Columns.TX_ID,
    )}, ${sql(Columns.STATUS)}, ${sql(Columns.REJECT_CODE)}, ${sql(
      Columns.REJECT_DETAIL,
    )}, ${sql(Columns.CREATED_AT)}, ${sql(Columns.UPDATED_AT)}
    FROM ${sql(tableName)}
    WHERE ${sql(Columns.TX_ID)} = ${txId}
    LIMIT 1`;
    if (found.length === 0) {
      yield* Effect.fail(
        new DatabaseError({
          message: "tx_pool row disappeared after insert conflict",
          table: tableName,
          cause: "No row found after conflicting insert",
        }),
      );
    }
    const row = found[0];
    if (!ensureKnownStatus(row.status)) {
      yield* Effect.fail(new TxPoolStatusError({ status: row.status }));
    }
    return row;
  }).pipe(
    Effect.withLogSpan(`insert queued ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to insert queued tx"),
  );

export const retrieveStatusByTxId = (
  txId: Buffer,
): Effect.Effect<
  StatusView,
  DatabaseError | NotFoundError | TxPoolStatusError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const result = yield* sql<StatusView>`SELECT ${sql(
      Columns.TX_ID,
    )}, ${sql(Columns.STATUS)}, ${sql(Columns.REJECT_CODE)}, ${sql(
      Columns.REJECT_DETAIL,
    )}, ${sql(Columns.CREATED_AT)}, ${sql(Columns.UPDATED_AT)}
      FROM ${sql(tableName)}
      WHERE ${sql(Columns.TX_ID)} = ${txId}
      LIMIT 1`;
    if (result.length === 0) {
      yield* new NotFoundError({
        message: "No tx_pool status row found for tx_id",
        cause: `No status row found for tx_id ${txId.toString("hex")}`,
        table: tableName,
        txIdHex: txId.toString("hex"),
      });
    }
    const row = result[0];
    if (!ensureKnownStatus(row.status)) {
      yield* Effect.fail(new TxPoolStatusError({ status: row.status }));
    }
    return row;
  }).pipe(
    Effect.withLogSpan(`retrieve status ${tableName}`),
    Effect.mapError((error): DatabaseError | NotFoundError | TxPoolStatusError =>
      error._tag === "SqlError"
        ? new DatabaseError({
            message: "Failed to retrieve tx status",
            table: tableName,
            cause: error,
          })
        : error,
    ),
  );

export const retrieveQueued = (
  limit: number,
): Effect.Effect<readonly QueuedEntry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<QueuedEntry>`SELECT ${sql(Columns.TX_ID)},
      ${sql(Columns.TX)},
      ${sql(Columns.ARRIVAL_SEQ)},
      ${sql(Columns.CREATED_AT)}
      FROM ${sql(tableName)}
      WHERE ${sql(Columns.STATUS)} = ${"queued"}
      ORDER BY ${sql(Columns.ARRIVAL_SEQ)} ASC
      LIMIT ${limit}`;
  }).pipe(
    Effect.withLogSpan(`retrieve queued ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve queued txs"),
  );

export const updateToAccepted = (
  txIds: readonly Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (txIds.length === 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${"accepted"},
          ${sql(Columns.REJECT_CODE)} = NULL,
          ${sql(Columns.REJECT_DETAIL)} = NULL,
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.TX_ID)} IN ${sql.in(txIds)}
      AND ${sql(Columns.STATUS)} = ${"queued"}`;
  }).pipe(
    Effect.withLogSpan(`mark accepted ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to mark txs as accepted"),
  );

export const updateRejected = (
  txId: Buffer,
  rejectCode: string,
  rejectDetail: string | null,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${"rejected"},
          ${sql(Columns.REJECT_CODE)} = ${rejectCode},
          ${sql(Columns.REJECT_DETAIL)} = ${rejectDetail},
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.TX_ID)} = ${txId}
      AND ${sql(Columns.STATUS)} = ${"queued"}`;
  }).pipe(
    Effect.withLogSpan(`mark rejected ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to mark tx as rejected"),
  );

export const updateCommitted = (
  txIds: readonly Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (txIds.length === 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${"committed"},
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.TX_ID)} IN ${sql.in(txIds)}
      AND ${sql(Columns.STATUS)} IN (${`accepted`}, ${`committed`})`;
  }).pipe(
    Effect.withLogSpan(`mark committed ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to mark txs as committed"),
  );

export const retrieveCountByStatus = (
  status: Status,
): Effect.Effect<bigint, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ count: string }>`SELECT COUNT(*) FROM ${sql(
      tableName,
    )} WHERE ${sql(Columns.STATUS)} = ${status}`;
    return BigInt(rows[0].count) ?? 0n;
  }).pipe(
    Effect.withLogSpan(`count by status ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to count tx_pool rows by status"),
  );

export const clear = clearTable(tableName);
