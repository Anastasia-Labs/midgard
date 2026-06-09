import { Database } from "@/services/database.js";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";
import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";

export const tableName = "pending_block_finalizations";
const depositsTableName = "pending_block_finalization_deposits";
const txsTableName = "pending_block_finalization_txs";

export enum Columns {
  HEADER_HASH = "header_hash",
  SUBMITTED_TX_HASH = "submitted_tx_hash",
  BLOCK_END_TIME = "block_end_time",
  STATUS = "status",
  OBSERVED_CONFIRMED_AT_MS = "observed_confirmed_at_ms",
  CREATED_AT = "created_at",
  UPDATED_AT = "updated_at",
}

enum MemberColumns {
  HEADER_HASH = "header_hash",
  MEMBER_ID = "member_id",
  ORDINAL = "ordinal",
}

export const Status = {
  PendingSubmission: "pending_submission",
  SubmittedLocalFinalizationPending: "submitted_local_finalization_pending",
  SubmittedUnconfirmed: "submitted_unconfirmed",
  ObservedWaitingStability: "observed_waiting_stability",
  Finalized: "finalized",
  Abandoned: "abandoned",
} as const;

export type Status = (typeof Status)[keyof typeof Status];

const ACTIVE_STATUSES: readonly Status[] = [
  Status.PendingSubmission,
  Status.SubmittedLocalFinalizationPending,
  Status.SubmittedUnconfirmed,
  Status.ObservedWaitingStability,
];

export type Row = {
  [Columns.HEADER_HASH]: Buffer;
  [Columns.SUBMITTED_TX_HASH]: Buffer | null;
  [Columns.BLOCK_END_TIME]: Date;
  [Columns.STATUS]: Status;
  [Columns.OBSERVED_CONFIRMED_AT_MS]: bigint | null;
  [Columns.CREATED_AT]: Date;
  [Columns.UPDATED_AT]: Date;
};

export type Record = Row & {
  readonly depositEventIds: readonly Buffer[];
  readonly mempoolTxIds: readonly Buffer[];
};

export type PrepareInput = {
  readonly headerHash: Buffer;
  readonly blockEndTime: Date;
  readonly depositEventIds: readonly Buffer[];
  readonly mempoolTxIds: readonly Buffer[];
};

const uniqueBuffers = (values: readonly Buffer[]): readonly Buffer[] =>
  Array.from(new Set(values.map((value) => value.toString("hex")))).map((hex) =>
    Buffer.from(hex, "hex"),
  );

const retrieveMembers = (
  sql: SqlClient.SqlClient,
  memberTableName: string,
  headerHash: Buffer,
): Effect.Effect<readonly Buffer[], never, never> =>
  Effect.gen(function* () {
    const rows = yield* sql<{
      [MemberColumns.MEMBER_ID]: Buffer;
    }>`SELECT ${sql(MemberColumns.MEMBER_ID)} FROM ${sql(memberTableName)}
      WHERE ${sql(MemberColumns.HEADER_HASH)} = ${headerHash}
      ORDER BY ${sql(MemberColumns.ORDINAL)} ASC`;
    return rows.map((row) => row[MemberColumns.MEMBER_ID]);
  }).pipe(Effect.orDie);

const retrieveRecord = (
  sql: SqlClient.SqlClient,
  row: Row,
): Effect.Effect<Record, never, never> =>
  Effect.gen(function* () {
    const [depositEventIds, mempoolTxIds] = yield* Effect.all(
      [
        retrieveMembers(sql, depositsTableName, row[Columns.HEADER_HASH]),
        retrieveMembers(sql, txsTableName, row[Columns.HEADER_HASH]),
      ],
      { concurrency: "unbounded" },
    );
    return {
      ...row,
      depositEventIds,
      mempoolTxIds,
    };
  }).pipe(Effect.orDie);

export const createTables: Effect.Effect<void, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
          ${sql(Columns.HEADER_HASH)} BYTEA PRIMARY KEY,
          ${sql(Columns.SUBMITTED_TX_HASH)} BYTEA UNIQUE,
          ${sql(Columns.BLOCK_END_TIME)} TIMESTAMPTZ NOT NULL,
          ${sql(Columns.STATUS)} TEXT NOT NULL,
          ${sql(Columns.OBSERVED_CONFIRMED_AT_MS)} BIGINT,
          ${sql(Columns.CREATED_AT)} TIMESTAMPTZ NOT NULL DEFAULT NOW(),
          ${sql(Columns.UPDATED_AT)} TIMESTAMPTZ NOT NULL DEFAULT NOW(),
          CHECK (${sql(Columns.STATUS)} IN (
            'pending_submission',
            'submitted_local_finalization_pending',
            'submitted_unconfirmed',
            'observed_waiting_stability',
            'finalized',
            'abandoned'
          ))
        );`;
        yield* sql`CREATE TABLE IF NOT EXISTS ${sql(depositsTableName)} (
          ${sql(MemberColumns.HEADER_HASH)} BYTEA NOT NULL REFERENCES ${sql(
            tableName,
          )}(${sql(Columns.HEADER_HASH)}) ON DELETE CASCADE,
          ${sql(MemberColumns.MEMBER_ID)} BYTEA NOT NULL REFERENCES ${sql(
            "deposits_utxos",
          )}(${sql("event_id")}) ON DELETE RESTRICT,
          ${sql(MemberColumns.ORDINAL)} INTEGER NOT NULL,
          PRIMARY KEY (${sql(MemberColumns.HEADER_HASH)}, ${sql(
            MemberColumns.MEMBER_ID,
          )}),
          UNIQUE (${sql(MemberColumns.HEADER_HASH)}, ${sql(
            MemberColumns.ORDINAL,
          )})
        );`;
        yield* sql`CREATE TABLE IF NOT EXISTS ${sql(txsTableName)} (
          ${sql(MemberColumns.HEADER_HASH)} BYTEA NOT NULL REFERENCES ${sql(
            tableName,
          )}(${sql(Columns.HEADER_HASH)}) ON DELETE CASCADE,
          ${sql(MemberColumns.MEMBER_ID)} BYTEA NOT NULL,
          ${sql(MemberColumns.ORDINAL)} INTEGER NOT NULL,
          PRIMARY KEY (${sql(MemberColumns.HEADER_HASH)}, ${sql(
            MemberColumns.MEMBER_ID,
          )}),
          UNIQUE (${sql(MemberColumns.HEADER_HASH)}, ${sql(
            MemberColumns.ORDINAL,
          )})
        );`;
        yield* sql`DROP INDEX IF EXISTS ${sql(
          `uniq_${tableName}_single_active`,
        )};`;
        yield* sql`CREATE UNIQUE INDEX IF NOT EXISTS ${sql(
          `uniq_${tableName}_single_active`,
        )} ON ${sql(tableName)} ((1))
          WHERE ${sql(Columns.STATUS)} IN (
            'pending_submission',
            'submitted_local_finalization_pending',
            'submitted_unconfirmed',
            'observed_waiting_stability'
          );`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.STATUS}`,
        )} ON ${sql(tableName)} (${sql(Columns.STATUS)});`;
      }),
    );
  }).pipe(
    Effect.withLogSpan(`creating table ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to create pending-finalization tables",
    ),
  );

export const retrieveActive = (): Effect.Effect<
  Option.Option<Record>,
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.STATUS)} IN ${sql.in(ACTIVE_STATUSES)}
      ORDER BY ${sql(Columns.CREATED_AT)} ASC
      LIMIT 1`;
    if (rows.length <= 0) {
      return Option.none();
    }
    return Option.some(yield* retrieveRecord(sql, rows[0]!));
  }).pipe(
    Effect.withLogSpan(`retrieveActive ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve active pending-finalization record",
    ),
  );

export const retrieveByHeaderHash = (
  headerHash: Buffer,
): Effect.Effect<Option.Option<Record>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
      LIMIT 1`;
    if (rows.length <= 0) {
      return Option.none();
    }
    return Option.some(yield* retrieveRecord(sql, rows[0]!));
  }).pipe(
    Effect.withLogSpan(`retrieveByHeaderHash ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve pending-finalization record by header hash",
    ),
  );

export const preparePendingSubmission = (
  input: PrepareInput,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const depositEventIds = uniqueBuffers(input.depositEventIds);
    const mempoolTxIds = uniqueBuffers(input.mempoolTxIds);
    yield* sql.withTransaction(
      Effect.gen(function* () {
        const activeRows = yield* sql<Row>`SELECT * FROM ${sql(tableName)}
          WHERE ${sql(Columns.STATUS)} IN ${sql.in(ACTIVE_STATUSES)}
          LIMIT 1`;
        const active = activeRows[0];
        if (
          active !== undefined &&
          !active[Columns.HEADER_HASH].equals(input.headerHash)
        ) {
          return yield* Effect.fail(
            new DatabaseError({
              table: tableName,
              message:
                "Refusing to prepare a new pending block while another active pending-finalization record exists",
              cause: `active_header_hash=${active[Columns.HEADER_HASH].toString(
                "hex",
              )},requested_header_hash=${input.headerHash.toString("hex")}`,
            }),
          );
        }
        if (active !== undefined) {
          yield* sql`DELETE FROM ${sql(tableName)}
            WHERE ${sql(Columns.HEADER_HASH)} = ${input.headerHash}
              AND ${sql(Columns.STATUS)} = ${Status.PendingSubmission}`;
        }
        yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert({
          [Columns.HEADER_HASH]: input.headerHash,
          [Columns.SUBMITTED_TX_HASH]: null,
          [Columns.BLOCK_END_TIME]: input.blockEndTime,
          [Columns.STATUS]: Status.PendingSubmission,
          [Columns.OBSERVED_CONFIRMED_AT_MS]: null,
        })}`;
        if (depositEventIds.length > 0) {
          yield* sql`INSERT INTO ${sql(depositsTableName)} ${sql.insert(
            depositEventIds.map((eventId, ordinal) => ({
              [MemberColumns.HEADER_HASH]: input.headerHash,
              [MemberColumns.MEMBER_ID]: eventId,
              [MemberColumns.ORDINAL]: ordinal,
            })),
          )}`;
        }
        if (mempoolTxIds.length > 0) {
          yield* sql`INSERT INTO ${sql(txsTableName)} ${sql.insert(
            mempoolTxIds.map((txId, ordinal) => ({
              [MemberColumns.HEADER_HASH]: input.headerHash,
              [MemberColumns.MEMBER_ID]: txId,
              [MemberColumns.ORDINAL]: ordinal,
            })),
          )}`;
        }
      }),
    );
  }).pipe(
    Effect.withLogSpan(`preparePendingSubmission ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to prepare pending block finalization",
    ),
  );

export const markSubmitted = (
  headerHash: Buffer,
  submittedTxHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.SUBMITTED_TX_HASH)} = ${submittedTxHash},
          ${sql(Columns.STATUS)} = ${Status.SubmittedLocalFinalizationPending},
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} = ${Status.PendingSubmission}
      RETURNING *`;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Failed to mark pending block as submitted",
          cause: `header_hash=${headerHash.toString("hex")}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`markSubmitted ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark pending block as submitted",
    ),
  );

export const markLocalFinalizationComplete = (
  headerHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.SubmittedUnconfirmed},
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} IN (
          ${Status.SubmittedLocalFinalizationPending},
          ${Status.SubmittedUnconfirmed}
        )
      RETURNING *`;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Failed to mark pending block as locally finalized and awaiting confirmation",
          cause: `header_hash=${headerHash.toString("hex")}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`markLocalFinalizationComplete ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark pending block local finalization complete",
    ),
  );

export const markObservedWaitingStability = (
  headerHash: Buffer,
  observedConfirmedAtMs: bigint,
  submittedTxHash?: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.SUBMITTED_TX_HASH)} = COALESCE(
            ${sql(Columns.SUBMITTED_TX_HASH)},
            ${submittedTxHash ?? null}
          ),
          ${sql(Columns.STATUS)} = ${Status.ObservedWaitingStability},
          ${sql(Columns.OBSERVED_CONFIRMED_AT_MS)} = COALESCE(
            ${sql(Columns.OBSERVED_CONFIRMED_AT_MS)},
            ${observedConfirmedAtMs}
          ),
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} IN (
          ${Status.PendingSubmission},
          ${Status.SubmittedLocalFinalizationPending},
          ${Status.SubmittedUnconfirmed},
          ${Status.ObservedWaitingStability}
        )
      RETURNING *`;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Failed to mark pending block as observed waiting stability",
          cause: `header_hash=${headerHash.toString("hex")}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`markObservedWaitingStability ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark pending block as observed waiting stability",
    ),
  );

export const markFinalized = (
  headerHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Finalized},
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} IN (
          ${Status.SubmittedUnconfirmed},
          ${Status.ObservedWaitingStability}
        )
      RETURNING *`;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Failed to finalize pending block journal",
          cause: `header_hash=${headerHash.toString("hex")}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`markFinalized ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to finalize pending block journal",
    ),
  );

export const markAbandoned = (
  headerHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Row>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Abandoned},
          ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
        AND ${sql(Columns.STATUS)} IN ${sql.in(ACTIVE_STATUSES)}
      RETURNING *`;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Failed to abandon pending block journal",
          cause: `header_hash=${headerHash.toString("hex")}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`markAbandoned ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to abandon pending block journal",
    ),
  );

export const clear = Effect.all(
  [
    clearTable(depositsTableName),
    clearTable(txsTableName),
    clearTable(tableName),
  ],
  { discard: true },
);
