import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { Database } from "@/services/database.js";

export const tableName = "da_payloads";

export enum Columns {
  HEADER_HASH = "header_hash",
  VERSION = "version",
  PAYLOAD_CBOR = "payload_cbor",
  PAYLOAD_SHA256 = "payload_sha256",
  UTXOS_ROOT = "utxos_root",
  FORCED_TRANSACTIONS_ROOT = "forced_transactions_root",
  TRANSACTIONS_ROOT = "transactions_root",
  DEPOSITS_ROOT = "deposits_root",
  WITHDRAWALS_ROOT = "withdrawals_root",
  TRANSITION_TRACE_ROOT = "transition_trace_root",
  EVENT_TO_STEP_ROOT = "event_to_step_root",
  WITHDRAWAL_COUNT = "withdrawal_count",
  FORCED_TRANSACTION_COUNT = "forced_transaction_count",
  L2_TRANSACTION_COUNT = "l2_transaction_count",
  DEPOSIT_COUNT = "deposit_count",
  TOTAL_EVENT_COUNT = "total_event_count",
  TRANSITION_STEP_COUNT = "transition_step_count",
  BLOCK_START_TIME = "block_start_time",
  BLOCK_END_TIME = "block_end_time",
  CREATED_AT = "created_at",
  UPDATED_AT = "updated_at",
}

export type Row = {
  [Columns.HEADER_HASH]: Buffer;
  [Columns.VERSION]: number;
  [Columns.PAYLOAD_CBOR]: Buffer;
  [Columns.PAYLOAD_SHA256]: Buffer;
  [Columns.UTXOS_ROOT]: string;
  [Columns.FORCED_TRANSACTIONS_ROOT]: string;
  [Columns.TRANSACTIONS_ROOT]: string;
  [Columns.DEPOSITS_ROOT]: string;
  [Columns.WITHDRAWALS_ROOT]: string;
  [Columns.TRANSITION_TRACE_ROOT]: string;
  [Columns.EVENT_TO_STEP_ROOT]: string;
  [Columns.WITHDRAWAL_COUNT]: bigint;
  [Columns.FORCED_TRANSACTION_COUNT]: bigint;
  [Columns.L2_TRANSACTION_COUNT]: bigint;
  [Columns.DEPOSIT_COUNT]: bigint;
  [Columns.TOTAL_EVENT_COUNT]: bigint;
  [Columns.TRANSITION_STEP_COUNT]: bigint;
  [Columns.BLOCK_START_TIME]: Date;
  [Columns.BLOCK_END_TIME]: Date;
  [Columns.CREATED_AT]: Date;
  [Columns.UPDATED_AT]: Date;
};

type PgBigInt = bigint | number | string;

type RawRow = Omit<
  Row,
  | Columns.WITHDRAWAL_COUNT
  | Columns.FORCED_TRANSACTION_COUNT
  | Columns.L2_TRANSACTION_COUNT
  | Columns.DEPOSIT_COUNT
  | Columns.TOTAL_EVENT_COUNT
  | Columns.TRANSITION_STEP_COUNT
> & {
  [Columns.WITHDRAWAL_COUNT]: PgBigInt;
  [Columns.FORCED_TRANSACTION_COUNT]: PgBigInt;
  [Columns.L2_TRANSACTION_COUNT]: PgBigInt;
  [Columns.DEPOSIT_COUNT]: PgBigInt;
  [Columns.TOTAL_EVENT_COUNT]: PgBigInt;
  [Columns.TRANSITION_STEP_COUNT]: PgBigInt;
};

const toBigInt = (value: PgBigInt): bigint =>
  typeof value === "bigint" ? value : BigInt(value);

const normalizeRow = (row: RawRow): Row => ({
  ...row,
  [Columns.WITHDRAWAL_COUNT]: toBigInt(row[Columns.WITHDRAWAL_COUNT]),
  [Columns.FORCED_TRANSACTION_COUNT]: toBigInt(
    row[Columns.FORCED_TRANSACTION_COUNT],
  ),
  [Columns.L2_TRANSACTION_COUNT]: toBigInt(row[Columns.L2_TRANSACTION_COUNT]),
  [Columns.DEPOSIT_COUNT]: toBigInt(row[Columns.DEPOSIT_COUNT]),
  [Columns.TOTAL_EVENT_COUNT]: toBigInt(row[Columns.TOTAL_EVENT_COUNT]),
  [Columns.TRANSITION_STEP_COUNT]: toBigInt(row[Columns.TRANSITION_STEP_COUNT]),
});

export type InsertInput = Pick<
  Row,
  | Columns.HEADER_HASH
  | Columns.VERSION
  | Columns.PAYLOAD_CBOR
  | Columns.PAYLOAD_SHA256
  | Columns.UTXOS_ROOT
  | Columns.FORCED_TRANSACTIONS_ROOT
  | Columns.TRANSACTIONS_ROOT
  | Columns.DEPOSITS_ROOT
  | Columns.WITHDRAWALS_ROOT
  | Columns.TRANSITION_TRACE_ROOT
  | Columns.EVENT_TO_STEP_ROOT
  | Columns.WITHDRAWAL_COUNT
  | Columns.FORCED_TRANSACTION_COUNT
  | Columns.L2_TRANSACTION_COUNT
  | Columns.DEPOSIT_COUNT
  | Columns.TOTAL_EVENT_COUNT
  | Columns.TRANSITION_STEP_COUNT
  | Columns.BLOCK_START_TIME
  | Columns.BLOCK_END_TIME
>;

export const upsertAvailable = (
  input: InsertInput,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Pick<Row, Columns.HEADER_HASH>>`
      INSERT INTO ${sql(tableName)} ${sql.insert(input)}
      ON CONFLICT (${sql(Columns.HEADER_HASH)}) DO UPDATE SET
        ${sql(Columns.UPDATED_AT)} = NOW()
      WHERE ${sql(tableName)}.${sql(Columns.VERSION)} = EXCLUDED.${sql(
        Columns.VERSION,
      )}
        AND ${sql(tableName)}.${sql(Columns.PAYLOAD_CBOR)} = EXCLUDED.${sql(
          Columns.PAYLOAD_CBOR,
        )}
        AND ${sql(tableName)}.${sql(Columns.PAYLOAD_SHA256)} = EXCLUDED.${sql(
          Columns.PAYLOAD_SHA256,
        )}
        AND ${sql(tableName)}.${sql(Columns.UTXOS_ROOT)} = EXCLUDED.${sql(
          Columns.UTXOS_ROOT,
        )}
        AND ${sql(tableName)}.${sql(
          Columns.FORCED_TRANSACTIONS_ROOT,
        )} = EXCLUDED.${sql(Columns.FORCED_TRANSACTIONS_ROOT)}
        AND ${sql(tableName)}.${sql(Columns.TRANSACTIONS_ROOT)} = EXCLUDED.${sql(
          Columns.TRANSACTIONS_ROOT,
        )}
        AND ${sql(tableName)}.${sql(Columns.DEPOSITS_ROOT)} = EXCLUDED.${sql(
          Columns.DEPOSITS_ROOT,
        )}
        AND ${sql(tableName)}.${sql(Columns.WITHDRAWALS_ROOT)} = EXCLUDED.${sql(
          Columns.WITHDRAWALS_ROOT,
        )}
        AND ${sql(tableName)}.${sql(
          Columns.TRANSITION_TRACE_ROOT,
        )} = EXCLUDED.${sql(Columns.TRANSITION_TRACE_ROOT)}
        AND ${sql(tableName)}.${sql(Columns.EVENT_TO_STEP_ROOT)} = EXCLUDED.${sql(
          Columns.EVENT_TO_STEP_ROOT,
        )}
        AND ${sql(tableName)}.${sql(Columns.WITHDRAWAL_COUNT)} = EXCLUDED.${sql(
          Columns.WITHDRAWAL_COUNT,
        )}
        AND ${sql(tableName)}.${sql(
          Columns.FORCED_TRANSACTION_COUNT,
        )} = EXCLUDED.${sql(Columns.FORCED_TRANSACTION_COUNT)}
        AND ${sql(tableName)}.${sql(
          Columns.L2_TRANSACTION_COUNT,
        )} = EXCLUDED.${sql(Columns.L2_TRANSACTION_COUNT)}
        AND ${sql(tableName)}.${sql(Columns.DEPOSIT_COUNT)} = EXCLUDED.${sql(
          Columns.DEPOSIT_COUNT,
        )}
        AND ${sql(tableName)}.${sql(Columns.TOTAL_EVENT_COUNT)} = EXCLUDED.${sql(
          Columns.TOTAL_EVENT_COUNT,
        )}
        AND ${sql(tableName)}.${sql(
          Columns.TRANSITION_STEP_COUNT,
        )} = EXCLUDED.${sql(Columns.TRANSITION_STEP_COUNT)}
        AND ${sql(tableName)}.${sql(Columns.BLOCK_START_TIME)} = EXCLUDED.${sql(
          Columns.BLOCK_START_TIME,
        )}
        AND ${sql(tableName)}.${sql(Columns.BLOCK_END_TIME)} = EXCLUDED.${sql(
          Columns.BLOCK_END_TIME,
        )}
      RETURNING ${sql(Columns.HEADER_HASH)}
    `;
    if (rows.length !== 1) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Refusing to overwrite DA payload because an existing payload for the header differs",
          cause: `header_hash=${input[Columns.HEADER_HASH].toString("hex")}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`upsertAvailable ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to store DA payload"),
  );

export const retrieveByHeaderHash = (
  headerHash: Buffer,
): Effect.Effect<Option.Option<Row>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<RawRow>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
      LIMIT 1`;
    return rows.length === 0
      ? Option.none()
      : Option.some(normalizeRow(rows[0]!));
  }).pipe(
    Effect.withLogSpan(`retrieveByHeaderHash ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve DA payload"),
  );

export const pruneOlderThan = (
  cutoff: Date,
): Effect.Effect<number, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ readonly deleted_count: string }>`
      WITH deleted AS (
        DELETE FROM ${sql(tableName)}
        WHERE ${sql(Columns.CREATED_AT)} < ${cutoff}
        RETURNING 1
      )
      SELECT COUNT(*)::text AS deleted_count FROM deleted`;
    return Number(rows[0]?.deleted_count ?? "0");
  }).pipe(
    Effect.withLogSpan(`pruneOlderThan ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to prune DA payloads"),
  );

export const clear: Effect.Effect<void, DatabaseError, Database> =
  clearTable(tableName);
