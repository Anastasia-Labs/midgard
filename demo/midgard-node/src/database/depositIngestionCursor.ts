import { Database } from "@/services/database.js";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
  clearTable,
} from "@/database/utils/common.js";

export const tableName = "deposit_ingestion_cursor";
export const DEFAULT_CURSOR_NAME = "stable_l1_deposits";

export enum Columns {
  CURSOR_NAME = "cursor_name",
  STABLE_TIP_HASH = "stable_tip_hash",
  STABLE_TIP_SLOT = "stable_tip_slot",
  STABLE_TIP_TIME_MS = "stable_tip_time_ms",
  SCAN_UPPER_BOUND_TIME_MS = "scan_upper_bound_time_ms",
  LAST_SCANNED_EVENT_ID = "last_scanned_event_id",
  UPDATED_AT = "updated_at",
}

export type Entry = {
  [Columns.CURSOR_NAME]: string;
  [Columns.STABLE_TIP_HASH]: string;
  [Columns.STABLE_TIP_SLOT]: bigint;
  [Columns.STABLE_TIP_TIME_MS]: bigint;
  [Columns.SCAN_UPPER_BOUND_TIME_MS]: bigint;
  [Columns.LAST_SCANNED_EVENT_ID]: Buffer;
  [Columns.UPDATED_AT]: Date;
};

export type AdvanceCursorInput = {
  readonly stableTipHash: string;
  readonly stableTipSlot: bigint;
  readonly stableTipTimeMs: bigint;
  readonly scanUpperBoundTimeMs: bigint;
  readonly lastScannedEventId: Buffer;
};

export const createTable: Effect.Effect<void, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
      ${sql(Columns.CURSOR_NAME)} TEXT PRIMARY KEY,
      ${sql(Columns.STABLE_TIP_HASH)} TEXT NOT NULL,
      ${sql(Columns.STABLE_TIP_SLOT)} BIGINT NOT NULL,
      ${sql(Columns.STABLE_TIP_TIME_MS)} BIGINT NOT NULL,
      ${sql(Columns.SCAN_UPPER_BOUND_TIME_MS)} BIGINT NOT NULL,
      ${sql(Columns.LAST_SCANNED_EVENT_ID)} BYTEA NOT NULL,
      ${sql(Columns.UPDATED_AT)} TIMESTAMPTZ NOT NULL DEFAULT NOW()
    );`;
  }).pipe(
    Effect.withLogSpan(`creating table ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to create the table"),
  );

export const retrieve = (
  cursorName = DEFAULT_CURSOR_NAME,
): Effect.Effect<Option.Option<Entry>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.CURSOR_NAME)} = ${cursorName}
      LIMIT 1`;
    return rows.length <= 0 ? Option.none() : Option.some(rows[0]!);
  }).pipe(
    Effect.withLogSpan(`retrieve ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve deposit cursor"),
  );

export const advance = (
  input: AdvanceCursorInput,
  cursorName = DEFAULT_CURSOR_NAME,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert({
      [Columns.CURSOR_NAME]: cursorName,
      [Columns.STABLE_TIP_HASH]: input.stableTipHash,
      [Columns.STABLE_TIP_SLOT]: input.stableTipSlot,
      [Columns.STABLE_TIP_TIME_MS]: input.stableTipTimeMs,
      [Columns.SCAN_UPPER_BOUND_TIME_MS]: input.scanUpperBoundTimeMs,
      [Columns.LAST_SCANNED_EVENT_ID]: input.lastScannedEventId,
    })}
      ON CONFLICT (${sql(Columns.CURSOR_NAME)}) DO UPDATE SET
        ${sql(Columns.STABLE_TIP_HASH)} = EXCLUDED.${sql(
          Columns.STABLE_TIP_HASH,
        )},
        ${sql(Columns.STABLE_TIP_SLOT)} = EXCLUDED.${sql(
          Columns.STABLE_TIP_SLOT,
        )},
        ${sql(Columns.STABLE_TIP_TIME_MS)} = EXCLUDED.${sql(
          Columns.STABLE_TIP_TIME_MS,
        )},
        ${sql(Columns.SCAN_UPPER_BOUND_TIME_MS)} = EXCLUDED.${sql(
          Columns.SCAN_UPPER_BOUND_TIME_MS,
        )},
        ${sql(Columns.LAST_SCANNED_EVENT_ID)} = EXCLUDED.${sql(
          Columns.LAST_SCANNED_EVENT_ID,
        )},
        ${sql(Columns.UPDATED_AT)} = NOW()`;
  }).pipe(
    Effect.withLogSpan(`advance ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to advance deposit cursor"),
  );

export const clear = clearTable(tableName);
