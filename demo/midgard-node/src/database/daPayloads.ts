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
  TRANSACTIONS_ROOT = "transactions_root",
  DEPOSITS_ROOT = "deposits_root",
  WITHDRAWALS_ROOT = "withdrawals_root",
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
  [Columns.TRANSACTIONS_ROOT]: string;
  [Columns.DEPOSITS_ROOT]: string;
  [Columns.WITHDRAWALS_ROOT]: string;
  [Columns.BLOCK_START_TIME]: Date;
  [Columns.BLOCK_END_TIME]: Date;
  [Columns.CREATED_AT]: Date;
  [Columns.UPDATED_AT]: Date;
};

export type InsertInput = Pick<
  Row,
  | Columns.HEADER_HASH
  | Columns.VERSION
  | Columns.PAYLOAD_CBOR
  | Columns.PAYLOAD_SHA256
  | Columns.UTXOS_ROOT
  | Columns.TRANSACTIONS_ROOT
  | Columns.DEPOSITS_ROOT
  | Columns.WITHDRAWALS_ROOT
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
        AND ${sql(tableName)}.${sql(Columns.TRANSACTIONS_ROOT)} = EXCLUDED.${sql(
          Columns.TRANSACTIONS_ROOT,
        )}
        AND ${sql(tableName)}.${sql(Columns.DEPOSITS_ROOT)} = EXCLUDED.${sql(
          Columns.DEPOSITS_ROOT,
        )}
        AND ${sql(tableName)}.${sql(Columns.WITHDRAWALS_ROOT)} = EXCLUDED.${sql(
          Columns.WITHDRAWALS_ROOT,
        )}
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
    const rows = yield* sql<Row>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.HEADER_HASH)} = ${headerHash}
      LIMIT 1`;
    return rows.length === 0 ? Option.none() : Option.some(rows[0]!);
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
