import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import { Database } from "@/services/database.js";

export const tableName = "commit_build_calibration";

export enum Columns {
  ID = "id",
  MS_PER_TX_EWMA = "ms_per_tx_ewma",
  SAMPLE_COUNT = "sample_count",
  UPDATED_AT = "updated_at",
}

export type State = {
  readonly msPerTxEwma: number;
  readonly sampleCount: bigint;
  readonly updatedAt: Date;
};

type RawRow = {
  readonly [Columns.MS_PER_TX_EWMA]: number | string;
  readonly [Columns.SAMPLE_COUNT]: bigint | number | string;
  readonly [Columns.UPDATED_AT]: Date;
};

export const retrieve: Effect.Effect<State, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const [row] = yield* sql<RawRow>`SELECT
      ${sql(Columns.MS_PER_TX_EWMA)},
      ${sql(Columns.SAMPLE_COUNT)},
      ${sql(Columns.UPDATED_AT)}
      FROM ${sql(tableName)} WHERE ${sql(Columns.ID)} = 1`;
    if (row === undefined) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Commit build calibration singleton row is missing",
          cause: undefined,
        }),
      );
    }
    return {
      msPerTxEwma: Number(row[Columns.MS_PER_TX_EWMA]),
      sampleCount: BigInt(row[Columns.SAMPLE_COUNT]),
      updatedAt: row[Columns.UPDATED_AT],
    };
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve commit build calibration",
    ),
  );

export const update = (
  msPerTxEwma: number,
): Effect.Effect<State, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (!Number.isFinite(msPerTxEwma) || msPerTxEwma <= 0) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Refusing to persist invalid commit build calibration",
          cause: `ms_per_tx_ewma=${msPerTxEwma.toString()}`,
        }),
      );
    }
    const sql = yield* SqlClient.SqlClient;
    const [row] = yield* sql<RawRow>`UPDATE ${sql(tableName)} SET
      ${sql(Columns.MS_PER_TX_EWMA)} = ${msPerTxEwma},
      ${sql(Columns.SAMPLE_COUNT)} = ${sql(Columns.SAMPLE_COUNT)} + 1,
      ${sql(Columns.UPDATED_AT)} = CURRENT_TIMESTAMP
      WHERE ${sql(Columns.ID)} = 1
      RETURNING ${sql(Columns.MS_PER_TX_EWMA)},
        ${sql(Columns.SAMPLE_COUNT)}, ${sql(Columns.UPDATED_AT)}`;
    if (row === undefined) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message: "Commit build calibration singleton row is missing",
          cause: undefined,
        }),
      );
    }
    return {
      msPerTxEwma: Number(row[Columns.MS_PER_TX_EWMA]),
      sampleCount: BigInt(row[Columns.SAMPLE_COUNT]),
      updatedAt: row[Columns.UPDATED_AT],
    };
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to update commit build calibration",
    ),
  );
