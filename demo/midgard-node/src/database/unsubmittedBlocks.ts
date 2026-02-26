import { Database } from "@/services/database.js";
import { Effect } from "effect";
import { SqlClient } from "@effect/sql";
import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";

export const tableName = "unsubmitted_blocks";

export enum Columns {
  SEQUENCE = "sequence",
  BLOCK = "block",
  L1_CBOR = "l1_cbor",
  PRODUCED_UTXOS = "produced_utxos",
  TIMESTAMPTZ = "time_stamp_tz",
}

export type EntryNoMeta = {
  [Columns.BLOCK]: Buffer;
  [Columns.L1_CBOR]: Buffer;
  // Serialized payload for produced UTxOs of this block commit tx.
  [Columns.PRODUCED_UTXOS]: Buffer;
};

export type Entry = EntryNoMeta & {
  [Columns.SEQUENCE]: bigint;
  [Columns.TIMESTAMPTZ]: Date;
};

export const createTable: Effect.Effect<void, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
          ${sql(Columns.SEQUENCE)} BIGSERIAL PRIMARY KEY,
          ${sql(Columns.BLOCK)} BYTEA NOT NULL UNIQUE,
          ${sql(Columns.L1_CBOR)} BYTEA NOT NULL,
          ${sql(Columns.PRODUCED_UTXOS)} BYTEA NOT NULL,
          ${sql(Columns.TIMESTAMPTZ)} TIMESTAMPTZ NOT NULL DEFAULT(NOW())
        );`;

        // Keep migration explicit: do not silently synthesize produced UTxOs.
        yield* sql`ALTER TABLE ${sql(tableName)} ADD COLUMN IF NOT EXISTS ${sql(
          Columns.PRODUCED_UTXOS,
        )} BYTEA`;
        const nullRows = yield* sql<{
          count: string;
        }>`SELECT COUNT(*)::text AS count
          FROM ${sql(tableName)}
          WHERE ${sql(Columns.PRODUCED_UTXOS)} IS NULL`;
        const nullCount = BigInt(nullRows[0]?.count ?? "0");
        if (nullCount > 0n) {
          yield* Effect.fail(
            new DatabaseError({
              message:
                "Found unsubmitted_blocks rows with missing produced_utxos. Backfill them before startup to preserve deterministic chaining.",
              cause: `NULL produced_utxos rows: ${nullCount.toString()}`,
              table: tableName,
            }),
          );
        }
        yield* sql`ALTER TABLE ${sql(tableName)}
          ALTER COLUMN ${sql(Columns.PRODUCED_UTXOS)} SET NOT NULL`;
      }),
    );
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
      ON CONFLICT (${sql(Columns.BLOCK)}) DO UPDATE SET
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

export const deleteByBlocks = (
  blocks: Buffer[] | readonly Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (blocks.length <= 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM ${sql(tableName)} WHERE ${sql.in(
      Columns.BLOCK,
      blocks,
    )}`;
  }).pipe(
    Effect.withLogSpan(`deleteByBlocks ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to remove unsubmitted blocks by header hash",
    ),
  );

export const deleteUpToAndIncludingBlock = (
  block: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql`WITH ${sql("target")} AS (
      SELECT ${sql(Columns.SEQUENCE)}
      FROM ${sql(tableName)}
      WHERE ${sql(Columns.BLOCK)} = ${block}
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
