import { Database } from "@/services/database.js";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { Address } from "@lucid-evolution/lucid";
import * as Ledger from "@/database/utils/ledger.js";
import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";

export const tableName = "mempool_ledger";

export enum Columns {
  TX_ID = Ledger.Columns.TX_ID,
  OUTREF = Ledger.Columns.OUTREF,
  OUTPUT = Ledger.Columns.OUTPUT,
  ADDRESS = Ledger.Columns.ADDRESS,
  SOURCE_EVENT_ID = "source_event_id",
  TIMESTAMPTZ = Ledger.Columns.TIMESTAMPTZ,
}

export type EntryNoTimeStamp = {
  [Columns.TX_ID]: Buffer;
  [Columns.OUTREF]: Buffer;
  [Columns.OUTPUT]: Buffer;
  [Columns.ADDRESS]: Address;
  [Columns.SOURCE_EVENT_ID]: Buffer | null;
};

export type EntryWithTimeStamp = EntryNoTimeStamp & {
  [Columns.TIMESTAMPTZ]: Date;
};

export type Entry = EntryNoTimeStamp | EntryWithTimeStamp;

export type DepositEntry = EntryNoTimeStamp & {
  [Columns.SOURCE_EVENT_ID]: Buffer;
};

const normalizeEntry = (
  entry: Ledger.Entry | Entry,
): EntryNoTimeStamp => ({
  [Columns.TX_ID]: entry[Ledger.Columns.TX_ID],
  [Columns.OUTREF]: entry[Ledger.Columns.OUTREF],
  [Columns.OUTPUT]: entry[Ledger.Columns.OUTPUT],
  [Columns.ADDRESS]: entry[Ledger.Columns.ADDRESS],
  [Columns.SOURCE_EVENT_ID]:
    Columns.SOURCE_EVENT_ID in entry
      ? entry[Columns.SOURCE_EVENT_ID] ?? null
      : null,
});

export const createTable: Effect.Effect<void, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
          ${sql(Columns.TX_ID)} BYTEA NOT NULL,
          ${sql(Columns.OUTREF)} BYTEA NOT NULL,
          ${sql(Columns.OUTPUT)} BYTEA NOT NULL,
          ${sql(Columns.ADDRESS)} TEXT NOT NULL,
          ${sql(Columns.SOURCE_EVENT_ID)} BYTEA,
          ${sql(Columns.TIMESTAMPTZ)} TIMESTAMPTZ NOT NULL DEFAULT(NOW()),
          PRIMARY KEY (${sql(Columns.OUTREF)}),
          FOREIGN KEY (${sql(Columns.SOURCE_EVENT_ID)})
            REFERENCES ${sql("deposits_utxos")}(${sql("event_id")})
            ON DELETE RESTRICT
        );`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.ADDRESS}`,
        )} ON ${sql(tableName)} (${sql(Columns.ADDRESS)});`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.SOURCE_EVENT_ID}`,
        )} ON ${sql(tableName)} (${sql(Columns.SOURCE_EVENT_ID)});`;
        yield* sql`DROP INDEX IF EXISTS ${sql(
          `uniq_${tableName}_${Columns.SOURCE_EVENT_ID}`,
        )};`;
        yield* sql`CREATE UNIQUE INDEX IF NOT EXISTS ${sql(
          `uniq_${tableName}_${Columns.SOURCE_EVENT_ID}`,
        )} ON ${sql(tableName)} (${sql(Columns.SOURCE_EVENT_ID)});`;
      }),
    );
  }).pipe(
    Effect.withLogSpan(`creating table ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to create the table"),
  );

export const insert = (
  entries: readonly (Ledger.Entry | Entry)[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insert Ledger UTxOs`);
    if (entries.length <= 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(
      entries.map(normalizeEntry),
    )}
      ON CONFLICT (${sql(Columns.OUTREF)}) DO NOTHING`;
  }).pipe(
    Effect.withLogSpan(`insertEntries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insertEntries: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to insert given UTxOs"),
  );

export const insertDepositEntriesStrict = (
  entries: readonly DepositEntry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (entries.length <= 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Pick<EntryWithTimeStamp, Columns.SOURCE_EVENT_ID>>`
      INSERT INTO ${sql(tableName)} ${sql.insert(entries)}
      RETURNING ${sql(Columns.SOURCE_EVENT_ID)}
    `;
    if (rows.length !== entries.length) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Failed to insert projected deposit UTxOs into mempool_ledger exactly once",
          cause: `requested=${entries.length},inserted=${rows.length}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`insertDepositEntriesStrict ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to insert projected deposits into mempool_ledger",
    ),
  );

export const reconcileDepositEntries = (
  entries: readonly DepositEntry[],
): Effect.Effect<number, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (entries.length <= 0) {
      return 0;
    }
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Pick<EntryWithTimeStamp, Columns.SOURCE_EVENT_ID>>`
      INSERT INTO ${sql(tableName)} ${sql.insert(entries)}
      ON CONFLICT (${sql(Columns.SOURCE_EVENT_ID)}) DO UPDATE SET
        ${sql(Columns.SOURCE_EVENT_ID)} =
          ${sql(tableName)}.${sql(Columns.SOURCE_EVENT_ID)}
      WHERE ${sql(tableName)}.${sql(Columns.TX_ID)} = EXCLUDED.${sql(
        Columns.TX_ID,
      )}
        AND ${sql(tableName)}.${sql(Columns.OUTREF)} = EXCLUDED.${sql(
          Columns.OUTREF,
        )}
        AND ${sql(tableName)}.${sql(Columns.OUTPUT)} = EXCLUDED.${sql(
          Columns.OUTPUT,
        )}
        AND ${sql(tableName)}.${sql(Columns.ADDRESS)} = EXCLUDED.${sql(
          Columns.ADDRESS,
        )}
      RETURNING ${sql(Columns.SOURCE_EVENT_ID)}
    `;
    if (rows.length !== entries.length) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Failed to reconcile projected deposits because an existing source_event_id row does not match the requested projection payload",
          cause: `requested=${entries.length},reconciled=${rows.length}`,
        }),
      );
    }
    return rows.length;
  }).pipe(
    Effect.withLogSpan(`reconcileDepositEntries ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to reconcile projected deposits into mempool_ledger",
    ),
  );

export const retrieve: Effect.Effect<
  readonly EntryWithTimeStamp[],
  DatabaseError,
  Database
> = Effect.gen(function* () {
  yield* Effect.logDebug(`${tableName} db: attempt to retrieveEntries`);
  const sql = yield* SqlClient.SqlClient;
  return yield* sql<EntryWithTimeStamp>`SELECT * FROM ${sql(tableName)}`;
}).pipe(
  Effect.withLogSpan(`retrieveEntries ${tableName}`),
  Effect.tapErrorTag("SqlError", (e) =>
    Effect.logError(`${tableName} db: retrieveEntries: ${JSON.stringify(e)}`),
  ),
  sqlErrorToDatabaseError(tableName, "Failed to retrieve the whole ledger"),
);

export const retrieveByAddress = (
  address: string,
): Effect.Effect<readonly EntryWithTimeStamp[], DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to retrieve Ledger UTxOs`);
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<EntryWithTimeStamp>`SELECT * FROM ${sql(
      tableName,
    )} WHERE ${sql(Columns.ADDRESS)} = ${address}`;
  }).pipe(
    Effect.withLogSpan(`retrieveEntriesWithAddress ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieveEntriesWithAddress: ${JSON.stringify(e)}`,
      ),
    ),
    sqlErrorToDatabaseError(
      tableName,
      `Failed to retrieve UTxOs of address: ${address}`,
    ),
  );

export const retrieveByTxOutRefs = (
  outrefs: readonly Buffer[],
): Effect.Effect<readonly EntryWithTimeStamp[], DatabaseError, Database> =>
  Effect.gen(function* () {
    if (outrefs.length === 0) {
      return [];
    }
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<EntryWithTimeStamp>`SELECT * FROM ${sql(
      tableName,
    )} WHERE ${sql(Columns.OUTREF)} IN ${sql.in(outrefs)}`;
  }).pipe(
    Effect.withLogSpan(`retrieveEntriesByOutRefs ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieveEntriesByOutRefs: ${JSON.stringify(e)}`,
      ),
    ),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve UTxOs for the given outrefs",
    ),
  );

export const retrieveBySourceEventIds = (
  eventIds: readonly Buffer[],
): Effect.Effect<readonly EntryWithTimeStamp[], DatabaseError, Database> =>
  Effect.gen(function* () {
    if (eventIds.length === 0) {
      return [];
    }
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<EntryWithTimeStamp>`SELECT * FROM ${sql(
      tableName,
    )} WHERE ${sql(Columns.SOURCE_EVENT_ID)} IN ${sql.in(eventIds)}`;
  }).pipe(
    Effect.withLogSpan(`retrieveEntriesBySourceEventIds ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieveEntriesBySourceEventIds: ${JSON.stringify(e)}`,
      ),
    ),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve deposit-origin UTxOs by source event ids",
    ),
  );

export const clearUTxOs = (
  refs: Buffer[],
): Effect.Effect<readonly Buffer[], DatabaseError, Database> =>
  Effect.gen(function* () {
    if (refs.length === 0) {
      return [];
    }
    const sql = yield* SqlClient.SqlClient;
    const deleted = yield* sql<{ [Columns.SOURCE_EVENT_ID]: Buffer | null }>`
      DELETE FROM ${sql(tableName)}
      WHERE ${sql(Columns.OUTREF)} IN ${sql.in(refs)}
      RETURNING ${sql(Columns.SOURCE_EVENT_ID)}
    `;
    return deleted
      .map((row) => row[Columns.SOURCE_EVENT_ID])
      .filter((eventId): eventId is Buffer => eventId !== null);
  }).pipe(sqlErrorToDatabaseError(tableName, "Failed to delete given UTxOs"));

export const clear = clearTable(tableName);
