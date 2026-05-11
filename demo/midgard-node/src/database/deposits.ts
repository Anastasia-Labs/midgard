import { Database } from "@/services/database.js";
import { Effect } from "effect";
import {
  DatabaseError,
  sqlErrorToDatabaseError,
} from "@/database/utils/common.js";
import * as SDK from "@al-ft/midgard-sdk";
import { CML, Data as LucidData } from "@lucid-evolution/lucid";
import { SqlClient } from "@effect/sql";
import * as UserEvents from "@/database/utils/user-events.js";
import * as Ledger from "@/database/utils/ledger.js";

export const tableName = "deposits_utxos";

export enum Columns {
  ID = UserEvents.Columns.ID,
  INFO = UserEvents.Columns.INFO,
  INCLUSION_TIME = UserEvents.Columns.INCLUSION_TIME,
  LEDGER_TX_ID = "ledger_tx_id",
  LEDGER_OUTPUT = "ledger_output",
  LEDGER_ADDRESS = "ledger_address",
}

export type Entry = UserEvents.Entry & {
  [Columns.LEDGER_TX_ID]: Buffer | null;
  [Columns.LEDGER_OUTPUT]: Buffer | null;
  [Columns.LEDGER_ADDRESS]: string | null;
};

export const createTable: Effect.Effect<void, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
          ${sql(Columns.ID)} BYTEA NOT NULL,
          ${sql(Columns.INFO)} BYTEA NOT NULL,
          ${sql(Columns.INCLUSION_TIME)} TIMESTAMPTZ NOT NULL,
          ${sql(Columns.LEDGER_TX_ID)} BYTEA,
          ${sql(Columns.LEDGER_OUTPUT)} BYTEA,
          ${sql(Columns.LEDGER_ADDRESS)} TEXT,
          PRIMARY KEY (${sql(Columns.ID)})
        );`;
        yield* sql`ALTER TABLE ${sql(tableName)}
          ADD COLUMN IF NOT EXISTS ${sql(Columns.LEDGER_TX_ID)} BYTEA;`;
        yield* sql`ALTER TABLE ${sql(tableName)}
          ADD COLUMN IF NOT EXISTS ${sql(Columns.LEDGER_OUTPUT)} BYTEA;`;
        yield* sql`ALTER TABLE ${sql(tableName)}
          ADD COLUMN IF NOT EXISTS ${sql(Columns.LEDGER_ADDRESS)} TEXT;`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.INCLUSION_TIME}`,
        )} ON ${sql(tableName)} (${sql(Columns.INCLUSION_TIME)});`;
      }),
    );
  }).pipe(
    Effect.withLogSpan(`creating table ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to create the table"),
  );

export const insertEntry = (
  entry: Entry,
): Effect.Effect<void, DatabaseError, Database> => insertEntries([entry]);

export const insertEntries = (
  entries: Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to insert deposit UTxOs`);
    if (entries.length <= 0) {
      return;
    }

    const sql = yield* SqlClient.SqlClient;
    yield* sql`INSERT INTO ${sql(tableName)} ${sql.insert(entries)}
      ON CONFLICT (${sql(Columns.ID)}) DO UPDATE SET
        ${sql(Columns.INFO)} = EXCLUDED.${sql(Columns.INFO)},
        ${sql(Columns.INCLUSION_TIME)} = EXCLUDED.${sql(Columns.INCLUSION_TIME)},
        ${sql(Columns.LEDGER_TX_ID)} = COALESCE(EXCLUDED.${sql(Columns.LEDGER_TX_ID)}, ${sql(tableName)}.${sql(Columns.LEDGER_TX_ID)}),
        ${sql(Columns.LEDGER_OUTPUT)} = COALESCE(EXCLUDED.${sql(Columns.LEDGER_OUTPUT)}, ${sql(tableName)}.${sql(Columns.LEDGER_OUTPUT)}),
        ${sql(Columns.LEDGER_ADDRESS)} = COALESCE(EXCLUDED.${sql(Columns.LEDGER_ADDRESS)}, ${sql(tableName)}.${sql(Columns.LEDGER_ADDRESS)})`;
  }).pipe(
    Effect.withLogSpan(`insertEntries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: insertEntries: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to insert given deposit UTxOs"),
  );

export const retrieveTimeBoundEntries = (
  startTime: Date,
  endTime: Date,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(
      `${tableName} db: attempt to retrieveTimeBoundEntries`,
    );
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${startTime} < ${sql(Columns.INCLUSION_TIME)}
        AND ${sql(Columns.INCLUSION_TIME)} <= ${endTime}`;
  }).pipe(
    Effect.withLogSpan(`retrieveTimeBoundEntries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(
        `${tableName} db: retrieveTimeBoundEntries: ${JSON.stringify(e)}`,
      ),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve deposit UTxOs"),
  );

export const retrieveAllEntries = (): Effect.Effect<
  readonly Entry[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    yield* Effect.logDebug(`${tableName} db: attempt to retrieveEntries`);
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}`;
  }).pipe(
    Effect.withLogSpan(`retrieveEntries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      Effect.logError(`${tableName} db: retrieveEntries: ${JSON.stringify(e)}`),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve deposit UTxOs"),
  );

export const toLedgerEntry = (
  entry: Entry,
): Effect.Effect<Ledger.Entry, DatabaseError, never> =>
  Effect.gen(function* () {
    if (
      entry[Columns.LEDGER_TX_ID] === null ||
      entry[Columns.LEDGER_OUTPUT] === null ||
      entry[Columns.LEDGER_ADDRESS] === null
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Deposit entry is missing projected ledger columns required for offchain UTxO state",
          cause: `event_id=${entry[Columns.ID].toString("hex")}`,
        }),
      );
    }

    const ledgerOutRef = yield* Effect.try({
      try: () => {
        const outRef = LucidData.from(
          entry[Columns.ID].toString("hex"),
          SDK.OutputReference,
        );
        return Buffer.from(
          CML.TransactionInput.new(
            CML.TransactionHash.from_hex(outRef.transactionId),
            outRef.outputIndex,
          ).to_cbor_bytes(),
        );
      },
      catch: (cause) =>
        new DatabaseError({
          table: tableName,
          message: "Failed to convert deposit event id into ledger outref",
          cause,
        }),
    });

    return {
      [Ledger.Columns.TX_ID]: entry[Columns.LEDGER_TX_ID],
      [Ledger.Columns.OUTREF]: ledgerOutRef,
      [Ledger.Columns.OUTPUT]: entry[Columns.LEDGER_OUTPUT],
      [Ledger.Columns.ADDRESS]: entry[Columns.LEDGER_ADDRESS],
    };
  });

export const retrieveTimeBoundLedgerEntries = (
  startTime: Date,
  endTime: Date,
): Effect.Effect<readonly Ledger.Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const entries = yield* retrieveTimeBoundEntries(startTime, endTime);
    return yield* Effect.forEach(entries, toLedgerEntry);
  });

export const delEntries = (
  ids: Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  UserEvents.delEntries(tableName, ids);

export const pruneOlderThan = (
  cutoff: Date,
): Effect.Effect<number, DatabaseError, Database> =>
  UserEvents.pruneOlderThan(tableName, cutoff);
