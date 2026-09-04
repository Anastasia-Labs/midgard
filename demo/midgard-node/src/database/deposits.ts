import { outRefToCbor } from "@al-ft/lucid-midgard";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import { Database } from "../services/database.js";
import {
  clearTable,
  DatabaseError,
  logDatabaseError,
  sqlErrorToDatabaseError,
} from "./utils/common.js";
import * as Ledger from "./utils/ledger.js";
import * as ProjectedEvents from "./utils/projected-events.js";
import * as UserEvents from "./utils/user-events.js";

export const tableName = "deposits_utxos";

export enum Columns {
  ID = UserEvents.Columns.ID,
  INFO = UserEvents.Columns.INFO,
  INCLUSION_TIME = UserEvents.Columns.INCLUSION_TIME,
  DEPOSIT_L1_TX_HASH = "deposit_l1_tx_hash",
  LEDGER_TX_ID = "ledger_tx_id",
  LEDGER_OUTPUT = "ledger_output",
  LEDGER_ADDRESS = "ledger_address",
  PROJECTED_HEADER_HASH = "projected_header_hash",
  STATUS = "status",
}

export const Status = {
  Awaiting: "awaiting",
  Projected: "projected",
  Consumed: "consumed",
} as const;

export type Status = (typeof Status)[keyof typeof Status];

export type Entry = UserEvents.Entry & {
  [Columns.DEPOSIT_L1_TX_HASH]: Buffer;
  [Columns.LEDGER_TX_ID]: Buffer;
  [Columns.LEDGER_OUTPUT]: Buffer;
  [Columns.LEDGER_ADDRESS]: string;
  [Columns.PROJECTED_HEADER_HASH]: Buffer | null;
  [Columns.STATUS]: Status;
};

const projectedEventsTable = {
  tableName,
  idColumn: Columns.ID,
  inclusionTimeColumn: Columns.INCLUSION_TIME,
  projectedHeaderHashColumn: Columns.PROJECTED_HEADER_HASH,
  statusColumn: Columns.STATUS,
  awaitingStatus: Status.Awaiting,
  projectedStatus: Status.Projected,
  terminalStatus: Status.Consumed,
  entitySingular: "deposit",
  entityPlural: "deposits",
  idLabel: "event_id",
} as const satisfies ProjectedEvents.ProjectedEventTable;

const projectedEventAdapter = ProjectedEvents.makeProjectedEventAdapter<Entry>({
  config: projectedEventsTable,
  projectedPendingStatuses: [Status.Projected, Status.Consumed],
  messages: {
    retrieveByProjectedHeaderHash:
      "Failed to retrieve deposit UTxOs by projected header hash",
    retrievePendingHeaderEntriesUpTo:
      "Failed to retrieve deposits pending header assignment up to the requested time",
    retrieveProjectedPendingHeaderEntries:
      "Failed to retrieve projected deposits awaiting header assignment",
    markAwaitingAsProjected: "Failed to mark awaiting deposits as projected",
    markProjectedByEventIds:
      "Failed to mark deposits as assigned to the given header",
    clearProjectedHeaderAssignmentByEventIds:
      "Failed to clear projected header assignments for the given deposits",
  },
});

const sameEntryPayload = (left: Entry, right: Entry): boolean =>
  left[Columns.ID].equals(right[Columns.ID]) &&
  left[Columns.INFO].equals(right[Columns.INFO]) &&
  left[Columns.INCLUSION_TIME].getTime() ===
    right[Columns.INCLUSION_TIME].getTime() &&
  left[Columns.DEPOSIT_L1_TX_HASH].equals(right[Columns.DEPOSIT_L1_TX_HASH]) &&
  left[Columns.LEDGER_TX_ID].equals(right[Columns.LEDGER_TX_ID]) &&
  left[Columns.LEDGER_OUTPUT].equals(right[Columns.LEDGER_OUTPUT]) &&
  left[Columns.LEDGER_ADDRESS] === right[Columns.LEDGER_ADDRESS];

export const createTable: Effect.Effect<void, DatabaseError, Database> =
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* sql`CREATE TABLE IF NOT EXISTS ${sql(tableName)} (
          ${sql(Columns.ID)} BYTEA NOT NULL,
          ${sql(Columns.INFO)} BYTEA NOT NULL,
          ${sql(Columns.INCLUSION_TIME)} TIMESTAMPTZ NOT NULL,
          ${sql(Columns.DEPOSIT_L1_TX_HASH)} BYTEA NOT NULL,
          ${sql(Columns.LEDGER_TX_ID)} BYTEA NOT NULL,
          ${sql(Columns.LEDGER_OUTPUT)} BYTEA NOT NULL,
          ${sql(Columns.LEDGER_ADDRESS)} TEXT NOT NULL,
          ${sql(Columns.PROJECTED_HEADER_HASH)} BYTEA,
          ${sql(Columns.STATUS)} TEXT NOT NULL,
          PRIMARY KEY (${sql(Columns.ID)}),
          CHECK (${sql(Columns.STATUS)} IN ('awaiting', 'projected', 'consumed')),
          CHECK (
            ${sql(Columns.STATUS)} <> 'awaiting'
            OR ${sql(Columns.PROJECTED_HEADER_HASH)} IS NULL
          )
        );`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.STATUS}_${Columns.INCLUSION_TIME}_${Columns.ID}`,
        )} ON ${sql(tableName)} (
          ${sql(Columns.STATUS)},
          ${sql(Columns.INCLUSION_TIME)},
          ${sql(Columns.ID)}
        );`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.PROJECTED_HEADER_HASH}`,
        )} ON ${sql(tableName)} (${sql(Columns.PROJECTED_HEADER_HASH)});`;
        yield* sql`CREATE INDEX IF NOT EXISTS ${sql(
          `idx_${tableName}_${Columns.DEPOSIT_L1_TX_HASH}`,
        )} ON ${sql(tableName)} (${sql(Columns.DEPOSIT_L1_TX_HASH)});`;
      }),
    );
  }).pipe(
    Effect.withLogSpan(`creating table ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to create the table"),
  );

export const insertEntries = (
  entries: readonly Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (entries.length <= 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    const incomingById = new Map<string, Entry>();
    for (const incoming of entries) {
      const key = incoming[Columns.ID].toString("hex");
      const existingIncoming = incomingById.get(key);
      if (
        existingIncoming !== undefined &&
        !sameEntryPayload(existingIncoming, incoming)
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: tableName,
            message:
              "Refusing to insert deposits because the same event_id appears with conflicting payloads in one batch",
            cause: `event_id=${key}`,
          }),
        );
      }
      incomingById.set(key, incoming);
    }
    const normalizedEntries = [...incomingById.values()];
    const rows = yield* sql<{ [Columns.ID]: Buffer }>`
      INSERT INTO ${sql(tableName)} ${sql.insert(normalizedEntries)}
      ON CONFLICT (${sql(Columns.ID)}) DO UPDATE SET
        ${sql(Columns.ID)} = ${sql(tableName)}.${sql(Columns.ID)}
      WHERE ${sql(tableName)}.${sql(Columns.INFO)} = EXCLUDED.${sql(
        Columns.INFO,
      )}
        AND ${sql(tableName)}.${sql(Columns.INCLUSION_TIME)} = EXCLUDED.${sql(
          Columns.INCLUSION_TIME,
        )}
        AND ${sql(tableName)}.${sql(Columns.DEPOSIT_L1_TX_HASH)} = EXCLUDED.${sql(
          Columns.DEPOSIT_L1_TX_HASH,
        )}
        AND ${sql(tableName)}.${sql(Columns.LEDGER_TX_ID)} = EXCLUDED.${sql(
          Columns.LEDGER_TX_ID,
        )}
        AND ${sql(tableName)}.${sql(Columns.LEDGER_OUTPUT)} = EXCLUDED.${sql(
          Columns.LEDGER_OUTPUT,
        )}
        AND ${sql(tableName)}.${sql(Columns.LEDGER_ADDRESS)} = EXCLUDED.${sql(
          Columns.LEDGER_ADDRESS,
        )}
      RETURNING ${sql(Columns.ID)}
    `;
    if (rows.length !== normalizedEntries.length) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Refusing to upsert deposit because the same event_id has conflicting persisted payload",
          cause: `requested=${normalizedEntries.length},upserted=${rows.length}`,
        }),
      );
    }
  }).pipe(
    Effect.withLogSpan(`insertEntries ${tableName}`),
    Effect.tapErrorTag("SqlError", (e) =>
      logDatabaseError(tableName, "insertEntries", e),
    ),
    sqlErrorToDatabaseError(tableName, "Failed to insert given deposit UTxOs"),
  );

export const retrieveAllEntries = (): Effect.Effect<
  readonly Entry[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      ORDER BY ${sql(Columns.INCLUSION_TIME)} ASC, ${sql(Columns.ID)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveEntries ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve deposit UTxOs"),
  );

export const retrieveByEventId = (
  eventId: Buffer,
): Effect.Effect<Option.Option<Entry>, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.ID)} = ${eventId}
      LIMIT 1`;
    return rows.length === 0 ? Option.none() : Option.some(rows[0]!);
  }).pipe(
    Effect.withLogSpan(`retrieveByEventId ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve deposit UTxO by event id",
    ),
  );

export const retrieveByCardanoTxHash = (
  cardanoTxHash: Buffer,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.DEPOSIT_L1_TX_HASH)} = ${cardanoTxHash}
      ORDER BY ${sql(Columns.INCLUSION_TIME)} ASC, ${sql(Columns.ID)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveByCardanoTxHash ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve deposit UTxOs by Cardano tx hash",
    ),
  );

export const retrieveByProjectedHeaderHash = (
  projectedHeaderHash: Buffer,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  projectedEventAdapter.retrieveByProjectedHeaderHash(projectedHeaderHash);

export const retrieveTimeBoundEntries = (
  startTime: Date,
  endTime: Date,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${startTime} < ${sql(Columns.INCLUSION_TIME)}
        AND ${sql(Columns.INCLUSION_TIME)} <= ${endTime}
      ORDER BY ${sql(Columns.INCLUSION_TIME)} ASC, ${sql(Columns.ID)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveTimeBoundEntries ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve time-bound deposit UTxOs",
    ),
  );

export const retrieveAwaitingEntries = (): Effect.Effect<
  readonly Entry[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.STATUS)} = ${Status.Awaiting}
      ORDER BY ${sql(Columns.INCLUSION_TIME)} ASC, ${sql(Columns.ID)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveAwaitingEntries ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve awaiting deposits"),
  );

export const retrieveAwaitingEntriesDueBy = (
  endTime: Date,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.STATUS)} = ${Status.Awaiting}
        AND ${sql(Columns.INCLUSION_TIME)} <= ${endTime}
      ORDER BY ${sql(Columns.INCLUSION_TIME)} ASC, ${sql(Columns.ID)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveAwaitingEntriesDueBy ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve awaiting deposits due by the requested time",
    ),
  );

export const retrieveProjectedPendingHeaderEntries = (): Effect.Effect<
  readonly Entry[],
  DatabaseError,
  Database
> => projectedEventAdapter.retrieveProjectedPendingHeaderEntries();

export const retrievePendingHeaderEntriesUpTo = (
  endTime: Date,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  projectedEventAdapter.retrievePendingHeaderEntriesUpTo(endTime);

export const retrieveProjectedEntries = (): Effect.Effect<
  readonly Entry[],
  DatabaseError,
  Database
> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.STATUS)} = ${Status.Projected}
      ORDER BY ${sql(Columns.INCLUSION_TIME)} ASC, ${sql(Columns.ID)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveProjectedEntries ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to retrieve projected deposits"),
  );

export const markAwaitingAsProjected = (
  ids: readonly Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  projectedEventAdapter.markAwaitingAsProjected(ids);

export const markProjectedByEventIds = (
  ids: readonly Buffer[],
  projectedHeaderHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  projectedEventAdapter.markProjectedByEventIds(ids, projectedHeaderHash);

export const clearProjectedHeaderAssignmentByEventIds = (
  ids: readonly Buffer[],
  projectedHeaderHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  projectedEventAdapter.clearProjectedHeaderAssignmentByEventIds(
    ids,
    projectedHeaderHash,
  );

export const reopenAfterStateQueueCorrectionByEventIds = (
  ids: readonly Buffer[],
  removedHeaderHash: Buffer,
) =>
  ProjectedEvents.reopenAfterStateQueueCorrectionByEventIds(
    projectedEventsTable,
    ids,
    removedHeaderHash,
  );

export const markConsumedByEventIds = (
  ids: readonly Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (ids.length <= 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    yield* sql`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Consumed}
      WHERE ${sql(Columns.ID)} IN ${sql.in(ids)}
        AND ${sql(Columns.STATUS)} IN (${Status.Projected}, ${Status.Consumed})`;
  }).pipe(
    Effect.withLogSpan(`markConsumedByEventIds ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to mark projected deposits as consumed",
    ),
  );

export const toLedgerEntry = (
  entry: Entry,
): Effect.Effect<Ledger.Entry, DatabaseError, never> =>
  Effect.gen(function* () {
    const ledgerOutRef = yield* Effect.try({
      try: () => {
        const outRef = LucidData.from(
          entry[Columns.ID].toString("hex"),
          SDK.OutputReference,
        );
        // The ledger key is the §5.3 field-0/1 item encoding, matching on-chain
        // `ledger_outref_key` — never CML's minimal-index TransactionInput CBOR.
        return outRefToCbor({
          txHash: outRef.transactionId,
          outputIndex: Number(outRef.outputIndex),
        });
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

export const toMempoolLedgerEntry = (
  entry: Entry,
): Effect.Effect<
  Ledger.Entry & { readonly source_event_id: Buffer },
  DatabaseError,
  never
> =>
  toLedgerEntry(entry).pipe(
    Effect.map((ledgerEntry) => ({
      ...ledgerEntry,
      source_event_id: Buffer.from(entry[Columns.ID]),
    })),
  );

export const delEntries = (
  ids: readonly Buffer[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (ids.length <= 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    yield* sql`DELETE FROM ${sql(tableName)} WHERE ${sql(
      Columns.ID,
    )} IN ${sql.in(ids)}`;
  }).pipe(
    Effect.withLogSpan(`delEntries ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to delete deposit UTxOs"),
  );

export const pruneOlderThan = (
  cutoff: Date,
): Effect.Effect<number, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const deleted = yield* sql<{ [Columns.ID]: Buffer }>`DELETE FROM ${sql(
      tableName,
    )}
      WHERE ${sql(Columns.INCLUSION_TIME)} < ${cutoff}
        AND ${sql(Columns.STATUS)} = ${Status.Consumed}
        AND ${sql(Columns.PROJECTED_HEADER_HASH)} IS NOT NULL
        AND NOT EXISTS (
          SELECT 1 FROM ${sql("pending_block_finalization_deposits")} pending
          WHERE pending.${sql("member_id")} = ${sql(tableName)}.${sql(
            Columns.ID,
          )}
        )
      RETURNING ${sql(Columns.ID)}`;
    return deleted.length;
  }).pipe(
    Effect.withLogSpan(`pruneOlderThan ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to prune old deposits"),
  );

export const clear = clearTable(tableName);
