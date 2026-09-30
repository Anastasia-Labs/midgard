import { isDeepStrictEqual } from "node:util";

import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import { Database } from "../services/database.js";
import { withHistoryIngestion } from "../services/event-history-producer.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";
import * as ProjectedEvents from "./utils/projected-events.js";
import * as UserEvents from "./utils/user-events.js";

export const tableName = "withdrawal_utxos";

export enum Columns {
  ID = UserEvents.Columns.ID,
  RAW_EVENT_INFO = "raw_event_info",
  SETTLEMENT_EVENT_INFO = "settlement_event_info",
  INCLUSION_TIME = UserEvents.Columns.INCLUSION_TIME,
  // Latest authenticated history location; admission identity is the stable ID.
  WITHDRAWAL_L1_TX_HASH = "withdrawal_l1_tx_hash",
  WITHDRAWAL_L1_OUTPUT_INDEX = "withdrawal_l1_output_index",
  ASSET_NAME = "asset_name",
  L2_OUTREF = "l2_outref",
  L2_OWNER = "l2_owner",
  L2_VALUE = "l2_value",
  L1_ADDRESS = "l1_address",
  L1_DATUM = "l1_datum",
  REFUND_ADDRESS = "refund_address",
  REFUND_DATUM = "refund_datum",
  VALIDITY = "validity",
  VALIDITY_DETAIL = "validity_detail",
  CLASSIFICATION_REVISION = "classification_revision",
  REOPENED_FROM_HEADER_HASH = "reopened_from_header_hash",
  PROJECTED_HEADER_HASH = "projected_header_hash",
  STATUS = "status",
}

export const Status = {
  Awaiting: "awaiting",
  Projected: "projected",
  Finalized: "finalized",
} as const;

export type Status = (typeof Status)[keyof typeof Status];

export const Validity = {
  WithdrawalIsValid: "WithdrawalIsValid",
  NonExistentWithdrawalUtxo: "NonExistentWithdrawalUtxo",
  SpentWithdrawalUtxo: "SpentWithdrawalUtxo",
  IncorrectWithdrawalOwner: "IncorrectWithdrawalOwner",
  IncorrectWithdrawalValue: "IncorrectWithdrawalValue",
  IncorrectWithdrawalSignature: "IncorrectWithdrawalSignature",
  TooManyTokensInWithdrawal: "TooManyTokensInWithdrawal",
  UnpayableWithdrawalValue: "UnpayableWithdrawalValue",
} as const;

export type Validity = (typeof Validity)[keyof typeof Validity];

export type Entry = {
  [Columns.ID]: Buffer;
  [Columns.RAW_EVENT_INFO]: Buffer;
  [Columns.SETTLEMENT_EVENT_INFO]: Buffer | null;
  [Columns.INCLUSION_TIME]: Date;
  [Columns.WITHDRAWAL_L1_TX_HASH]: Buffer;
  [Columns.WITHDRAWAL_L1_OUTPUT_INDEX]: number;
  [Columns.ASSET_NAME]: Buffer;
  [Columns.L2_OUTREF]: Buffer;
  [Columns.L2_OWNER]: Buffer;
  [Columns.L2_VALUE]: Buffer;
  [Columns.L1_ADDRESS]: Buffer;
  [Columns.L1_DATUM]: Buffer;
  [Columns.REFUND_ADDRESS]: Buffer;
  [Columns.REFUND_DATUM]: Buffer;
  [Columns.VALIDITY]: Validity | null;
  [Columns.VALIDITY_DETAIL]: unknown;
  [Columns.CLASSIFICATION_REVISION]: number;
  [Columns.REOPENED_FROM_HEADER_HASH]: Buffer | null;
  [Columns.PROJECTED_HEADER_HASH]: Buffer | null;
  [Columns.STATUS]: Status;
};

export type SettlementInfoAssignment = {
  readonly eventId: Buffer;
  readonly expectedClassificationRevision: number;
  readonly settlementEventInfo: Buffer;
  readonly validity: Validity;
  readonly validityDetail?: unknown;
};

const projectedEventsTable = {
  tableName,
  idColumn: Columns.ID,
  inclusionTimeColumn: Columns.INCLUSION_TIME,
  projectedHeaderHashColumn: Columns.PROJECTED_HEADER_HASH,
  statusColumn: Columns.STATUS,
  awaitingStatus: Status.Awaiting,
  projectedStatus: Status.Projected,
  terminalStatus: Status.Finalized,
  entitySingular: "withdrawal",
  entityPlural: "withdrawals",
  idLabel: "event_id",
  touchUpdatedAt: true,
  validateHeaderAssignment: (row, idHex) =>
    row[Columns.SETTLEMENT_EVENT_INFO] === null ||
    row[Columns.VALIDITY] === null
      ? Effect.fail(
          new DatabaseError({
            table: tableName,
            message:
              "Failed to assign projected header because the withdrawal has not been classified",
            cause: `event_id=${idHex}`,
          }),
        )
      : Effect.void,
} as const satisfies ProjectedEvents.ProjectedEventTable;

export const projectedEventAdapter =
  ProjectedEvents.makeProjectedEventAdapter<Entry>({
    config: projectedEventsTable,
    pendingHeaderStatuses: [Status.Awaiting, Status.Projected],
    projectedPendingStatuses: [Status.Projected],
    messages: {
      retrieveByProjectedHeaderHash:
        "Failed to retrieve withdrawals by projected header hash",
      retrievePendingHeaderEntriesUpTo:
        "Failed to retrieve withdrawals pending header assignment",
      retrieveProjectedPendingHeaderEntries:
        "Failed to retrieve projected withdrawals awaiting header assignment",
      markAwaitingAsProjected:
        "Failed to mark awaiting withdrawals as projected",
      markProjectedByEventIds:
        "Failed to mark withdrawals as assigned to the given header",
      clearProjectedHeaderAssignmentByEventIds:
        "Failed to clear projected header assignments for withdrawals",
    },
  });

export const validityDetailEquals = (left: unknown, right: unknown): boolean =>
  isDeepStrictEqual(left ?? {}, right ?? {});

const sameImmutablePayload = (left: Entry, right: Entry): boolean =>
  left[Columns.ID].equals(right[Columns.ID]) &&
  left[Columns.RAW_EVENT_INFO].equals(right[Columns.RAW_EVENT_INFO]) &&
  left[Columns.INCLUSION_TIME].getTime() ===
    right[Columns.INCLUSION_TIME].getTime() &&
  left[Columns.WITHDRAWAL_L1_TX_HASH].equals(
    right[Columns.WITHDRAWAL_L1_TX_HASH],
  ) &&
  left[Columns.WITHDRAWAL_L1_OUTPUT_INDEX] ===
    right[Columns.WITHDRAWAL_L1_OUTPUT_INDEX] &&
  left[Columns.ASSET_NAME].equals(right[Columns.ASSET_NAME]) &&
  left[Columns.L2_OUTREF].equals(right[Columns.L2_OUTREF]) &&
  left[Columns.L2_OWNER].equals(right[Columns.L2_OWNER]) &&
  left[Columns.L2_VALUE].equals(right[Columns.L2_VALUE]) &&
  left[Columns.L1_ADDRESS].equals(right[Columns.L1_ADDRESS]) &&
  left[Columns.L1_DATUM].equals(right[Columns.L1_DATUM]) &&
  left[Columns.REFUND_ADDRESS].equals(right[Columns.REFUND_ADDRESS]) &&
  left[Columns.REFUND_DATUM].equals(right[Columns.REFUND_DATUM]);

export const insertEntries = (
  entries: readonly Entry[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (entries.length <= 0) {
      return;
    }
    const incomingById = new Map<string, Entry>();
    for (const incoming of entries) {
      const key = incoming[Columns.ID].toString("hex");
      const existingIncoming = incomingById.get(key);
      if (
        existingIncoming !== undefined &&
        !sameImmutablePayload(existingIncoming, incoming)
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: tableName,
            message:
              "Refusing to insert withdrawals because the same event_id appears with conflicting payloads in one batch",
            cause: `event_id=${key}`,
          }),
        );
      }
      incomingById.set(key, incoming);
    }

    const sql = yield* SqlClient.SqlClient;
    const insertColumns = [
      Columns.ID,
      Columns.RAW_EVENT_INFO,
      Columns.SETTLEMENT_EVENT_INFO,
      Columns.INCLUSION_TIME,
      Columns.WITHDRAWAL_L1_TX_HASH,
      Columns.WITHDRAWAL_L1_OUTPUT_INDEX,
      Columns.ASSET_NAME,
      Columns.L2_OUTREF,
      Columns.L2_OWNER,
      Columns.L2_VALUE,
      Columns.L1_ADDRESS,
      Columns.L1_DATUM,
      Columns.REFUND_ADDRESS,
      Columns.REFUND_DATUM,
      Columns.VALIDITY,
      Columns.VALIDITY_DETAIL,
      Columns.PROJECTED_HEADER_HASH,
      Columns.STATUS,
    ];
    // Bind serialized JSON as text before casting; a JSONB-typed parameter
    // makes postgres.js encode the string a second time. Render rows explicitly
    // because sql.insert only preserves parameter/custom fragments, not casts.
    const normalizedEntries = [...incomingById.values()].map((entry) => ({
      ...entry,
      [Columns.VALIDITY_DETAIL]: sql`CAST(${JSON.stringify(
        entry[Columns.VALIDITY_DETAIL] ?? {},
      )} AS TEXT)::JSONB`,
    }));
    // Ingestion may observe a continuation at a new output. Refresh only that
    // location: projection, settlement classification and event content survive.
    // Reject the entire batch if any immutable payload differs.
    yield* sql.withTransaction(
      Effect.gen(function* () {
        const rows = yield* sql<{ [Columns.ID]: Buffer }>`
          INSERT INTO ${sql(tableName)} (${sql.csv(insertColumns.map((column) => sql`${sql(column)}`))})
          VALUES ${sql.csv(normalizedEntries.map((entry) => sql`(${sql.csv(insertColumns.map((column) => sql`${entry[column]}`))})`))}
          ON CONFLICT (${sql(Columns.ID)}) DO UPDATE SET
            ${sql(Columns.WITHDRAWAL_L1_TX_HASH)} = EXCLUDED.${sql(Columns.WITHDRAWAL_L1_TX_HASH)},
            ${sql(Columns.WITHDRAWAL_L1_OUTPUT_INDEX)} = EXCLUDED.${sql(Columns.WITHDRAWAL_L1_OUTPUT_INDEX)},
            updated_at = NOW()
          WHERE ${sql(tableName)}.${sql(Columns.RAW_EVENT_INFO)} = EXCLUDED.${sql(
            Columns.RAW_EVENT_INFO,
          )}
            AND ${sql(tableName)}.${sql(Columns.INCLUSION_TIME)} = EXCLUDED.${sql(
              Columns.INCLUSION_TIME,
            )}
            AND ${sql(tableName)}.${sql(Columns.ASSET_NAME)} = EXCLUDED.${sql(
              Columns.ASSET_NAME,
            )}
            AND ${sql(tableName)}.${sql(Columns.L2_OUTREF)} = EXCLUDED.${sql(
              Columns.L2_OUTREF,
            )}
            AND ${sql(tableName)}.${sql(Columns.L2_OWNER)} = EXCLUDED.${sql(
              Columns.L2_OWNER,
            )}
            AND ${sql(tableName)}.${sql(Columns.L2_VALUE)} = EXCLUDED.${sql(
              Columns.L2_VALUE,
            )}
            AND ${sql(tableName)}.${sql(Columns.L1_ADDRESS)} = EXCLUDED.${sql(
              Columns.L1_ADDRESS,
            )}
            AND ${sql(tableName)}.${sql(Columns.L1_DATUM)} = EXCLUDED.${sql(
              Columns.L1_DATUM,
            )}
            AND ${sql(tableName)}.${sql(Columns.REFUND_ADDRESS)} = EXCLUDED.${sql(
              Columns.REFUND_ADDRESS,
            )}
            AND ${sql(tableName)}.${sql(Columns.REFUND_DATUM)} = EXCLUDED.${sql(
              Columns.REFUND_DATUM,
            )}
          RETURNING ${sql(Columns.ID)}
        `;
        if (rows.length !== normalizedEntries.length) {
          return yield* Effect.fail(
            new DatabaseError({
              table: tableName,
              message:
                "Refusing to upsert withdrawal because the same event_id has conflicting persisted payload",
              cause: `requested=${normalizedEntries.length},upserted=${rows.length}`,
            }),
          );
        }
      }),
    );
  }).pipe(
    withHistoryIngestion,
    Effect.withLogSpan(`insertEntries ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to insert withdrawal UTxOs"),
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
    sqlErrorToDatabaseError(tableName, "Failed to retrieve withdrawals"),
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
      "Failed to retrieve withdrawal by event id",
    ),
  );
