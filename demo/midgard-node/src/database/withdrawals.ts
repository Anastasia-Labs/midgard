import { isDeepStrictEqual } from "node:util";

import { outRefToCbor } from "@al-ft/lucid-midgard";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect, Option } from "effect";

import { Database } from "../services/database.js";
import {
  withHistoryIngestion,
  withHistoryWrite,
} from "../services/event-history-producer.js";
import {
  clearTable,
  DatabaseError,
  sqlErrorToDatabaseError,
} from "./utils/common.js";
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

const projectedEventAdapter = ProjectedEvents.makeProjectedEventAdapter<Entry>({
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
    markAwaitingAsProjected: "Failed to mark awaiting withdrawals as projected",
    markProjectedByEventIds:
      "Failed to mark withdrawals as assigned to the given header",
    clearProjectedHeaderAssignmentByEventIds:
      "Failed to clear projected header assignments for withdrawals",
  },
});

const validityDetailEquals = (left: unknown, right: unknown): boolean =>
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

export const retrieveByEventIds = (
  eventIds: readonly Buffer[],
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    if (eventIds.length <= 0) {
      return [];
    }
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.ID)} IN ${sql.in(eventIds)}
      ORDER BY ${sql(Columns.INCLUSION_TIME)} ASC, ${sql(Columns.ID)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveByEventIds ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve withdrawals by event ids",
    ),
  );

export const retrieveByCardanoTxHash = (
  cardanoTxHash: Buffer,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
      WHERE ${sql(Columns.WITHDRAWAL_L1_TX_HASH)} = ${cardanoTxHash}
      ORDER BY ${sql(Columns.INCLUSION_TIME)} ASC, ${sql(Columns.ID)} ASC`;
  }).pipe(
    Effect.withLogSpan(`retrieveByCardanoTxHash ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to retrieve withdrawals by Cardano tx hash",
    ),
  );

export const retrieveByProjectedHeaderHash = (
  projectedHeaderHash: Buffer,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  projectedEventAdapter.retrieveByProjectedHeaderHash(projectedHeaderHash);

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
      "Failed to retrieve awaiting withdrawals due by the requested time",
    ),
  );

export const retrievePendingHeaderEntriesUpTo = (
  endTime: Date,
): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
  projectedEventAdapter.retrievePendingHeaderEntriesUpTo(endTime);

export const retrieveProjectedPendingHeaderEntries = (): Effect.Effect<
  readonly Entry[],
  DatabaseError,
  Database
> => projectedEventAdapter.retrieveProjectedPendingHeaderEntries();

export const markAwaitingAsProjected = (
  assignments: readonly Pick<
    SettlementInfoAssignment,
    "eventId" | "expectedClassificationRevision"
  >[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        for (const assignment of [...assignments].sort((a, b) =>
          Buffer.compare(a.eventId, b.eventId),
        )) {
          const [row] =
            yield* sql<Entry>`SELECT * FROM ${sql(tableName)} WHERE ${sql(Columns.ID)} = ${assignment.eventId} FOR UPDATE`;
          if (
            !row ||
            row[Columns.CLASSIFICATION_REVISION] !==
              assignment.expectedClassificationRevision
          ) {
            return yield* Effect.fail(
              new DatabaseError({
                table: tableName,
                message: "Refusing stale withdrawal projection",
                cause: assignment.eventId.toString("hex"),
              }),
            );
          }
        }
        yield* projectedEventAdapter.markAwaitingAsProjected(
          assignments.map((assignment) => assignment.eventId),
        );
      }),
    );
  }).pipe(
    withHistoryWrite,
    sqlErrorToDatabaseError(tableName, "Failed to project withdrawals"),
  );

export const assertClassificationSnapshots = (
  assignments: readonly SettlementInfoAssignment[],
  allowedHeader: Buffer | null = null,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    for (const assignment of [...assignments].sort((a, b) =>
      Buffer.compare(a.eventId, b.eventId),
    )) {
      const [row] =
        yield* sql<Entry>`SELECT * FROM ${sql(tableName)} WHERE ${sql(Columns.ID)} = ${assignment.eventId} FOR UPDATE`;
      const header = row?.[Columns.PROJECTED_HEADER_HASH];
      if (
        !row ||
        (row[Columns.CLASSIFICATION_REVISION] !==
          assignment.expectedClassificationRevision &&
          (allowedHeader === null || !header?.equals(allowedHeader))) ||
        !row[Columns.SETTLEMENT_EVENT_INFO]?.equals(
          assignment.settlementEventInfo,
        ) ||
        row[Columns.VALIDITY] !== assignment.validity ||
        !validityDetailEquals(
          row[Columns.VALIDITY_DETAIL],
          assignment.validityDetail,
        ) ||
        (header !== null &&
          (allowedHeader === null || !header?.equals(allowedHeader)))
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: tableName,
            message: "Refusing stale withdrawal classification snapshot",
            cause: assignment.eventId.toString("hex"),
          }),
        );
      }
    }
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to validate withdrawal snapshots",
    ),
  );

export const setSettlementInfoForEventIds = (
  assignments: readonly SettlementInfoAssignment[],
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (assignments.length <= 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.forEach(
        [...assignments].sort((a, b) => Buffer.compare(a.eventId, b.eventId)),
        (assignment) =>
          Effect.gen(function* () {
            const rows = yield* sql<Entry>`SELECT * FROM ${sql(tableName)}
              WHERE ${sql(Columns.ID)} = ${assignment.eventId}
              LIMIT 1 FOR UPDATE`;
            const current = rows[0];
            if (current === undefined) {
              return yield* Effect.fail(
                new DatabaseError({
                  table: tableName,
                  message:
                    "Failed to set withdrawal settlement info because the row does not exist",
                  cause: `event_id=${assignment.eventId.toString("hex")}`,
                }),
              );
            }
            if (
              current[Columns.CLASSIFICATION_REVISION] !==
                assignment.expectedClassificationRevision ||
              current[Columns.PROJECTED_HEADER_HASH] !== null ||
              current[Columns.STATUS] === Status.Finalized
            ) {
              return yield* Effect.fail(
                new DatabaseError({
                  table: tableName,
                  message:
                    "Refusing stale withdrawal classification or classification of an assigned withdrawal",
                  cause: assignment.eventId.toString("hex"),
                }),
              );
            }
            const existingInfo = current[Columns.SETTLEMENT_EVENT_INFO];
            if (
              existingInfo !== null &&
              !existingInfo.equals(assignment.settlementEventInfo)
            ) {
              return yield* Effect.fail(
                new DatabaseError({
                  table: tableName,
                  message:
                    "Failed to set withdrawal settlement info because the row already has conflicting settlement info",
                  cause: `event_id=${assignment.eventId.toString("hex")}`,
                }),
              );
            }
            const existingValidity = current[Columns.VALIDITY];
            if (
              existingValidity !== null &&
              existingValidity !== assignment.validity
            ) {
              return yield* Effect.fail(
                new DatabaseError({
                  table: tableName,
                  message:
                    "Failed to set withdrawal settlement info because the row already has conflicting validity",
                  cause: `event_id=${assignment.eventId.toString("hex")},existing=${existingValidity},requested=${assignment.validity}`,
                }),
              );
            }
            if (
              existingInfo !== null &&
              existingValidity !== null &&
              !validityDetailEquals(
                current[Columns.VALIDITY_DETAIL],
                assignment.validityDetail ?? {},
              )
            ) {
              return yield* Effect.fail(
                new DatabaseError({
                  table: tableName,
                  message:
                    "Failed to set withdrawal settlement info because the row already has conflicting validity detail",
                  cause: `event_id=${assignment.eventId.toString("hex")}`,
                }),
              );
            }

            yield* sql`UPDATE ${sql(tableName)}
              SET ${sql(Columns.SETTLEMENT_EVENT_INFO)} = ${assignment.settlementEventInfo},
                  ${sql(Columns.VALIDITY)} = ${assignment.validity},
                  ${sql(Columns.VALIDITY_DETAIL)} = CAST(${JSON.stringify(
                    assignment.validityDetail ?? {},
                  )} AS TEXT)::JSONB,
                  updated_at = NOW()
              WHERE ${sql(Columns.ID)} = ${assignment.eventId}`;
          }),
        { discard: true },
      ),
    );
  }).pipe(
    withHistoryWrite,
    Effect.withLogSpan(`setSettlementInfoForEventIds ${tableName}`),
    sqlErrorToDatabaseError(
      tableName,
      "Failed to set withdrawal settlement event info",
    ),
  );

export const markProjectedByEventIds = (
  assignments: readonly SettlementInfoAssignment[],
  projectedHeaderHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.gen(function* () {
        yield* assertClassificationSnapshots(assignments, projectedHeaderHash);
        yield* projectedEventAdapter.markProjectedByEventIds(
          assignments.map((assignment) => assignment.eventId),
          projectedHeaderHash,
        );
      }),
    );
  }).pipe(
    withHistoryWrite,
    sqlErrorToDatabaseError(tableName, "Failed to assign withdrawal header"),
  );

export const clearProjectedHeaderAssignmentByEventIds = (
  ids: readonly Buffer[],
  projectedHeaderHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  projectedEventAdapter
    .clearProjectedHeaderAssignmentByEventIds(ids, projectedHeaderHash)
    .pipe(withHistoryWrite);

export const reopenAfterStateQueueCorrectionByEventIds = (
  ids: readonly Buffer[],
  removedHeaderHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.forEach(
        [...ids].sort(Buffer.compare),
        (id) =>
          Effect.gen(function* () {
            const [current] =
              yield* sql<Entry>`SELECT * FROM ${sql(tableName)} WHERE ${sql(Columns.ID)} = ${id} FOR UPDATE`;
            if (
              !current?.[Columns.PROJECTED_HEADER_HASH]?.equals(
                removedHeaderHash,
              )
            ) {
              return yield* Effect.fail(
                new DatabaseError({
                  table: tableName,
                  message:
                    "Cannot reopen withdrawal not assigned to the corrected header",
                  cause: id.toString("hex"),
                }),
              );
            }
            yield* sql`UPDATE ${sql(tableName)} SET
        ${sql(Columns.SETTLEMENT_EVENT_INFO)} = NULL,
        ${sql(Columns.VALIDITY)} = NULL,
        ${sql(Columns.VALIDITY_DETAIL)} = '{}'::jsonb,
        ${sql(Columns.STATUS)} = ${Status.Awaiting},
        ${sql(Columns.PROJECTED_HEADER_HASH)} = NULL,
        ${sql(Columns.REOPENED_FROM_HEADER_HASH)} = ${removedHeaderHash},
        ${sql(Columns.CLASSIFICATION_REVISION)} = ${sql(Columns.CLASSIFICATION_REVISION)} + 1,
        updated_at = NOW()
        WHERE ${sql(Columns.ID)} = ${id}`;
          }),
        { discard: true },
      ),
    );
  }).pipe(
    withHistoryWrite,
    sqlErrorToDatabaseError(
      tableName,
      "Failed to reopen corrected withdrawals",
    ),
  );

/** Only a validated, locked correction journal may supply these snapshots. */
export const restoreCorrectedClassification = (
  assignments: readonly Omit<
    SettlementInfoAssignment,
    "expectedClassificationRevision"
  >[],
  headerHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* sql.withTransaction(
      Effect.forEach(
        [...assignments].sort((a, b) => Buffer.compare(a.eventId, b.eventId)),
        (assignment) =>
          Effect.gen(function* () {
            const [current] =
              yield* sql<Entry>`SELECT * FROM ${sql(tableName)} WHERE ${sql(Columns.ID)} = ${assignment.eventId} FOR UPDATE`;
            if (
              !current ||
              current[Columns.PROJECTED_HEADER_HASH] !== null ||
              !current[Columns.REOPENED_FROM_HEADER_HASH]?.equals(headerHash)
            ) {
              return yield* Effect.fail(
                new DatabaseError({
                  table: tableName,
                  message:
                    "Cannot restore corrected withdrawal after conflicting header assignment or correction",
                  cause: assignment.eventId.toString("hex"),
                }),
              );
            }
            yield* sql`UPDATE ${sql(tableName)} SET
        ${sql(Columns.SETTLEMENT_EVENT_INFO)} = ${assignment.settlementEventInfo},
        ${sql(Columns.VALIDITY)} = ${assignment.validity},
        ${sql(Columns.VALIDITY_DETAIL)} = CAST(${JSON.stringify(assignment.validityDetail ?? {})} AS TEXT)::JSONB,
        ${sql(Columns.STATUS)} = ${Status.Projected},
        ${sql(Columns.PROJECTED_HEADER_HASH)} = ${headerHash},
        ${sql(Columns.REOPENED_FROM_HEADER_HASH)} = NULL,
        ${sql(Columns.CLASSIFICATION_REVISION)} = ${sql(Columns.CLASSIFICATION_REVISION)} + 1,
        updated_at = NOW()
        WHERE ${sql(Columns.ID)} = ${assignment.eventId}`;
          }),
        { discard: true },
      ),
    );
  }).pipe(
    withHistoryWrite,
    sqlErrorToDatabaseError(
      tableName,
      "Failed to restore corrected withdrawal classification",
    ),
  );

export const markFinalizedByEventIds = (
  ids: readonly Buffer[],
  projectedHeaderHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (ids.length <= 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ [Columns.ID]: Buffer }>`UPDATE ${sql(tableName)}
      SET ${sql(Columns.STATUS)} = ${Status.Finalized},
          ${sql(Columns.PROJECTED_HEADER_HASH)} = ${projectedHeaderHash},
          updated_at = NOW()
      WHERE ${sql(Columns.ID)} IN ${sql.in(ids)}
        AND ${sql(Columns.STATUS)} IN (${Status.Projected}, ${Status.Finalized})
        AND ${sql(Columns.PROJECTED_HEADER_HASH)} = ${projectedHeaderHash}
      RETURNING ${sql(Columns.ID)}`;
    if (rows.length !== ids.length) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Failed to finalize withdrawals because at least one row is missing, unprojected, or assigned to a different header",
          cause: `requested=${ids.length},finalized=${rows.length},header_hash=${projectedHeaderHash.toString("hex")}`,
        }),
      );
    }
  }).pipe(
    withHistoryWrite,
    Effect.withLogSpan(`markFinalizedByEventIds ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to mark withdrawals finalized"),
  );

export const toRootKeyValue = (
  entry: Entry,
): Effect.Effect<
  { readonly key: Buffer; readonly value: Buffer },
  DatabaseError,
  never
> =>
  Effect.gen(function* () {
    const settlementEventInfo = entry[Columns.SETTLEMENT_EVENT_INFO];
    if (settlementEventInfo === null) {
      return yield* Effect.fail(
        new DatabaseError({
          table: tableName,
          message:
            "Failed to convert withdrawal to root key/value because it has not been classified",
          cause: `event_id=${entry[Columns.ID].toString("hex")}`,
        }),
      );
    }
    return {
      key: Buffer.from(entry[Columns.ID]),
      value: Buffer.from(settlementEventInfo),
    };
  });

export const toLedgerOutRef = (
  entry: Entry,
): Effect.Effect<Buffer, DatabaseError, never> =>
  Effect.try({
    try: () => {
      const outRef = LucidData.from(
        entry[Columns.L2_OUTREF].toString("hex"),
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
        message: "Failed to convert withdrawal l2_outref into ledger outref",
        cause,
      }),
  });

export const pruneOlderThan = (
  cutoff: Date,
): Effect.Effect<number, DatabaseError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const deleted = yield* sql<{ [Columns.ID]: Buffer }>`DELETE FROM ${sql(
      tableName,
    )}
      WHERE ${sql(Columns.INCLUSION_TIME)} < ${cutoff}
        AND ${sql(Columns.STATUS)} = ${Status.Finalized}
        AND ${sql(Columns.PROJECTED_HEADER_HASH)} IS NOT NULL
        AND NOT EXISTS (
          SELECT 1 FROM ${sql("pending_block_finalization_withdrawals")} pending
          WHERE pending.${sql("member_id")} = ${sql(tableName)}.${sql(
            Columns.ID,
          )}
        )
      RETURNING ${sql(Columns.ID)}`;
    return deleted.length;
  }).pipe(
    withHistoryWrite,
    Effect.withLogSpan(`pruneOlderThan ${tableName}`),
    sqlErrorToDatabaseError(tableName, "Failed to prune old withdrawals"),
  );

export const clear = clearTable(tableName);
