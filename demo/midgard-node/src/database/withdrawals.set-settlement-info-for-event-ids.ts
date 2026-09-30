import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { Database } from "../services/database.js";
import { withHistoryWrite } from "../services/event-history-producer.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";
import {
  Columns,
  type Entry,
  projectedEventAdapter,
  type SettlementInfoAssignment,
  Status,
  tableName,
  validityDetailEquals,
} from "./withdrawals.insert-entries.js";

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
