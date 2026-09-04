import { SqlClient, SqlError } from "@effect/sql";
import { Effect } from "effect";

import { Database } from "../../services/database.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./common.js";

export type ProjectedEventRow = Record<string, unknown>;

type ProjectedEventError = DatabaseError | SqlError.SqlError;

export type ProjectedEventTable = {
  readonly tableName: string;
  readonly idColumn: string;
  readonly inclusionTimeColumn: string;
  readonly projectedHeaderHashColumn: string;
  readonly statusColumn: string;
  readonly awaitingStatus: string;
  readonly projectedStatus: string;
  readonly terminalStatus: string;
  readonly entitySingular: string;
  readonly entityPlural: string;
  readonly idLabel: string;
  readonly touchUpdatedAt?: boolean;
  readonly validateHeaderAssignment?: (
    row: ProjectedEventRow,
    idHex: string,
  ) => Effect.Effect<void, DatabaseError>;
};

const idHex = (config: ProjectedEventTable, row: ProjectedEventRow): string =>
  (row[config.idColumn] as Buffer).toString("hex");

const status = (config: ProjectedEventTable, row: ProjectedEventRow): string =>
  row[config.statusColumn] as string;

const projectedHeaderHash = (
  config: ProjectedEventTable,
  row: ProjectedEventRow,
): Buffer | null => row[config.projectedHeaderHashColumn] as Buffer | null;

const projectedLifecycleStatuses = (
  config: ProjectedEventTable,
): readonly string[] => [config.projectedStatus, config.terminalStatus];

const isProjectedLifecycleStatus = (
  config: ProjectedEventTable,
  value: string,
): boolean => projectedLifecycleStatuses(config).includes(value);

const retrieveRowsByIds = (
  config: ProjectedEventTable,
  ids: readonly Buffer[],
): Effect.Effect<readonly ProjectedEventRow[], ProjectedEventError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<ProjectedEventRow>`SELECT * FROM ${sql(config.tableName)}
      WHERE ${sql(config.idColumn)} IN ${sql.in(ids)}`;
  });

export const retrieveByProjectedHeaderHash = <Entry extends object>(
  config: ProjectedEventTable,
  headerHash: Buffer,
): Effect.Effect<readonly Entry[], ProjectedEventError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(config.tableName)}
      WHERE ${sql(config.projectedHeaderHashColumn)} = ${headerHash}
      ORDER BY ${sql(config.inclusionTimeColumn)} ASC, ${sql(
        config.idColumn,
      )} ASC`;
  });

export const retrievePendingHeaderEntriesUpTo = <Entry extends object>(
  config: ProjectedEventTable,
  endTime: Date,
  statuses: readonly string[] = [],
): Effect.Effect<readonly Entry[], ProjectedEventError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    if (statuses.length > 0) {
      return yield* sql<Entry>`SELECT * FROM ${sql(config.tableName)}
        WHERE ${sql(config.statusColumn)} IN ${sql.in(statuses)}
          AND ${sql(config.projectedHeaderHashColumn)} IS NULL
          AND ${sql(config.inclusionTimeColumn)} <= ${endTime}
        ORDER BY ${sql(config.inclusionTimeColumn)} ASC, ${sql(
          config.idColumn,
        )} ASC`;
    }
    return yield* sql<Entry>`SELECT * FROM ${sql(config.tableName)}
      WHERE ${sql(config.projectedHeaderHashColumn)} IS NULL
        AND ${sql(config.inclusionTimeColumn)} <= ${endTime}
      ORDER BY ${sql(config.inclusionTimeColumn)} ASC, ${sql(
        config.idColumn,
      )} ASC`;
  });

export const retrieveProjectedPendingHeaderEntries = <Entry extends object>(
  config: ProjectedEventTable,
  statuses: readonly string[],
): Effect.Effect<readonly Entry[], ProjectedEventError, Database> =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<Entry>`SELECT * FROM ${sql(config.tableName)}
      WHERE ${sql(config.statusColumn)} IN ${sql.in(statuses)}
        AND ${sql(config.projectedHeaderHashColumn)} IS NULL
      ORDER BY ${sql(config.inclusionTimeColumn)} ASC, ${sql(
        config.idColumn,
      )} ASC`;
  });

export const markAwaitingAsProjected = (
  config: ProjectedEventTable,
  ids: readonly Buffer[],
): Effect.Effect<void, ProjectedEventError, Database> =>
  Effect.gen(function* () {
    if (ids.length <= 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    const rows = config.touchUpdatedAt
      ? yield* sql<ProjectedEventRow>`UPDATE ${sql(config.tableName)}
          SET ${sql(config.statusColumn)} = ${config.projectedStatus},
              updated_at = NOW()
          WHERE ${sql(config.idColumn)} IN ${sql.in(ids)}
            AND ${sql(config.statusColumn)} = ${config.awaitingStatus}
            AND ${sql(config.projectedHeaderHashColumn)} IS NULL
          RETURNING *`
      : yield* sql<ProjectedEventRow>`UPDATE ${sql(config.tableName)}
          SET ${sql(config.statusColumn)} = ${config.projectedStatus}
          WHERE ${sql(config.idColumn)} IN ${sql.in(ids)}
            AND ${sql(config.statusColumn)} = ${config.awaitingStatus}
            AND ${sql(config.projectedHeaderHashColumn)} IS NULL
          RETURNING *`;
    if (rows.length === ids.length) {
      return;
    }

    const currentRows = yield* retrieveRowsByIds(config, ids);
    if (currentRows.length !== ids.length) {
      return yield* Effect.fail(
        new DatabaseError({
          table: config.tableName,
          message: `Failed to mark awaiting ${config.entityPlural} as projected because at least one row no longer exists`,
          cause: `requested=${ids.length},found=${currentRows.length}`,
        }),
      );
    }

    for (const row of currentRows) {
      const rowStatus = status(config, row);
      const rowProjectedHeaderHash = projectedHeaderHash(config, row);
      if (!isProjectedLifecycleStatus(config, rowStatus)) {
        return yield* Effect.fail(
          new DatabaseError({
            table: config.tableName,
            message: `Failed to mark awaiting ${config.entityPlural} as projected because at least one row is still not in a projected lifecycle state`,
            cause: `${config.idLabel}=${idHex(config, row)},status=${rowStatus}`,
          }),
        );
      }
      if (rowProjectedHeaderHash !== null) {
        return yield* Effect.fail(
          new DatabaseError({
            table: config.tableName,
            message: `Failed to mark awaiting ${config.entityPlural} as projected because at least one row is already assigned to a header`,
            cause: `${config.idLabel}=${idHex(
              config,
              row,
            )},projected_header_hash=${rowProjectedHeaderHash.toString("hex")}`,
          }),
        );
      }
    }
  });

export const markProjectedByEventIds = (
  config: ProjectedEventTable,
  ids: readonly Buffer[],
  headerHash: Buffer,
): Effect.Effect<void, ProjectedEventError, Database> =>
  Effect.gen(function* () {
    if (ids.length <= 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    const existing = yield* retrieveRowsByIds(config, ids);
    const existingById = new Map(
      existing.map((row) => [idHex(config, row), row] as const),
    );

    for (const id of ids) {
      const key = id.toString("hex");
      const row = existingById.get(key);
      if (row === undefined) {
        return yield* Effect.fail(
          new DatabaseError({
            table: config.tableName,
            message: `Failed to mark ${config.entitySingular} header assignment because it does not exist`,
            cause: `${config.idLabel}=${key}`,
          }),
        );
      }
      const rowStatus = status(config, row);
      if (!isProjectedLifecycleStatus(config, rowStatus)) {
        return yield* Effect.fail(
          new DatabaseError({
            table: config.tableName,
            message: `Failed to assign projected header because the ${config.entitySingular} is not in a projected lifecycle state`,
            cause: `${config.idLabel}=${key},status=${rowStatus}`,
          }),
        );
      }
      if (config.validateHeaderAssignment !== undefined) {
        yield* config.validateHeaderAssignment(row, key);
      }
      const currentProjectedHeaderHash = projectedHeaderHash(config, row);
      if (
        currentProjectedHeaderHash !== null &&
        !currentProjectedHeaderHash.equals(headerHash)
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: config.tableName,
            message: `Failed to assign projected header because the ${config.entitySingular} is already assigned to a different header`,
            cause: `${config.idLabel}=${key},existing_header_hash=${currentProjectedHeaderHash.toString(
              "hex",
            )},requested_header_hash=${headerHash.toString("hex")}`,
          }),
        );
      }
    }

    if (config.touchUpdatedAt) {
      yield* sql`UPDATE ${sql(config.tableName)}
        SET ${sql(config.projectedHeaderHashColumn)} = ${headerHash},
            updated_at = NOW()
        WHERE ${sql(config.idColumn)} IN ${sql.in(ids)}
          AND ${sql(config.projectedHeaderHashColumn)} IS NULL`;
      return;
    }
    yield* sql`UPDATE ${sql(config.tableName)}
      SET ${sql(config.projectedHeaderHashColumn)} = ${headerHash}
      WHERE ${sql(config.idColumn)} IN ${sql.in(ids)}
        AND ${sql(config.projectedHeaderHashColumn)} IS NULL`;
  });

export const clearProjectedHeaderAssignmentByEventIds = (
  config: ProjectedEventTable,
  ids: readonly Buffer[],
  headerHash: Buffer,
): Effect.Effect<void, ProjectedEventError, Database> =>
  Effect.gen(function* () {
    if (ids.length <= 0) {
      return;
    }
    const sql = yield* SqlClient.SqlClient;
    const existing = yield* retrieveRowsByIds(config, ids);
    const existingById = new Map(
      existing.map((row) => [idHex(config, row), row] as const),
    );

    for (const id of ids) {
      const key = id.toString("hex");
      const row = existingById.get(key);
      if (row === undefined) {
        return yield* Effect.fail(
          new DatabaseError({
            table: config.tableName,
            message: `Failed to clear projected header assignment because the ${config.entitySingular} does not exist`,
            cause: `${config.idLabel}=${key}`,
          }),
        );
      }
      const currentProjectedHeaderHash = projectedHeaderHash(config, row);
      if (
        currentProjectedHeaderHash !== null &&
        !currentProjectedHeaderHash.equals(headerHash)
      ) {
        return yield* Effect.fail(
          new DatabaseError({
            table: config.tableName,
            message: `Failed to clear projected header assignment because the ${config.entitySingular} is assigned to a different header`,
            cause: `${config.idLabel}=${key},existing_header_hash=${currentProjectedHeaderHash.toString(
              "hex",
            )},requested_header_hash=${headerHash.toString("hex")}`,
          }),
        );
      }
    }

    if (config.touchUpdatedAt) {
      yield* sql`UPDATE ${sql(config.tableName)}
        SET ${sql(config.projectedHeaderHashColumn)} = NULL,
            updated_at = NOW()
        WHERE ${sql(config.idColumn)} IN ${sql.in(ids)}
          AND ${sql(config.projectedHeaderHashColumn)} = ${headerHash}`;
      return;
    }
    yield* sql`UPDATE ${sql(config.tableName)}
      SET ${sql(config.projectedHeaderHashColumn)} = NULL
      WHERE ${sql(config.idColumn)} IN ${sql.in(ids)}
        AND ${sql(config.projectedHeaderHashColumn)} = ${headerHash}`;
  });

/**
 * Reopens events from an L2 block removed by authenticated state correction.
 * The exact prior header binding prevents a correction from stealing an event
 * already re-projected into a different canonical block. Repeated recovery is
 * idempotent only in the projected+unassigned state produced here.
 */
export const reopenAfterStateQueueCorrectionByEventIds = (
  config: ProjectedEventTable,
  ids: readonly Buffer[],
  removedHeaderHash: Buffer,
): Effect.Effect<void, DatabaseError, Database> =>
  Effect.gen(function* () {
    if (ids.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* retrieveRowsByIds(config, ids);
    const rowsById = new Map(
      rows.map((row) => [idHex(config, row), row] as const),
    );
    for (const id of ids) {
      const row = rowsById.get(id.toString("hex"));
      if (row === undefined) {
        return yield* Effect.fail(
          new DatabaseError({
            table: config.tableName,
            message: `Cannot reopen corrected ${config.entitySingular}: row is missing`,
            cause: `${config.idLabel}=${id.toString("hex")}`,
          }),
        );
      }
      const assignedHeader = projectedHeaderHash(config, row);
      const status = row[config.statusColumn as keyof typeof row];
      const reopenState = classifyStateQueueCorrectionEventReopen({
        assignedHeader,
        removedHeaderHash,
        status: String(status),
        projectedStatus: config.projectedStatus,
        terminalStatus: config.terminalStatus,
      });
      if (reopenState === "conflict") {
        return yield* Effect.fail(
          new DatabaseError({
            table: config.tableName,
            message: `Cannot reopen corrected ${config.entitySingular}: header/status changed`,
            cause: `${config.idLabel}=${id.toString("hex")},status=${String(status)},assigned_header=${assignedHeader?.toString("hex") ?? "none"}`,
          }),
        );
      }
    }
    yield* sql`UPDATE ${sql(config.tableName)}
      SET ${sql(config.statusColumn)} = ${config.projectedStatus},
          ${sql(config.projectedHeaderHashColumn)} = NULL
          ${config.touchUpdatedAt ? sql`, updated_at = NOW()` : sql``}
      WHERE ${sql(config.idColumn)} IN ${sql.in(ids)}
        AND ${sql(config.projectedHeaderHashColumn)} = ${removedHeaderHash}
        AND ${sql(config.statusColumn)} IN (
          ${config.projectedStatus},
          ${config.terminalStatus}
        )`;
  }).pipe(
    sqlErrorToDatabaseError(
      config.tableName,
      `Failed to reopen corrected ${config.entityPlural}`,
    ),
  );

export const classifyStateQueueCorrectionEventReopen = ({
  assignedHeader,
  removedHeaderHash,
  status,
  projectedStatus,
  terminalStatus,
}: {
  readonly assignedHeader: Buffer | null;
  readonly removedHeaderHash: Buffer;
  readonly status: string;
  readonly projectedStatus: string;
  readonly terminalStatus: string;
}): "reopen" | "already-reopened" | "conflict" => {
  if (assignedHeader === null && status === projectedStatus) {
    return "already-reopened";
  }
  if (
    assignedHeader?.equals(removedHeaderHash) === true &&
    (status === projectedStatus || status === terminalStatus)
  ) {
    return "reopen";
  }
  return "conflict";
};

export type ProjectedEventAdapterMessages = {
  readonly retrieveByProjectedHeaderHash: string;
  readonly retrievePendingHeaderEntriesUpTo: string;
  readonly retrieveProjectedPendingHeaderEntries: string;
  readonly markAwaitingAsProjected: string;
  readonly markProjectedByEventIds: string;
  readonly clearProjectedHeaderAssignmentByEventIds: string;
};

export type ProjectedEventAdapterInput = {
  readonly config: ProjectedEventTable;
  readonly pendingHeaderStatuses?: readonly string[];
  readonly projectedPendingStatuses?: readonly string[];
  readonly messages: ProjectedEventAdapterMessages;
};

export const makeProjectedEventAdapter = <Entry extends object>({
  config,
  pendingHeaderStatuses = [],
  projectedPendingStatuses = [config.projectedStatus],
  messages,
}: ProjectedEventAdapterInput) => ({
  retrieveByProjectedHeaderHash: (
    headerHash: Buffer,
  ): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
    retrieveByProjectedHeaderHash<Entry>(config, headerHash).pipe(
      Effect.withLogSpan(`retrieveByProjectedHeaderHash ${config.tableName}`),
      sqlErrorToDatabaseError(
        config.tableName,
        messages.retrieveByProjectedHeaderHash,
      ),
    ),
  retrievePendingHeaderEntriesUpTo: (
    endTime: Date,
  ): Effect.Effect<readonly Entry[], DatabaseError, Database> =>
    retrievePendingHeaderEntriesUpTo<Entry>(
      config,
      endTime,
      pendingHeaderStatuses,
    ).pipe(
      Effect.withLogSpan(
        `retrievePendingHeaderEntriesUpTo ${config.tableName}`,
      ),
      sqlErrorToDatabaseError(
        config.tableName,
        messages.retrievePendingHeaderEntriesUpTo,
      ),
    ),
  retrieveProjectedPendingHeaderEntries: (): Effect.Effect<
    readonly Entry[],
    DatabaseError,
    Database
  > =>
    retrieveProjectedPendingHeaderEntries<Entry>(
      config,
      projectedPendingStatuses,
    ).pipe(
      Effect.withLogSpan(
        `retrieveProjectedPendingHeaderEntries ${config.tableName}`,
      ),
      sqlErrorToDatabaseError(
        config.tableName,
        messages.retrieveProjectedPendingHeaderEntries,
      ),
    ),
  markAwaitingAsProjected: (
    ids: readonly Buffer[],
  ): Effect.Effect<void, DatabaseError, Database> =>
    markAwaitingAsProjected(config, ids).pipe(
      Effect.withLogSpan(`markAwaitingAsProjected ${config.tableName}`),
      sqlErrorToDatabaseError(
        config.tableName,
        messages.markAwaitingAsProjected,
      ),
    ),
  markProjectedByEventIds: (
    ids: readonly Buffer[],
    projectedHeaderHash: Buffer,
  ): Effect.Effect<void, DatabaseError, Database> =>
    markProjectedByEventIds(config, ids, projectedHeaderHash).pipe(
      Effect.withLogSpan(`markProjectedByEventIds ${config.tableName}`),
      sqlErrorToDatabaseError(
        config.tableName,
        messages.markProjectedByEventIds,
      ),
    ),
  clearProjectedHeaderAssignmentByEventIds: (
    ids: readonly Buffer[],
    projectedHeaderHash: Buffer,
  ): Effect.Effect<void, DatabaseError, Database> =>
    clearProjectedHeaderAssignmentByEventIds(
      config,
      ids,
      projectedHeaderHash,
    ).pipe(
      Effect.withLogSpan(
        `clearProjectedHeaderAssignmentByEventIds ${config.tableName}`,
      ),
      sqlErrorToDatabaseError(
        config.tableName,
        messages.clearProjectedHeaderAssignmentByEventIds,
      ),
    ),
});
