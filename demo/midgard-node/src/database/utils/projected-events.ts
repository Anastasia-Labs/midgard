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

/** Complete groups, never row pages: even the last returned instant includes every event. */
export const retrievePendingHeaderInstantCounts = (
  config: ProjectedEventTable,
  endTime: Date,
  limit: number,
  statuses: readonly string[] = [],
): Effect.Effect<
  readonly { readonly inclusionTimeMs: number; readonly count: number }[],
  ProjectedEventError,
  Database
> =>
  Effect.gen(function* () {
    if (
      !Number.isSafeInteger(limit) ||
      limit <= 0 ||
      !Number.isSafeInteger(endTime.getTime())
    )
      return yield* Effect.fail(
        new DatabaseError({
          table: config.tableName,
          message: "Invalid pending event instant query bound",
          cause: { limit, endTime },
        }),
      );
    const sql = yield* SqlClient.SqlClient;
    const rows =
      statuses.length > 0
        ? yield* sql<{
            inclusion_time: Date;
            count: string;
          }>`SELECT ${sql(config.inclusionTimeColumn)} AS inclusion_time, COUNT(*)::text AS count FROM ${sql(config.tableName)}
          WHERE ${sql(config.statusColumn)} IN ${sql.in(statuses)} AND ${sql(config.projectedHeaderHashColumn)} IS NULL AND ${sql(config.inclusionTimeColumn)} <= ${endTime}
          GROUP BY ${sql(config.inclusionTimeColumn)} ORDER BY ${sql(config.inclusionTimeColumn)} ASC LIMIT ${limit}`
        : yield* sql<{
            inclusion_time: Date;
            count: string;
          }>`SELECT ${sql(config.inclusionTimeColumn)} AS inclusion_time, COUNT(*)::text AS count FROM ${sql(config.tableName)}
          WHERE ${sql(config.projectedHeaderHashColumn)} IS NULL AND ${sql(config.inclusionTimeColumn)} <= ${endTime}
          GROUP BY ${sql(config.inclusionTimeColumn)} ORDER BY ${sql(config.inclusionTimeColumn)} ASC LIMIT ${limit}`;
    return yield* Effect.forEach(rows, (row) => {
      const inclusionTimeMs = row.inclusion_time.getTime();
      const count = Number(row.count);
      return Number.isSafeInteger(inclusionTimeMs) &&
        Number.isSafeInteger(count) &&
        count > 0
        ? Effect.succeed({ inclusionTimeMs, count })
        : Effect.fail(
            new DatabaseError({
              table: config.tableName,
              message:
                "Pending event instant count is not a safe positive integer",
              cause: row,
            }),
          );
    });
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

export type ProjectedEventAdapterMessages = {
  readonly retrieveByProjectedHeaderHash: string;
  readonly retrievePendingHeaderEntriesUpTo: string;
  readonly retrieveProjectedPendingHeaderEntries: string;
  readonly markAwaitingAsProjected: string;
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
  retrievePendingHeaderInstantCounts: (endTime: Date, limit: number) =>
    retrievePendingHeaderInstantCounts(
      config,
      endTime,
      limit,
      pendingHeaderStatuses,
    ).pipe(
      sqlErrorToDatabaseError(
        config.tableName,
        "Failed to retrieve complete pending event instant counts",
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
});
