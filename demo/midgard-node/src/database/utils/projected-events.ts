import { SqlClient, SqlError } from "@effect/sql";
import { Effect } from "effect";

import { DatabaseError } from "@/database/utils/common.js";
import { Database } from "@/services/database.js";

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
