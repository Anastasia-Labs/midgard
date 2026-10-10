import { formatUnknownError } from "@al-ft/midgard-core/error-format";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import {
  Context,
  Data,
  Duration,
  Effect,
  Layer,
  Redacted,
  Schedule,
} from "effect";

import { isConnectionClassError } from "../provider-retry.js";
import { ConfigError, NodeConfig, NodeConfigDep } from "./config.js";
import { databaseUpstreamSocket } from "./database-upstream-socket.js";
import {
  DATABASE_CONNECTION_FAILED,
  DATABASE_UNREACHABLE,
  reportStartupWaiting,
  STARTUP_DATABASE_BUDGET,
  startupStepFailed,
  type StartupStepFailedError,
} from "./startup-waiting.js";

/**
 * Database service wiring for the Midgard node.
 */
export class DatabaseInitializationError extends Data.TaggedError(
  "DatabaseInitializationError",
)<SDK.GenericErrorFields> {}

export class AdmissionSql extends Context.Tag("AdmissionSql")<
  AdmissionSql,
  SqlClient.SqlClient
>() {}

export class BatchSql extends Context.Tag("BatchSql")<
  BatchSql,
  SqlClient.SqlClient
>() {}

export type DatabasePoolRole = "admission" | "batch" | "worker";

/** How long a new database connection may take to establish. */
export const DATABASE_CONNECT_TIMEOUT = Duration.seconds(10);

export const databaseConnectTimeout = (
  role: DatabasePoolRole,
): Duration.Duration => {
  switch (role) {
    case "admission":
    case "batch":
    case "worker":
      return DATABASE_CONNECT_TIMEOUT;
  }
};

export type DatabaseStartupRetryOptions = {
  readonly baseDelay: Duration.DurationInput;
  readonly maxDelay: Duration.DurationInput;
  /** How long a pool waits for PostgreSQL to take connections. */
  readonly budget: Duration.DurationInput;
};

export const DATABASE_STARTUP_RETRY: DatabaseStartupRetryOptions = {
  baseDelay: Duration.millis(500),
  maxDelay: Duration.seconds(5),
  budget: STARTUP_DATABASE_BUDGET,
};

/**
 * Rebuilds `layer` while it fails because PostgreSQL cannot be reached or
 * will not yet take a connection (restarting, in recovery, out of slots:
 * `isConnectionClassError`), for at most the budget (15 min by default),
 * logging the reason and reporting `database_unreachable` to the node's
 * startup (`reportStartupWaiting`) until the pool opens. A failure past the
 * budget fails the pool under `database_unreachable`; any other failure
 * (bad credentials, a missing database) fails it at once under
 * `database_connection_failed`. Both as a `StartupStepFailedError` naming
 * the pool, the reason and the last cause.
 */
export const retryDatabaseConnectionAtStartup = <A, E, R>(
  layer: Layer.Layer<A, E, R>,
  role: DatabasePoolRole,
  options: DatabaseStartupRetryOptions = DATABASE_STARTUP_RETRY,
): Layer.Layer<A, StartupStepFailedError, R> => {
  const key = `database_pool:${role}`;
  let attempts = 0;
  return Layer.retry(
    layer,
    Schedule.exponential(options.baseDelay).pipe(
      Schedule.union(Schedule.spaced(options.maxDelay)),
      Schedule.whileInput((error: E) => isConnectionClassError(error)),
      Schedule.upTo(options.budget),
      Schedule.tapInput((error: E) => {
        attempts += 1;
        return isConnectionClassError(error)
          ? Effect.zipRight(
              reportStartupWaiting(key, [DATABASE_UNREACHABLE]),
              Effect.logWarning(
                `Database unready: reason=${DATABASE_UNREACHABLE}; the ${role} pool waits and reconnects. cause=${formatUnknownError(error, { includeCause: true })}`,
              ),
            )
          : Effect.void;
      }),
    ),
  ).pipe(
    Layer.tap(() => reportStartupWaiting(key, [])),
    Layer.tapError(() => reportStartupWaiting(key, [])),
    Layer.mapError((error) => {
      const transient = isConnectionClassError(error);
      return startupStepFailed({
        step: key,
        reason: transient ? DATABASE_UNREACHABLE : DATABASE_CONNECTION_FAILED,
        cause: error,
        exhausted: transient,
        attempts: Math.max(1, attempts),
      });
    }),
  );
};

/**
 * Builds the PostgreSQL client layer from the decoded node configuration.
 */
const createPgLayerEffect = (
  role: DatabasePoolRole,
  resolveMaxConnections: (config: NodeConfigDep) => number,
) =>
  Effect.gen(function* () {
    const nodeConfig = yield* NodeConfig;
    const maxConnections = resolveMaxConnections(nodeConfig);
    yield* Effect.logInfo(
      `📚 Opening ${role} database pool (max_connections=${maxConnections.toString()})...`,
    );
    const pgClientLayer = PgClient.layer({
      host: nodeConfig.POSTGRES_HOST,
      port: nodeConfig.POSTGRES_PORT,
      // Built once per pool, outside the startup retry, so an upstream that
      // drops every connection is logged once, not on every attempt.
      socket: yield* databaseUpstreamSocket(
        role,
        nodeConfig.POSTGRES_HOST,
        nodeConfig.POSTGRES_PORT,
      ),
      username: nodeConfig.POSTGRES_USER,
      password: Redacted.make(nodeConfig.POSTGRES_PASSWORD),
      database: nodeConfig.POSTGRES_DB,
      maxConnections,
      applicationName: `midgard-node-${role}`,
      idleTimeout: Duration.minutes(5),
      // postgres.js opens pool connections lazily during steady-state traffic,
      // so every role needs enough establishment headroom under load. This
      // does not change statement, request, or endpoint latency timeouts.
      connectTimeout: databaseConnectTimeout(role),
    });
    const mappedLayer = Layer.mapError(pgClientLayer, (e) => {
      switch (e._tag) {
        case "ConfigError":
          return new ConfigError({
            message: "Improper config file provided",
            cause: e,
            fieldsAndValues: [
              ["POSTGRES_HOST", nodeConfig.POSTGRES_HOST],
              ["POSTGRES_PORT", nodeConfig.POSTGRES_PORT.toString()],
              ["POSTGRES_USER", nodeConfig.POSTGRES_USER],
              ["POSTGRES_DB", nodeConfig.POSTGRES_DB],
              ["POSTGRES_POOL_ROLE", role],
              ["POSTGRES_POOL_SIZE", maxConnections.toString()],
            ],
          });
        case "SqlError":
          return new DatabaseInitializationError({
            message: `Failed to initialize the ${role} database pool`,
            cause: e,
          });
      }
    });
    // A configuration error stays one; a pool that never opened fails as a
    // `DatabaseInitializationError` over the step's named failure.
    return retryDatabaseConnectionAtStartup(mappedLayer, role).pipe(
      Layer.mapError((failure) =>
        failure.cause instanceof ConfigError
          ? failure.cause
          : new DatabaseInitializationError({
              message: failure.message,
              cause: failure,
            }),
      ),
    );
  }).pipe(Effect.orDie);

/**
 * Live SQL client layer backed by PostgreSQL.
 */
const BatchSqlClientLive: Layer.Layer<
  SqlClient.SqlClient,
  DatabaseInitializationError | ConfigError,
  NodeConfig
> = Layer.unwrapEffect(
  createPgLayerEffect("batch", (config) => config.POSTGRES_BATCH_POOL_SIZE),
);

const AdmissionSqlClientLive: Layer.Layer<
  SqlClient.SqlClient,
  DatabaseInitializationError | ConfigError,
  NodeConfig
> = Layer.unwrapEffect(
  createPgLayerEffect(
    "admission",
    (config) => config.POSTGRES_ADMISSION_POOL_SIZE,
  ),
);

const WorkerSqlClientLive: Layer.Layer<
  SqlClient.SqlClient,
  DatabaseInitializationError | ConfigError,
  NodeConfig
> = Layer.unwrapEffect(
  createPgLayerEffect("worker", (config) => config.POSTGRES_WORKER_POOL_SIZE),
);

const BatchSqlAliasLive = Layer.effect(BatchSql, SqlClient.SqlClient);
const BatchDatabaseLive = Layer.provideMerge(
  BatchSqlAliasLive,
  BatchSqlClientLive,
);

const AdmissionSqlLive = Layer.effect(AdmissionSql, SqlClient.SqlClient).pipe(
  Layer.provide(AdmissionSqlClientLive),
);

export const admissionAsDefaultSqlLayer = Layer.effect(
  SqlClient.SqlClient,
  AdmissionSql,
);

const DatabaseLive = Layer.merge(BatchDatabaseLive, AdmissionSqlLive);

/** Public database service bundle used throughout the node. */
export const Database = {
  layer: DatabaseLive.pipe(Layer.provide(NodeConfig.layer)),
  /** Exposes the same decoded NodeConfig used to construct both SQL pools. */
  layerWithNodeConfig: Layer.provideMerge(DatabaseLive, NodeConfig.layer),
  workerLayer: WorkerSqlClientLive.pipe(Layer.provide(NodeConfig.layer)),
};

/**
 * Convenience alias for the SQL client service type.
 */
export type Database = SqlClient.SqlClient;
