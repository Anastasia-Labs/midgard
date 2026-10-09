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

import { followerCoveredTipSlot } from "../database/follower-events.covered-tip-slot.js";
import { installL1FollowerTipReader } from "../l1-heads.js";
import { isConnectionClassError } from "../provider-retry.js";
import { ConfigError, NodeConfig, NodeConfigDep } from "./config.js";
import { databaseUpstreamSocket } from "./database-upstream-socket.js";
import {
  DATABASE_UNREACHABLE,
  reportStartupWaiting,
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
};

export const DATABASE_STARTUP_RETRY: DatabaseStartupRetryOptions = {
  baseDelay: Duration.millis(500),
  maxDelay: Duration.seconds(5),
};

/**
 * Rebuilds `layer` while it fails because PostgreSQL cannot be reached or
 * will not yet take a connection (restarting, in recovery, out of slots),
 * with no deadline: it logs the unready reason and reports
 * `database_unreachable` to the node's startup (`reportStartupWaiting`)
 * until the pool opens. Any other failure (bad credentials, a missing
 * database, a configuration error) fails at once.
 */
export const retryDatabaseConnectionAtStartup = <A, E, R>(
  layer: Layer.Layer<A, E, R>,
  role: DatabasePoolRole,
  options: DatabaseStartupRetryOptions = DATABASE_STARTUP_RETRY,
): Layer.Layer<A, E, R> => {
  const key = `database_pool:${role}`;
  return Layer.retry(
    layer,
    Schedule.exponential(options.baseDelay).pipe(
      Schedule.union(Schedule.spaced(options.maxDelay)),
      Schedule.whileInput((error: E) => isConnectionClassError(error)),
      Schedule.tapInput((error: E) =>
        isConnectionClassError(error)
          ? Effect.zipRight(
              reportStartupWaiting(key, [DATABASE_UNREACHABLE]),
              Effect.logWarning(
                `Database unready: reason=${DATABASE_UNREACHABLE}; the ${role} pool waits and reconnects. cause=${formatUnknownError(error, { includeCause: true })}`,
              ),
            )
          : Effect.void,
      ),
    ),
  ).pipe(Layer.tap(() => reportStartupWaiting(key, [])));
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
    return retryDatabaseConnectionAtStartup(mappedLayer, role);
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

/** Installs this process's reader of the follower's covered tip, the node's
 * L1 tip (`l1-heads.ts`), over the pool it is built on. */
const FollowerTipReaderLive = Layer.effectDiscard(
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    installL1FollowerTipReader(() =>
      followerCoveredTipSlot.pipe(
        Effect.provideService(SqlClient.SqlClient, sql),
      ),
    );
  }),
);

const BatchSqlAliasLive = Layer.effect(BatchSql, SqlClient.SqlClient);
const BatchDatabaseLive = Layer.provideMerge(
  Layer.merge(BatchSqlAliasLive, FollowerTipReaderLive),
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
  workerLayer: Layer.provideMerge(
    FollowerTipReaderLive,
    WorkerSqlClientLive,
  ).pipe(Layer.provide(NodeConfig.layer)),
};

/**
 * Convenience alias for the SQL client service type.
 */
export type Database = SqlClient.SqlClient;
