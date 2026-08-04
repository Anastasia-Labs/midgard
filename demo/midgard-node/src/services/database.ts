import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { PgClient } from "@effect/sql-pg";
import { Context, Data, Duration, Effect, Layer, Redacted } from "effect";

import { ConfigError, NodeConfig, NodeConfigDep } from "@/services/config.js";

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

export const databaseConnectTimeout = (
  role: DatabasePoolRole,
): Duration.Duration => {
  switch (role) {
    case "admission":
    case "batch":
    case "worker":
      return Duration.seconds(10);
  }
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
    return Layer.mapError(pgClientLayer, (e) => {
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
  workerLayer: Layer.provide(WorkerSqlClientLive, NodeConfig.layer),
};

/**
 * Convenience alias for the SQL client service type.
 */
export type Database = SqlClient.SqlClient;
