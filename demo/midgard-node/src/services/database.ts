import { Data, Duration, Effect, Layer, Redacted } from "effect";
import { PgClient } from "@effect/sql-pg";
import { SqlClient } from "@effect/sql";
import { ConfigError, NodeConfig } from "@/services/config.js";
import * as SDK from "@al-ft/midgard-sdk";

/**
 * Database service wiring for the Midgard node.
 */
export class DatabaseInitializationError extends Data.TaggedError(
  "DatabaseInitializationError",
)<SDK.GenericErrorFields> {}

/**
 * Builds the PostgreSQL client layer from the decoded node configuration.
 */
const createPgLayerEffect = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  yield* Effect.logInfo("📚 Opening connection to db...");
  const pgClientLayer = PgClient.layer({
    host: nodeConfig.POSTGRES_HOST,
    port: nodeConfig.POSTGRES_PORT,
    username: nodeConfig.POSTGRES_USER,
    password: Redacted.make(nodeConfig.POSTGRES_PASSWORD),
    database: nodeConfig.POSTGRES_DB,
    maxConnections: 20,
    idleTimeout: Duration.minutes(5),
    connectTimeout: Duration.seconds(2),
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
          ],
        });
      case "SqlError":
        return new DatabaseInitializationError({
          message: `Failed to initialize the database`,
          cause: e,
        });
    }
  });
}).pipe(Effect.orDie);

/**
 * Live SQL client layer backed by PostgreSQL.
 */
const SqlClientLive: Layer.Layer<
  SqlClient.SqlClient,
  DatabaseInitializationError | ConfigError,
  NodeConfig
> = Layer.unwrapEffect(createPgLayerEffect);

/**
 * Public database service bundle used throughout the node.
 */
export const Database = {
  layer: Layer.provide(SqlClientLive, NodeConfig.layer),
};

/**
 * Convenience alias for the SQL client service type.
 */
export type Database = SqlClient.SqlClient;
