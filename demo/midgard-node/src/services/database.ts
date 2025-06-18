import { Duration, Effect, Layer, Redacted } from "effect";
import { PgClient } from "@effect/sql-pg";
import { SqlClient, SqlError } from "@effect/sql";
import { NodeConfig, NodeConfigDep } from "@/config.js";
import { ConfigError } from "effect/ConfigError";

export const createPgLayerEffect = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  yield* Effect.logInfo("📚 Opening connection to db...");
  return PgClient.layer(mkPgConfig(nodeConfig));
}).pipe(Effect.orDie);

const SqlClientLive: Layer.Layer<
  SqlClient.SqlClient,
  SqlError.SqlError | ConfigError,
  NodeConfig
> = Layer.unwrapEffect(createPgLayerEffect);

export const Database = {
  layer: SqlClientLive,
};

export type Database = SqlClient.SqlClient;

export const mkPgConfig = (nodeConfig: NodeConfigDep) => {
  return {
    host: nodeConfig.POSTGRES_HOST,
    username: nodeConfig.POSTGRES_USER,
    password: Redacted.make(nodeConfig.POSTGRES_PASSWORD),
    database: nodeConfig.POSTGRES_DB,
    maxConnections: 20,
    idleTimeout: Duration.minutes(5),
    connectTimeout: Duration.seconds(2),
    validateConnection: true,
    trace: "all",
  };
};
