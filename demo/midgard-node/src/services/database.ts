import { Effect, Layer, Redacted } from "effect";
import { PgClient } from "@effect/sql-pg";
import { SqlClient, SqlError } from "@effect/sql";
import { NodeConfig } from "@/config.js";
import { ConfigError } from "effect/ConfigError";

export const createPgLayerEffect = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const pgConfig = {
    host: nodeConfig.POSTGRES_HOST,
    username: nodeConfig.POSTGRES_USER,
    password: Redacted.make(nodeConfig.POSTGRES_PASSWORD),
    database: nodeConfig.POSTGRES_DB,
    maxConnections: 20,
    idleTimeout: 30_000,
    connectTimeout: 2_000,
    trace: "all"
  };
  return PgClient.layer(pgConfig);
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
