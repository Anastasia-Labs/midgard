import { Context, Effect, Layer, Redacted, Config } from "effect";
import { PgClient } from "@effect/sql-pg";
import { SqlClient, SqlError } from "@effect/sql";
import { ConfigError } from "effect/ConfigError";
import { NodeConfig, NodeConfigDep } from "@/config.js";
import { initializeDb } from "../database/init.js";

const makeSqlClientService = Effect.gen(function* ($) {
  const nodeConfig = yield* NodeConfig;

  const config = {
    host: nodeConfig.POSTGRES_HOST,
    database: nodeConfig.POSTGRES_DB,
    username: nodeConfig.POSTGRES_USER,
    password: Redacted.make(nodeConfig.POSTGRES_PASSWORD),
  };

  const baseClientLayer = PgClient.layer(config);

  const getClientAndInitialize = Effect.gen(function* () {
    const sqlClient = yield* SqlClient.SqlClient;
    yield* initializeDb();
    return sqlClient;
  });

  const clientEffect = Effect.provide(getClientAndInitialize, baseClientLayer);

  const clientEffectWithErrorMapping = clientEffect.pipe(
    Effect.mapError((error): SqlError.SqlError => {
      if (error instanceof SqlError.SqlError) {
        return error;
      }
      console.error("SqlClientService: Encountered unexpected error:", error);
      return new SqlError.SqlError({
        message: "An unexpected error occurred during SqlClient setup",
        cause: error,
      });
    }),
  );

  return yield* clientEffectWithErrorMapping.pipe(Effect.orDie);
}).pipe(Effect.orDie);

export class SqlClientService extends Context.Tag("SqlClientService")<
  SqlClientService,
  SqlClient.SqlClient
>() {
  static readonly layer: Layer.Layer<SqlClientService, never, NodeConfig> =
    Layer.effect(SqlClientService, makeSqlClientService);
}
