import { Effect, Layer, Scope, Context, Redacted } from "effect";
import { PgClient } from "@effect/sql-pg";
import { SqlClient, SqlError } from "@effect/sql";
import { ConfigError } from "effect/ConfigError";

import { BlocksDB } from "../../src/database/index";

const testDbConfig = {
  database: "database",
  host: "localhost",
  port: 5432,
  username: "postgres",
  password: Redacted.make("postgres"),
};

const BaseTestSqlClientLive: Layer.Layer<
  never,
  SqlError.SqlError | ConfigError,
  SqlClient.SqlClient
> = PgClient.layer(testDbConfig);

const ensureSchema = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  yield* Effect.logInfo("Ensuring test database schema...");
  yield* sql
    .unsafe(BlocksDB.createQuery)
    .pipe(
      Effect.catchTag("SqlError", (error) =>
        Effect.logWarning(
          "Ignoring schema creation error (might already exist)",
          error,
        ),
      ),
    );
  yield* Effect.logInfo("Test schema ready.");
});

export const testLayer: Layer.Layer<
  never,
  SqlError.SqlError | ConfigError,
  SqlClient.SqlClient
> = Layer.provideMerge(
  BaseTestSqlClientLive,
  Layer.effectDiscard(ensureSchema),
);
