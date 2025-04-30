import { Layer, Redacted } from "effect";
import { PgClient } from "@effect/sql-pg";
import { SqlClient, SqlError } from "@effect/sql";
import { ConfigError } from "effect/ConfigError";

const testDbConfig = {
  database: "database",
  host: "localhost",
  port: 5432,
  username: "postgres",
  password: Redacted.make("postgres"),
};

export const testSqlLayer: Layer.Layer<
  SqlClient.SqlClient,
  SqlError.SqlError | ConfigError,
  never
> = PgClient.layer(testDbConfig);
