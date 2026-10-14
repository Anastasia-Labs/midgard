import { HttpServerResponse } from "@effect/platform";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import { HttpReadSql } from "../services/database.js";

export const publicReadDataLimitResponse = () =>
  HttpServerResponse.json(
    {
      error: "Stored data exceeds public HTTP read capacity",
      error_code: "E_HTTP_READ_DATA_LIMIT",
    },
    { status: 503, headers: { "retry-after": "5", connection: "close" } },
  );

const permits = new WeakMap<object, Effect.Semaphore>();
/** A separate four-connection pool and fail-fast request quota protect admission SQL. */
export const withHttpReadSql = <E, R>(
  handler: Effect.Effect<HttpServerResponse.HttpServerResponse, E, R>,
  timeoutMs = 2_000,
) =>
  Effect.gen(function* () {
    const sql = yield* HttpReadSql;
    let semaphore = permits.get(sql);
    if (semaphore === undefined) {
      semaphore = Effect.unsafeMakeSemaphore(4);
      permits.set(sql, semaphore);
    }
    const available = yield* semaphore.withPermitsIfAvailable(1)(
      handler.pipe(
        Effect.provideService(SqlClient.SqlClient, sql),
        Effect.timeoutOption(timeoutMs),
      ),
    );
    if (Option.isSome(available) && Option.isSome(available.value))
      return available.value.value;
    return yield* HttpServerResponse.json(
      {
        error: "Public read capacity is unavailable",
        error_code: "E_HTTP_READ_UNAVAILABLE",
      },
      { status: 503, headers: { "retry-after": "5", connection: "close" } },
    );
  });
