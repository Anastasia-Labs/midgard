import { HttpServerResponse } from "@effect/platform";
import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import { HttpReadSql } from "../services/database.js";
import {
  HTTP_READ_LANE_PERMITS,
  type HttpReadLane,
} from "../services/http-resource-limits.js";

export const publicReadDataLimitResponse = () =>
  HttpServerResponse.json(
    {
      error: "Stored data exceeds public HTTP read capacity",
      error_code: "E_HTTP_READ_DATA_LIMIT",
    },
    { status: 503, headers: { "retry-after": "5", connection: "close" } },
  );

const permits = new WeakMap<object, Record<HttpReadLane, Effect.Semaphore>>();
/** A separate http-read pool and fail-fast request quotas protect admission SQL. */
export const withHttpReadSql = <E, R>(
  handler: Effect.Effect<HttpServerResponse.HttpServerResponse, E, R>,
  timeoutMs = 2_000,
  lane: HttpReadLane = "public",
) =>
  Effect.gen(function* () {
    const sql = yield* HttpReadSql;
    let lanes = permits.get(sql);
    if (lanes === undefined) {
      lanes = {
        public: Effect.unsafeMakeSemaphore(HTTP_READ_LANE_PERMITS.public),
        operator: Effect.unsafeMakeSemaphore(HTTP_READ_LANE_PERMITS.operator),
      };
      permits.set(sql, lanes);
    }
    const semaphore = lanes[lane];
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
        error:
          lane === "public"
            ? "Public read capacity is unavailable"
            : "Operator read capacity is unavailable",
        error_code: "E_HTTP_READ_UNAVAILABLE",
      },
      { status: 503, headers: { "retry-after": "5", connection: "close" } },
    );
  });
