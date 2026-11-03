import { HttpServerResponse } from "@effect/platform";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { Effect } from "effect";

import * as DepositStatusCommand from "../deposit-status.js";
import { failWith500 } from "../listen-response.js";
import { DEPOSIT_STATUS_ENDPOINT } from "./routes.js";

/**
 * `GET /deposit-status`: returns one serialized deposit row by event id or L1
 * tx hash.
 */
export const getDepositStatusHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;

  let lookup: DepositStatusCommand.DepositStatusLookup;
  try {
    lookup = DepositStatusCommand.parseDepositStatusLookup(params);
  } catch {
    const message = "Invalid request";
    yield* Effect.logInfo(
      `GET /${DEPOSIT_STATUS_ENDPOINT} - invalid request: ${message}`,
    );
    return yield* HttpServerResponse.json({ error: message }, { status: 400 });
  }

  const deposit =
    yield* DepositStatusCommand.resolveDepositStatusProgram(lookup);
  return yield* HttpServerResponse.json(
    DepositStatusCommand.encodeDepositStatus(deposit),
  );
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", DEPOSIT_STATUS_ENDPOINT, e),
  ),
  Effect.catchTag("DepositStatusCommandError", (e) =>
    HttpServerResponse.json({ error: e.message }, { status: e.status }),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      DEPOSIT_STATUS_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);
