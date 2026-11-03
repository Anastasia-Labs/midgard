import { HttpServerResponse } from "@effect/platform";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { Effect, Ref } from "effect";

import { Globals } from "../services/index.js";
import {
  operatorStatusCommand,
  parseOperatorKeyHashOption,
} from "../transactions/operators/commands.js";
import { SerializedStateQueueUTxO } from "../workers/utils/commit-block-header.js";
import { failWith500 } from "./listen-response.js";
import { OPERATOR_STATUS_ENDPOINT } from "./listen-router.get-state-queue-handler.js";

/**
 * `GET /operator/status[?operatorKeyHash=<hex>]` (admin): the same report as
 * the `operator-status` CLI verb, for the operator wallet's key by default.
 */
export const getOperatorStatusHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const rawKey = params["operatorKeyHash"];
  let operatorKeyHash: string | undefined;
  if (rawKey !== undefined) {
    if (typeof rawKey !== "string") {
      return yield* HttpServerResponse.json(
        { error: "operatorKeyHash must be a single hex string" },
        { status: 400 },
      );
    }
    try {
      operatorKeyHash = parseOperatorKeyHashOption(rawKey);
    } catch {
      return yield* HttpServerResponse.json(
        { error: "Invalid request" },
        { status: 400 },
      );
    }
  }
  const report = yield* operatorStatusCommand({ operatorKeyHash }).pipe(
    Effect.either,
  );
  if (report._tag === "Left") {
    yield* Effect.logWarning(
      `GET /${OPERATOR_STATUS_ENDPOINT} failed: ${"Operator status is unavailable"}`,
    );
    return yield* HttpServerResponse.json(
      { error: "Operator status is unavailable" },
      { status: 500 },
    );
  }
  return yield* HttpServerResponse.json(report.right);
});

export const getLogGlobalsHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✍  Logging global variables...`);
  const globals = yield* Globals;
  const BLOCKS_IN_QUEUE: number = yield* Ref.get(globals.BLOCKS_IN_QUEUE);
  const RESET_IN_PROGRESS: boolean = yield* Ref.get(globals.RESET_IN_PROGRESS);
  const COMMIT_WORKER_ACTIVE: boolean = yield* Ref.get(
    globals.COMMIT_WORKER_ACTIVE,
  );
  const COMMIT_PIPELINE_PHASE: string = yield* Ref.get(
    globals.COMMIT_PIPELINE_PHASE,
  );
  const AVAILABLE_CONFIRMED_BLOCK: "" | SerializedStateQueueUTxO =
    yield* Ref.get(globals.AVAILABLE_CONFIRMED_BLOCK);
  const PROCESSED_UNSUBMITTED_TXS_COUNT: number = yield* Ref.get(
    globals.PROCESSED_UNSUBMITTED_TXS_COUNT,
  );
  const PROCESSED_UNSUBMITTED_TXS_SIZE: number = yield* Ref.get(
    globals.PROCESSED_UNSUBMITTED_TXS_SIZE,
  );
  const UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH: string = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
  );
  const UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS: number = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
  );
  const unconfirmedSubmittedBlockAgeMs =
    UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH === "" ||
    UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS <= 0
      ? 0
      : Date.now() - UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS;
  const LOCAL_FINALIZATION_PENDING: boolean = yield* Ref.get(
    globals.LOCAL_FINALIZATION_PENDING,
  );
  const HEARTBEAT_BLOCK_COMMITMENT: number = yield* Ref.get(
    globals.HEARTBEAT_BLOCK_COMMITMENT,
  );
  const HEARTBEAT_BLOCK_CONFIRMATION: number = yield* Ref.get(
    globals.HEARTBEAT_BLOCK_CONFIRMATION,
  );
  const HEARTBEAT_MERGE: number = yield* Ref.get(globals.HEARTBEAT_MERGE);
  const HEARTBEAT_TX_QUEUE_PROCESSOR: number = yield* Ref.get(
    globals.HEARTBEAT_TX_QUEUE_PROCESSOR,
  );

  yield* Effect.logInfo(`
  BLOCKS_IN_QUEUE ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${BLOCKS_IN_QUEUE}
  RESET_IN_PROGRESS ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${RESET_IN_PROGRESS}
  COMMIT_WORKER_ACTIVE ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${COMMIT_WORKER_ACTIVE}
  COMMIT_PIPELINE_PHASE ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${COMMIT_PIPELINE_PHASE}
  AVAILABLE_CONFIRMED_BLOCK ⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${AVAILABLE_CONFIRMED_BLOCK === "" ? "unavailable" : "available"}
  PROCESSED_UNSUBMITTED_TXS_COUNT ⋅⋅⋅ ${PROCESSED_UNSUBMITTED_TXS_COUNT}
  PROCESSED_UNSUBMITTED_TXS_SIZE ⋅⋅⋅⋅ ${PROCESSED_UNSUBMITTED_TXS_SIZE}
  UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH ⋅⋅⋅⋅⋅⋅⋅ ${UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH}
  UNCONFIRMED_SUBMITTED_BLOCK_SINCE ⋅⋅⋅⋅⋅⋅⋅ ${UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS > 0 ? new Date(Number(UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS)).toLocaleString() : "N/A"} (${unconfirmedSubmittedBlockAgeMs}ms)
  LOCAL_FINALIZATION_PENDING ⋅⋅⋅⋅⋅⋅⋅⋅ ${LOCAL_FINALIZATION_PENDING}
  HEARTBEAT_BLOCK_COMMITMENT ⋅⋅ ${new Date(Number(HEARTBEAT_BLOCK_COMMITMENT)).toLocaleString()}
  HEARTBEAT_BLOCK_CONFIRMATION ⋅ ${new Date(Number(HEARTBEAT_BLOCK_CONFIRMATION)).toLocaleString()}
  HEARTBEAT_MERGE ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${new Date(Number(HEARTBEAT_MERGE)).toLocaleString()}
  HEARTBEAT_TX_QUEUE_PROCESSOR ⋅ ${new Date(Number(HEARTBEAT_TX_QUEUE_PROCESSOR)).toLocaleString()}
`);
  return yield* HttpServerResponse.json({
    message: `Global variables logged!`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "logGlobals", e)),
);
