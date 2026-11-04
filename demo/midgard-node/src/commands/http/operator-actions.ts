import { HttpServerResponse } from "@effect/platform";
import { Effect } from "effect";

import { blockCommitmentAction, mergeAction } from "../../fibers/index.js";
import * as Genesis from "../../genesis.js";
import { formatLandedStateQueue } from "../../l1-state-queue/index.js";
import { MidgardContracts } from "../../services/index.js";
import { readLandedStateQueue } from "../../services/landed-state-queue.js";
import * as Initialization from "../../transactions/initialization.js";
import { failWith500, handleStateQueueGetFailure } from "../listen-response.js";
import { COMMIT_ENDPOINT, INIT_ENDPOINT, MERGE_ENDPOINT } from "./routes.js";

/**
 * `GET /init`: initializes protocol state when startup policy and the landed
 * state queue (P1) allow it.
 */
export const getInitHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✨ Initialization request received`);
  const contracts = yield* MidgardContracts;
  const read = yield* readLandedStateQueue(contracts.stateQueue);
  if (read.kind !== "ok") {
    return yield* HttpServerResponse.json(
      {
        error: "Cannot initialize: the landed state queue is unavailable",
        reason: read.kind,
        details: read.detail,
      },
      { status: 503 },
    );
  }
  const landed = read.queue;
  if (landed.policyOutputCount > 0) {
    const details = formatLandedStateQueue(landed);
    if (!landed.healthy) {
      yield* Effect.logWarning(
        `GET /${INIT_ENDPOINT} - Refusing to initialize over an unhealthy state queue (${details})`,
      );
      return yield* HttpServerResponse.json(
        {
          error:
            "Cannot initialize: configured state_queue policy already has an unhealthy landed queue",
          details,
          reason: landed.reason ?? "unknown",
        },
        { status: 409 },
      );
    }
    yield* Effect.logInfo(
      `GET /${INIT_ENDPOINT} - Skipping initialization (already initialized): ${details}`,
    );
    return yield* HttpServerResponse.json({
      message: "State queue already initialized",
      details,
    });
  }

  const txHash = yield* Initialization.program();
  yield* Genesis.program;
  yield* Effect.logInfo(
    `GET /${INIT_ENDPOINT} - Initialization successful: ${txHash}`,
  );
  return yield* HttpServerResponse.json({
    message: `Initiation successful: ${txHash}`,
  });
}).pipe(Effect.catchAll((e) => failWith500("GET", INIT_ENDPOINT, e)));

/**
 * `GET /commit`: triggers manual block commitment.
 */
export const getCommitEndpoint = Effect.gen(function* () {
  yield* Effect.logInfo(
    `GET /${COMMIT_ENDPOINT} - Manual block commitment order received`,
  );
  yield* blockCommitmentAction;
  yield* Effect.logInfo(
    `GET /${COMMIT_ENDPOINT} - Block commitment successful`,
  );
  return yield* HttpServerResponse.json({
    message: "Block commitment successful",
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", COMMIT_ENDPOINT, e),
  ),
  Effect.catchTag("WorkerError", (e) =>
    failWith500("GET", COMMIT_ENDPOINT, e.cause, "failed worker"),
  ),
);

/**
 * `GET /merge`: triggers manual merge of the oldest queued block into
 * confirmed state.
 */
export const getMergeHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`GET /${MERGE_ENDPOINT} - Manual merge order received`);
  const attempt = yield* mergeAction(true).pipe(
    Effect.map((result) => ({ _tag: "Ran" as const, result })),
    Effect.catchTag("MergeProducerPermitUnavailable", (unavailable) =>
      Effect.succeed({ _tag: "PermitUnavailable" as const, unavailable }),
    ),
  );
  if (attempt._tag === "PermitUnavailable") {
    // Nothing ran: the follower-change driver has not published its view.
    // Retry later.
    yield* Effect.logWarning(
      `GET /${MERGE_ENDPOINT} - E_MERGE_PRODUCER_UNAVAILABLE`,
    );
    return yield* HttpServerResponse.json(
      {
        error: "Merge producer is unavailable",
        error_code: "E_MERGE_PRODUCER_UNAVAILABLE",
      },
      { status: 503 },
    );
  }
  const { result } = attempt;
  yield* Effect.logInfo(`POST /${MERGE_ENDPOINT} - Merge request processed`);
  return yield* HttpServerResponse.json({
    message: "Merge request processed",
    result,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      MERGE_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
  Effect.catchTag("TxSubmitError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, `${e._tag}: ${e.message}`),
  ),
  Effect.catchTag("TxConfirmError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, `${e._tag}: ${e.message}`),
  ),
  Effect.catchTag("TxSignError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, `${e._tag}: ${e.message}`),
  ),
  Effect.catchTag("CmlDeserializationError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("DataCoercionError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("LinkedListError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("HashingError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("LucidError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("StateQueueError", (e) =>
    handleStateQueueGetFailure(MERGE_ENDPOINT, e),
  ),
);
