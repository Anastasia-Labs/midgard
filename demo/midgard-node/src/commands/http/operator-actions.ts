import { HttpServerResponse } from "@effect/platform";
import { Effect } from "effect";

import { blockCommitmentAction } from "../../fibers/block-commitment.block-commitment-action.js";
import { formatLandedStateQueue } from "../../l1-state-queue/landed.js";
import { readLandedStateQueue } from "../../services/landed-state-queue.js";
import { MidgardContracts } from "../../services/midgard-contracts.make-midgard-contract-runtime.js";
import * as Initialization from "../../transactions/initialization.js";
import { failWith500 } from "../listen-response.js";
import { COMMIT_ENDPOINT, INIT_ENDPOINT } from "./routes.js";

/**
 * `GET /init`: initializes protocol state when startup policy and the landed
 * state queue allow it.
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
