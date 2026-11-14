import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { HttpServerResponse } from "@effect/platform";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";

import { MempoolDB, TxAdmissionsDB } from "../../database/index.js";
import { l1NowUnixTimeMs } from "../../l1-heads.js";
import { landedElements } from "../../l1-state-queue/index.js";
import {
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../../services/index.js";
import { readLandedStateQueue } from "../../services/landed-state-queue.js";
import {
  operatorStatusCommand,
  parseOperatorKeyHashOption,
} from "../../transactions/operators/commands.js";
import {
  classifyOldestQueuedBlockReadiness,
  DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING,
  mergeMaturityWindow,
  planMergePreflight,
} from "../../transactions/state-queue/merge-readiness.js";
import { SerializedStateQueueUTxO } from "../../workers/utils/commit-block-header.js";
import { parseReadPage } from "../listen-read-page.js";
import { failWith500, handleStateQueueGetFailure } from "../listen-response.js";
import { stateQueueHeaderPage } from "../listen-state-queue-page.js";
import { OPERATOR_STATUS_ENDPOINT } from "./routes.js";

/**
 * `GET /stateQueue`: logs and returns the current ordered state-queue headers.
 */
export const getStateQueueHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const requestPage = yield* Effect.try(() => parseReadPage(params, 60)).pipe(
    Effect.either,
  );
  if (requestPage._tag === "Left")
    return yield* HttpServerResponse.json(
      { error: "Invalid page" },
      { status: 400 },
    );
  const globals = yield* Globals;
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const nodeConfig = yield* NodeConfig;
  // The landed queue (P1). An unhealthy queue is still drawn, as far as its
  // walk reached, with its reason: reads keep serving.
  const read = yield* readLandedStateQueue(contracts.stateQueue);
  if (read.kind !== "ok") {
    return yield* Effect.fail(
      new SDK.StateQueueError({
        message: "The landed state queue is unavailable",
        cause: `${read.kind}: ${read.detail}`,
      }),
    );
  }
  const landed = read.queue;
  const sortedUTxOs = landedElements(landed).map(({ element }) => element);
  const headers = sortedUTxOs.flatMap((u) =>
    u.datum.key === "Empty" ? [] : [u.datum.key.Key.key],
  );
  const viewDigest = createHash("sha256")
    .update(
      JSON.stringify({
        generation: landed.view.generation,
        point: landed.view.point,
      }),
    )
    .digest();
  const headerPage = stateQueueHeaderPage(
    headers,
    requestPage.right,
    viewDigest,
  );
  if (headerPage.kind !== "page")
    return yield* HttpServerResponse.json(
      {
        error: "State queue cursor is invalid or its view changed",
        error_code:
          headerPage.kind === "stale"
            ? "E_HTTP_CURSOR_STALE"
            : "E_HTTP_CURSOR_INVALID",
      },
      { status: headerPage.kind === "stale" ? 409 : 400 },
    );
  const [
    unconfirmedSubmittedBlockTxHash,
    resetInProgress,
    durableAdmissionBacklog,
    mempoolTxCount,
  ] = yield* Effect.all(
    [
      Ref.get(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH),
      Ref.get(globals.RESET_IN_PROGRESS),
      TxAdmissionsDB.countBacklog,
      MempoolDB.retrieveTxCount,
    ],
    { concurrency: "unbounded" },
  );
  const mergeReadiness = planMergePreflight({
    force: false,
    queueLength: headers.length,
    minQueueLength:
      nodeConfig.MIN_QUEUE_LENGTH_FOR_MERGING ??
      DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING,
    unresolvedSubmittedBlockTxHash: unconfirmedSubmittedBlockTxHash,
    resetInProgress,
    durableAdmissionBacklog,
    mempoolTxCount,
  });
  const oldestQueuedBlock = sortedUTxOs.find((u) => u.datum.key !== "Empty");
  const oldestBlockReadiness =
    oldestQueuedBlock === undefined
      ? null
      : yield* Effect.gen(function* () {
          const blockHeader = yield* SDK.getHeaderFromStateQueueDatum(
            oldestQueuedBlock.datum,
          );
          const stateQueueNode =
            yield* SDK.getStateQueueNodeFromStateQueueDatum(
              oldestQueuedBlock.datum,
            );
          const headerHash = yield* SDK.hashBlockHeader(blockHeader);
          const maturity = mergeMaturityWindow(
            lucid.api,
            Number(stateQueueNode.block_time),
          );
          return classifyOldestQueuedBlockReadiness({
            headerHash,
            currentDaAvailability: stateQueueNode.da_attestation,
            provenFraud: stateQueueNode.proven_fraud,
            readyAfterUnixTime: maturity.readyAfterUnixTime,
            // Maturity at the L1 `slotNow`, as the merge fiber judges it.
            nowUnixTime: yield* l1NowUnixTimeMs(lucid.api),
          });
        });
  yield* Effect.logInfo("State queue read", {
    headerCount: headers.length,
    returnedCount: headerPage.entries.length,
  });
  return yield* HttpServerResponse.json({
    headers: headerPage.entries,
    nextCursor: headerPage.nextCursor,
    headerCount: headers.length,
    landedQueue: {
      healthy: landed.healthy,
      reason: landed.reason,
      detail: landed.detail,
      generation: landed.view.generation,
      slot: landed.view.point.slot,
    },
    mergeReadiness: {
      ...mergeReadiness,
      durableAdmissionBacklog: durableAdmissionBacklog.toString(),
      mempoolTxCount: mempoolTxCount.toString(),
      oldestBlock: oldestBlockReadiness,
    },
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", "logStateQueue", e),
  ),
  Effect.catchTag("DataCoercionError", (e) =>
    failWith500("GET", "logStateQueue", e.cause, e.message),
  ),
  Effect.catchTag("HashingError", (e) =>
    failWith500("GET", "logStateQueue", e.cause, e.message),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      "logStateQueue",
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
  Effect.catchTag("StateQueueError", (e) =>
    handleStateQueueGetFailure("logStateQueue", e),
  ),
);

/**
 * `GET /logBlocksDB`: logs a compact summary of block-link rows.
 */
export const getLogBlocksDBHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✍  Querying BlocksDB...`);
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    header_hash: Buffer;
    count: number;
  }>`SELECT header_hash, COUNT(*)::int AS count
    FROM (SELECT header_hash FROM blocks ORDER BY height DESC LIMIT 100) AS recent GROUP BY header_hash`;
  yield* Effect.logInfo(
    `Recent block-link sample: rows=${rows.reduce((sum, row) => sum + row.count, 0).toString()},headers=${rows.length.toString()}`,
  );
  return yield* HttpServerResponse.json({
    message: "Recent block-link counts logged",
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "logBlocksDB", e)),
  Effect.catchTag("SqlError", (e) => failWith500("POST", "logBlocksDB", e)),
);

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
  const HEARTBEAT_BLOCK_COMMITMENT: number = yield* Ref.get(
    globals.HEARTBEAT_BLOCK_COMMITMENT,
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
  HEARTBEAT_BLOCK_COMMITMENT ⋅⋅ ${new Date(Number(HEARTBEAT_BLOCK_COMMITMENT)).toLocaleString()}
  HEARTBEAT_MERGE ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${new Date(Number(HEARTBEAT_MERGE)).toLocaleString()}
  HEARTBEAT_TX_QUEUE_PROCESSOR ⋅ ${new Date(Number(HEARTBEAT_TX_QUEUE_PROCESSOR)).toLocaleString()}
`);
  return yield* HttpServerResponse.json({
    message: `Global variables logged!`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "logGlobals", e)),
);
