import "./listen-router.get-pipeline-status-handler.js";

import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { HttpServerResponse } from "@effect/platform";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { SqlClient } from "@effect/sql";
import { Effect, Ref } from "effect";

import {
  PublicHttpReadLimitError,
  requirePublicReadPayload,
} from "../database/http-read-byte-budget.js";
import { addressHistoryPage } from "../database/http-read-pages.js";
import {
  MempoolDB,
  MutationJobsDB,
  TxAdmissionsDB,
} from "../database/index.js";
import { mergeAction } from "../fibers/index.js";
import { l1NowUnixTimeMs } from "../l1-heads.js";
import { landedElements } from "../l1-state-queue/index.js";
import {
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "../services/index.js";
import { readLandedStateQueue } from "../services/landed-state-queue.js";
import {
  classifyOldestQueuedBlockReadiness,
  DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING,
  mergeMaturityWindow,
  planMergePreflight,
} from "../transactions/state-queue/merge-readiness.js";
import { parseAddressArgument } from "./command-utils.js";
import { publicReadDataLimitResponse } from "./listen-http-read.js";
import { encodeReadPage, parseReadPage } from "./listen-read-page.js";
import { failWith500, handleStateQueueGetFailure } from "./listen-response.js";
import {
  ADDRESS_HISTORY_ENDPOINT,
  MERGE_ENDPOINT,
} from "./listen-router.l1-provider-readiness-evidence-is-fresh.js";
import { stateQueueHeaderPage } from "./listen-state-queue-page.js";

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

/**
 * `GET /txs`: returns the address-history tx payloads for an address.
 */
export const getTxsOfAddressHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const addr = params["address"];

  if (typeof addr !== "string") {
    yield* Effect.logInfo(
      `GET /${ADDRESS_HISTORY_ENDPOINT} - Invalid address type: [redacted]`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid address type: [redacted]` },
      { status: 400 },
    );
  }
  try {
    const address = parseAddressArgument(addr);
    const rows = yield* addressHistoryPage(address, parseReadPage(params));
    const page = encodeReadPage(
      rows,
      parseReadPage(params),
      (row) => row.tx_id,
      (row) => SDK.bufferToHex(requirePublicReadPayload(row.tx)),
    );
    const cbors = page.entries;
    yield* Effect.logInfo(`Found ${cbors.length} CBORs with [redacted]`);
    return yield* HttpServerResponse.json({
      txs: cbors,
      nextCursor: page.nextCursor,
    });
  } catch (_error) {
    if (_error instanceof PublicHttpReadLimitError)
      return yield* publicReadDataLimitResponse();
    yield* Effect.logInfo(`Invalid address: [redacted]`);
    return yield* HttpServerResponse.json(
      { error: `Invalid address: [redacted]` },
      { status: 400 },
    );
  }
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "txs", e)),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      ADDRESS_HISTORY_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

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
    localFinalizationPending,
    resetInProgress,
    durableAdmissionBacklog,
    mempoolTxCount,
    unfinishedMutationJobs,
  ] = yield* Effect.all(
    [
      Ref.get(globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH),
      Ref.get(globals.LOCAL_FINALIZATION_PENDING),
      Ref.get(globals.RESET_IN_PROGRESS),
      TxAdmissionsDB.countBacklog,
      MempoolDB.retrieveTxCount,
      MutationJobsDB.countUnfinished,
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
    localFinalizationPending,
    resetInProgress,
    durableAdmissionBacklog,
    mempoolTxCount,
    unfinishedMutationJobs,
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
      unfinishedMutationJobs: unfinishedMutationJobs.toString(),
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
 * `GET /logGlobals`: logs the current process-global coordination state.
 */
export const OPERATOR_STATUS_ENDPOINT = "operator/status";
