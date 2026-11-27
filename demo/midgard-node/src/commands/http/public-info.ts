import * as SDK from "@al-ft/midgard-sdk/core";
import { HttpServerResponse } from "@effect/platform";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { Effect } from "effect";

import {
  blockTransactionPage,
  decodeRecentBlockCursor,
  encodeRecentBlockCursor,
  RECENT_BLOCK_CURSOR_BYTES,
  recentBlocksPage,
} from "../../database/http-read-pages.js";
import { NodeConfig } from "../../services/index.js";
import { Lucid } from "../../services/lucid.js";
import { ContractDeploymentIdentity } from "../../services/midgard-contracts.make-midgard-contract-runtime.js";
import { encodeReadPage, parseReadPage } from "../listen-read-page.js";
import { failWith500 } from "../listen-response.js";
import * as ProtocolInfoCommand from "../protocol-info.js";
import { parseFixedHexParam } from "./parameters.js";
import {
  BLOCK_ENDPOINT,
  BLOCKS_ENDPOINT,
  HEALTH_ENDPOINT,
  PROTOCOL_INFO_ENDPOINT,
} from "./routes.js";

/**
 * `GET /protocol-info`: returns stable public facts needed by external
 * Midgard transaction builders.
 */
export const getProtocolInfoHandler = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const lucid = yield* Lucid;
  const deploymentIdentity = yield* ContractDeploymentIdentity;
  const response = yield* Effect.try({
    try: () =>
      ProtocolInfoCommand.encodeProtocolInfo({
        nodeConfig,
        currentSlot: lucid.api.currentSlot(),
        hubOraclePolicyId: deploymentIdentity.hubOraclePolicyId,
        consensusProfile: deploymentIdentity.consensusProfile,
      }),
    catch: (error) => error,
  });
  return yield* HttpServerResponse.json(response);
}).pipe(Effect.catchAll((e) => failWith500("GET", PROTOCOL_INFO_ENDPOINT, e)));

/**
 * `GET /block`: returns tx hashes referenced by a committed block header.
 */
export const getBlockHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const hdrHash = params["header_hash"];

  const headerHash = parseFixedHexParam(hdrHash, 28);
  if (headerHash === null) {
    yield* Effect.logInfo(`GET /${BLOCK_ENDPOINT} - Invalid block hash`);
    return yield* HttpServerResponse.json(
      { error: "Invalid block hash" },
      { status: 400 },
    );
  }
  const headerHashHex = SDK.bufferToHex(headerHash);
  const parsedPage = yield* Effect.try(() => parseReadPage(params)).pipe(
    Effect.either,
  );
  if (parsedPage._tag === "Left")
    return yield* HttpServerResponse.json(
      { error: "Invalid page" },
      { status: 400 },
    );
  const hashes = yield* blockTransactionPage(headerHash, parsedPage.right);
  const page = encodeReadPage(
    hashes,
    parsedPage.right,
    (hash) => hash,
    SDK.bufferToHex,
  );
  yield* Effect.logInfo(
    `GET /${BLOCK_ENDPOINT} - Found block transactions`,
  ).pipe(
    Effect.annotateLogs({
      header_hash: headerHashHex,
      count: page.entries.length,
    }),
  );
  return yield* HttpServerResponse.json({
    hashes: page.entries,
    nextCursor: page.nextCursor,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", BLOCK_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      BLOCK_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `GET /blocks`: returns recent committed block metadata in reverse
 * chronological order (`block_end_time DESC, header_hash DESC`).
 */
export const getBlocksHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const parsedPage = yield* Effect.try(() => {
    const page = parseReadPage(params, RECENT_BLOCK_CURSOR_BYTES);
    if (page.cursor !== undefined) decodeRecentBlockCursor(page.cursor);
    return page;
  }).pipe(Effect.either);
  if (parsedPage._tag === "Left")
    return yield* HttpServerResponse.json(
      { error: "Invalid page" },
      { status: 400 },
    );
  const rows = yield* recentBlocksPage(parsedPage.right);
  const page = encodeReadPage(
    rows,
    parsedPage.right,
    encodeRecentBlockCursor,
    (row) => ({
      headerHash: SDK.bufferToHex(row.header_hash),
      blockStartTime: row.block_start_time.toISOString(),
      blockEndTime: row.block_end_time.toISOString(),
      blockTime: row.block_time.toISOString(),
      l2TransactionCount: Number(row.l2_transaction_count),
      depositCount: Number(row.deposit_count),
      withdrawalCount: Number(row.withdrawal_count),
      forcedTransactionCount: Number(row.forced_transaction_count),
      totalEventCount: Number(row.total_event_count),
    }),
  );
  yield* Effect.logInfo(`GET /${BLOCKS_ENDPOINT} - Found recent blocks`).pipe(
    Effect.annotateLogs({ count: page.entries.length }),
  );
  return yield* HttpServerResponse.json({
    blocks: page.entries,
    nextCursor: page.nextCursor,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", BLOCKS_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      BLOCKS_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `GET /healthz`: liveness endpoint that only confirms the server is running.
 */
export const getHealthHandler = Effect.gen(function* () {
  return yield* HttpServerResponse.json({
    status: "ok",
    now: new Date().toISOString(),
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", HEALTH_ENDPOINT, e),
  ),
);
