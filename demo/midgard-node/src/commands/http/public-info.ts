import * as SDK from "@al-ft/midgard-sdk";
import { HttpServerResponse } from "@effect/platform";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { Effect } from "effect";

import { blockTransactionPage } from "../../database/http-read-pages.js";
import {
  ContractDeploymentIdentity,
  Lucid,
  NodeConfig,
} from "../../services/index.js";
import { encodeReadPage, parseReadPage } from "../listen-read-page.js";
import { failWith500 } from "../listen-response.js";
import * as ProtocolInfoCommand from "../protocol-info.js";
import { parseFixedHexParam } from "./parameters.js";
import {
  BLOCK_ENDPOINT,
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
        deploymentMarker: deploymentIdentity.deploymentMarker,
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
  yield* Effect.logInfo(
    `GET /block - Request received for header_hash: [redacted]`,
  );

  const headerHash = parseFixedHexParam(hdrHash, 28);
  if (headerHash === null) {
    yield* Effect.logInfo(
      `GET /${BLOCK_ENDPOINT} - Invalid block hash: [redacted]`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid block hash: [redacted]` },
      { status: 400 },
    );
  }
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
    `GET /${BLOCK_ENDPOINT} - Found ${hashes.length} txs for block: [redacted]`,
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
