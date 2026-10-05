import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { SqlClient } from "@effect/sql/SqlClient";
import { Effect, Ref } from "effect";

import { Globals } from "../services/index.js";
import * as DepositStatusCommand from "./deposit-status.js";
import { failWith500 } from "./listen-response.js";
import { parseFixedHexParam } from "./listen-router.get-tx-handler.js";
import {
  type TxStatusBatchAdmissionRow,
  type TxStatusBatchMembershipRow,
  type TxStatusBatchRejectionRow,
} from "./listen-router.get-tx-status-handler.js";
import {
  DEPOSIT_STATUS_ENDPOINT,
  errorMessage,
  HEALTH_ENDPOINT,
  TX_STATUS_ENDPOINT,
} from "./listen-router.run-exact-gated-direct-l1-provider-probe.js";
import { resolveTxStatusBatch } from "./tx-status.js";
import { readTxStatusMergeEvidence } from "./tx-status-merge-evidence.js";

export const postTxStatusBatchHandler = Effect.gen(function* () {
  const request = yield* HttpServerRequest.HttpServerRequest;
  const parsedBody = yield* Effect.either(request.json);
  if (parsedBody._tag === "Left") {
    return yield* HttpServerResponse.json(
      { error: "Request body must be valid JSON." },
      { status: 400 },
    );
  }
  const txHashes = (parsedBody.right as { readonly txHashes?: unknown })
    .txHashes;
  if (
    !Array.isArray(txHashes) ||
    txHashes.some((entry) => typeof entry !== "string")
  ) {
    return yield* HttpServerResponse.json(
      { error: "Request body must include txHashes: string[]." },
      { status: 400 },
    );
  }
  if (txHashes.length === 0 || txHashes.length > 1000) {
    return yield* HttpServerResponse.json(
      { error: "txHashes must contain 1 to 1000 transaction hashes." },
      { status: 400 },
    );
  }
  const normalized = txHashes.map((txHash) => txHash.toLowerCase());
  const txIdBytes = normalized.map((txHash) => parseFixedHexParam(txHash, 32));
  const invalidIndex = txIdBytes.findIndex((txId) => txId === null);
  if (invalidIndex >= 0) {
    return yield* HttpServerResponse.json(
      { error: `Invalid transaction hash: ${txHashes[invalidIndex]}` },
      { status: 400 },
    );
  }
  const txIds = txIdBytes as Buffer[];
  const sql = yield* SqlClient;
  const globals = yield* Globals;
  const [
    rejectionRows,
    admissionRows,
    immutableRows,
    mempoolRows,
    processedMempoolRows,
    headerEvidenceByTxId,
  ] = yield* Effect.all(
    [
      sql<TxStatusBatchRejectionRow>`SELECT DISTINCT ON (tx_id)
          encode(tx_id, 'hex') AS tx_hash,
          reject_code,
          reject_detail,
          created_at
        FROM tx_rejections
        WHERE ${sql.in("tx_id", txIds)}
        ORDER BY tx_id, created_at DESC`,
      sql<TxStatusBatchAdmissionRow>`SELECT
          encode(tx_id, 'hex') AS tx_hash,
          status
        FROM tx_admissions
        WHERE ${sql.in("tx_id", txIds)}`,
      sql<TxStatusBatchMembershipRow>`SELECT
          encode(tx_id, 'hex') AS tx_hash
        FROM immutable
        WHERE ${sql.in("tx_id", txIds)}`,
      sql<TxStatusBatchMembershipRow>`SELECT
          encode(tx_id, 'hex') AS tx_hash
        FROM mempool
        WHERE ${sql.in("tx_id", txIds)}`,
      sql<TxStatusBatchMembershipRow>`SELECT
          encode(tx_id, 'hex') AS tx_hash
        FROM processed_mempool
        WHERE ${sql.in("tx_id", txIds)}`,
      readTxStatusMergeEvidence(txIds),
    ],
    { concurrency: "unbounded" },
  );
  const rejectionsByTxId = new Map(
    rejectionRows.map((row) => [
      row.tx_hash,
      {
        rejectCode: row.reject_code,
        rejectDetail: row.reject_detail,
        createdAtIso: row.created_at.toISOString(),
      },
    ]),
  );
  const admissionStatusByTxId = new Map(
    admissionRows.map((row) => [row.tx_hash, row.status]),
  );
  const immutableTxIds = new Set(immutableRows.map((row) => row.tx_hash));
  const mempoolTxIds = new Set(mempoolRows.map((row) => row.tx_hash));
  const processedMempoolTxIds = new Set(
    processedMempoolRows.map((row) => row.tx_hash),
  );
  const results = resolveTxStatusBatch({
    txIdsHex: normalized,
    rejectionsByTxId,
    admissionStatusByTxId,
    immutableTxIds,
    mempoolTxIds,
    processedMempoolTxIds,
    localFinalizationPending: yield* Ref.get(
      globals.LOCAL_FINALIZATION_PENDING,
    ),
    headerEvidenceByTxId,
  });
  return yield* HttpServerResponse.json({ results });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("POST", TX_STATUS_ENDPOINT, e),
  ),
  Effect.catchTag("SqlError", (e) =>
    failWith500(
      "POST",
      TX_STATUS_ENDPOINT,
      e.cause,
      "batched transaction status query failed",
    ),
  ),
);

/**
 * `GET /deposit-status`: returns one serialized deposit row by event id or L1
 * tx hash.
 */
export const getDepositStatusHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;

  let lookup: DepositStatusCommand.DepositStatusLookup;
  try {
    lookup = DepositStatusCommand.parseDepositStatusLookup(params);
  } catch (error) {
    const message = errorMessage(error);
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
