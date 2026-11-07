import * as SDK from "@al-ft/midgard-sdk";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { SqlClient } from "@effect/sql/SqlClient";
import { Effect, Option, Ref } from "effect";

import { publicTxCborByHash } from "../../database/http-read-lookups.js";
import { readHttpTxStatusFacts } from "../../database/http-tx-status-facts.js";
import { TxAdmissionsDB } from "../../database/index.js";
import { AdmissionWriter, Globals } from "../../services/index.js";
import { publicReadDataLimitResponse } from "../listen-http-read.js";
import {
  includePrivateHttpDiagnosticDetails,
  publicTxStatusPresentation,
} from "../listen-public-error-presentation.js";
import { failWith500 } from "../listen-response.js";
import { resolveTxStatus, resolveTxStatusBatch } from "../tx-status.js";
import { readTxStatusMergeEvidence } from "../tx-status-merge-evidence.js";
import { parseFixedHexParam } from "./parameters.js";
import { TX_ENDPOINT, TX_STATUS_ENDPOINT } from "./routes.js";

/**
 * `GET /tx`: returns the CBOR for a known tx hash from mempool or immutable
 * storage.
 */
export const getTxHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const txHashParam = params["tx_hash"];
  const txHashBytes = parseFixedHexParam(txHashParam, 32);
  if (txHashBytes === null) {
    yield* Effect.logInfo(
      `GET /${TX_ENDPOINT} - Invalid transaction hash: [redacted]`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid transaction hash: [redacted]` },
      { status: 404 },
    );
  }
  const foundCbor = yield* publicTxCborByHash(txHashBytes);
  yield* Effect.logInfo(
    `GET /${TX_ENDPOINT} - Transaction found in mempool: [redacted]`,
  );
  return yield* HttpServerResponse.json({
    tx: SDK.bufferToHex(foundCbor),
  });
}).pipe(
  Effect.catchTag("PublicHttpReadLimitError", publicReadDataLimitResponse),
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", TX_ENDPOINT, e)),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      TX_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `GET /tx-status`: resolves the node's canonical status for a tx hash.
 */
export const getTxStatusHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const txHashParam = params["tx_hash"];
  const txHashBytes = parseFixedHexParam(txHashParam, 32);
  if (txHashBytes === null) {
    return yield* HttpServerResponse.json(
      { error: `Invalid transaction hash: [redacted]` },
      { status: 400 },
    );
  }

  const globals = yield* Globals;
  // A tx the admission writer still holds has no row yet: it is queued.
  // Checked before the database read, so an acknowledged tx never reads as
  // not_found while its write is pending.
  const writer = yield* Effect.serviceOption(AdmissionWriter);
  if (
    Option.isSome(writer) &&
    writer.value.pendingSince(txHashBytes) !== undefined
  ) {
    return yield* HttpServerResponse.json(
      publicTxStatusPresentation(
        resolveTxStatus({
          txIdHex: txHashParam as string,
          rejection: null,
          admissionStatus: "queued",
          inImmutable: false,
          inMempool: false,
          inProcessedMempool: false,
          localFinalizationPending: yield* Ref.get(
            globals.LOCAL_FINALIZATION_PENDING,
          ),
        }),
      ),
    );
  }
  const facts = yield* readHttpTxStatusFacts(
    txHashBytes,
    includePrivateHttpDiagnosticDetails(),
  );

  const headerEvidence = yield* readTxStatusMergeEvidence([txHashBytes]);
  const resolved = resolveTxStatus({
    txIdHex: txHashParam as string,
    ...headerEvidence.get(txHashBytes.toString("hex")),
    rejection:
      facts.reject_code !== null && facts.created_at !== null
        ? {
            rejectCode: facts.reject_code,
            rejectDetail: facts.reject_detail,
            createdAtIso: facts.created_at.toISOString(),
          }
        : null,
    admissionStatus: facts.admission_status,
    inImmutable: facts.in_immutable,
    inMempool: facts.in_mempool,
    inProcessedMempool: facts.in_processed_mempool,
    localFinalizationPending: yield* Ref.get(
      globals.LOCAL_FINALIZATION_PENDING,
    ),
  });

  if (resolved.status === "not_found") {
    return yield* HttpServerResponse.json(
      publicTxStatusPresentation(resolved),
      { status: 404 },
    );
  }

  return yield* HttpServerResponse.json(publicTxStatusPresentation(resolved));
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", TX_STATUS_ENDPOINT, e),
  ),
  Effect.catchTag("SqlError", (e) =>
    failWith500(
      "GET",
      TX_STATUS_ENDPOINT,
      e.cause,
      "merge evidence query failed",
    ),
  ),
);

export type TxStatusBatchRejectionRow = {
  readonly tx_hash: string;
  readonly reject_code: string;
  readonly reject_detail: string | null;
  readonly created_at: Date;
};

export type TxStatusBatchAdmissionRow = {
  readonly tx_hash: string;
  readonly status: TxAdmissionsDB.Status;
};

export type TxStatusBatchMembershipRow = {
  readonly tx_hash: string;
};

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
  if (txHashes.length === 0 || txHashes.length > 100) {
    return yield* HttpServerResponse.json(
      { error: "txHashes must contain 1 to 100 transaction hashes." },
      { status: 400 },
    );
  }
  const normalized = txHashes.map((txHash) => txHash.toLowerCase());
  const txIdBytes = normalized.map((txHash) => parseFixedHexParam(txHash, 32));
  const invalidIndex = txIdBytes.findIndex((txId) => txId === null);
  if (invalidIndex >= 0) {
    return yield* HttpServerResponse.json(
      { error: `Invalid transaction hash: [redacted]` },
      { status: 400 },
    );
  }
  const txIds = txIdBytes as Buffer[];
  const sql = yield* SqlClient;
  const globals = yield* Globals;
  const writer = yield* Effect.serviceOption(AdmissionWriter);
  const pendingTxIds = new Set(
    Option.isSome(writer)
      ? normalized.filter(
          (_, index) => writer.value.pendingSince(txIds[index]!) !== undefined,
        )
      : [],
  );
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
          CASE WHEN ${includePrivateHttpDiagnosticDetails()} THEN reject_detail END AS reject_detail,
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
        WHERE ${sql.in("tx_id", txIds)} AND included_by IS NULL`,
      sql<TxStatusBatchMembershipRow>`SELECT
          encode(tx_id, 'hex') AS tx_hash
        FROM processed_mempool
        WHERE ${sql.in("tx_id", txIds)} AND included_by IS NULL`,
      readTxStatusMergeEvidence(txIds),
    ],
    { concurrency: 2 },
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
  const admissionStatusByTxId = new Map<string, TxAdmissionsDB.Status>(
    admissionRows.map((row) => [row.tx_hash, row.status]),
  );
  for (const txHash of pendingTxIds) {
    if (!admissionStatusByTxId.has(txHash)) {
      admissionStatusByTxId.set(txHash, "queued");
    }
  }
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
  return yield* HttpServerResponse.json({
    results: results.map((status) => publicTxStatusPresentation(status)),
  });
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
