/**
 * Explicit HTTP route graph for the node's command server.
 * This module groups endpoint handlers and access control in one place while
 * delegating startup checks and response shaping to narrower modules.
 */
import { hexToBytes } from "@al-ft/midgard-core/hex";
import * as SDK from "@al-ft/midgard-sdk";
import {
  HttpRouter,
  HttpServerRequest,
  HttpServerResponse,
} from "@effect/platform";
import type { HttpBodyError } from "@effect/platform/HttpBody";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { SqlClient } from "@effect/sql/SqlClient";
import { toHex } from "@lucid-evolution/lucid";
import { Cause, Duration, Effect, Metric, Ref } from "effect";

import {
  parseAddressArgument,
  parseTxOutRefCborHex,
} from "@/commands/command-utils.js";
import * as DepositStatusCommand from "@/commands/deposit-status.js";
import {
  failWith500,
  handleStateQueueGetFailure,
} from "@/commands/listen-response.js";
import {
  authorizeAdminRoute,
  isAdminRoutePath,
  normalizeSubmitTxCanonicalCborToNative,
  validateSubmitTxCanonicalCbor,
} from "@/commands/listen-utils.js";
import * as ProtocolInfoCommand from "@/commands/protocol-info.js";
import { evaluateReadiness } from "@/commands/readiness.js";
import { resolveTxStatus, resolveTxStatusBatch } from "@/commands/tx-status.js";
import * as UtxosCommand from "@/commands/utxos.js";
import {
  AddressHistoryDB,
  BlocksDB,
  ImmutableDB,
  MempoolDB,
  MempoolLedgerDB,
  MutationJobsDB,
  ProcessedMempoolDB,
  StateQueueMutationLeasesDB,
  TxAdmissionsDB,
  TxRejectionsDB,
} from "@/database/index.js";
import {
  blockCommitmentAction,
  mergeAction,
  requestTxQueueProcessorWakeup,
} from "@/fibers/index.js";
import * as Genesis from "@/genesis.js";
import {
  localOgmiosSubmitSlotEvidence,
  readLocalOgmiosSubmitSlot,
} from "@/local-ogmios-slot.js";
import { Database } from "@/services/index.js";
import {
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import {
  fetchStateQueueTopologyProgram,
  formatStateQueueTopology,
} from "@/services/state-queue-topology.js";
import * as Initialization from "@/transactions/initialization.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
  referenceScriptTargetsByCommand,
} from "@/transactions/reference-scripts.js";
import {
  classifyOldestQueuedBlockReadiness,
  DEFAULT_MIN_QUEUE_LENGTH_FOR_MERGING,
  mergeMaturityWindow,
  planMergePreflight,
} from "@/transactions/state-queue/merge-readiness.js";
import * as SubmitDeposit from "@/transactions/submit-deposit.js";
import { SerializedStateQueueUTxO } from "@/workers/utils/commit-block-header.js";

const TX_ENDPOINT: string = "tx";
const ADDRESS_HISTORY_ENDPOINT: string = "txs";
const MERGE_ENDPOINT: string = "merge";
const UTXO_ENDPOINT: string = "utxo";
const UTXOS_ENDPOINT: string = "utxos";
const BLOCK_ENDPOINT: string = "block";
const INIT_ENDPOINT: string = "init";
const COMMIT_ENDPOINT: string = "commit";
const SUBMIT_ENDPOINT: string = "submit";
const DEPOSIT_BUILD_ENDPOINT: string = "deposit/build";
const STATE_QUEUE_ENDPOINT: string = "stateQueue";
const TX_STATUS_ENDPOINT: string = "tx-status";
const PIPELINE_STATUS_ENDPOINT: string = "pipeline-status";
const DEPOSIT_STATUS_ENDPOINT: string = "deposit-status";
const PROTOCOL_INFO_ENDPOINT: string = "protocol-info";
const HEALTH_ENDPOINT: string = "healthz";
const READINESS_ENDPOINT: string = "readyz";

const errorMessage = (error: unknown): string =>
  error instanceof Error ? error.message : String(error);
const STATE_QUEUE_MUTATION_LEASE_ENDPOINT: string = "stateQueueMutationLease";

const txCounter = Metric.counter("tx_count", {
  description: "A counter for tracking submit transactions",
  bigint: true,
  incremental: true,
});

const submitHandlerLatencyTimer = Metric.timer(
  "submit_handler_latency",
  "Latency of POST /submit handler responses in milliseconds",
);

const submitBodyReadDurationTimer = Metric.timer(
  "submit_body_read_duration",
  "Duration of POST /submit request body reads in milliseconds",
);

const submitNormalizeDurationTimer = Metric.timer(
  "submit_normalize_duration",
  "Duration of POST /submit canonical CBOR validation and normalization in milliseconds",
);

const submitDurableAdmissionDurationTimer = Metric.timer(
  "submit_durable_admission_duration",
  "Duration of POST /submit durable admission writes in milliseconds",
);

const submitResponseDurationTimer = Metric.timer(
  "submit_response_duration",
  "Duration of POST /submit response construction in milliseconds",
);

const submitQueueOfferFailureCounter = Metric.counter(
  "submit_queue_offer_failure_count",
  {
    description:
      "Number of POST /submit requests rejected because the queue was full",
    bigint: true,
    incremental: true,
  },
);

const isApplicationCbor = (contentType: string | undefined): boolean =>
  contentType?.split(";")[0]?.trim().toLowerCase() === "application/cbor";

const parseFixedHexParam = (
  value: unknown,
  byteLength: number,
): Buffer | null => {
  if (typeof value !== "string") {
    return null;
  }
  try {
    return hexToBytes(value, { byteLength, trim: false });
  } catch {
    return null;
  }
};

const parsePositiveInteger = (
  value: unknown,
  label: string,
): number | undefined => {
  if (value === undefined) {
    return undefined;
  }
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value <= 0) {
    throw new Error(`${label} must be a positive safe integer.`);
  }
  return value;
};

const encodeStateQueueMutationLease = (
  lease: StateQueueMutationLeasesDB.Entry | undefined,
  now: Date = new Date(),
) =>
  lease === undefined
    ? null
    : (() => {
        const expiresAt = lease[StateQueueMutationLeasesDB.Columns.EXPIRES_AT];
        const remainingMs = expiresAt.getTime() - now.getTime();
        return {
          token: lease[StateQueueMutationLeasesDB.Columns.TOKEN],
          holder: lease[StateQueueMutationLeasesDB.Columns.HOLDER],
          status: lease[StateQueueMutationLeasesDB.Columns.STATUS],
          acquiredAt:
            lease[StateQueueMutationLeasesDB.Columns.ACQUIRED_AT].toISOString(),
          expiresAt: expiresAt.toISOString(),
          releasedAt:
            lease[
              StateQueueMutationLeasesDB.Columns.RELEASED_AT
            ]?.toISOString() ?? null,
          lastError:
            lease[StateQueueMutationLeasesDB.Columns.LAST_ERROR] ?? null,
          remainingMs,
          expired: remainingMs < 0,
          blockedUntil: expiresAt.toISOString(),
        };
      })();

const encodeStateQueueMutationLeaseInspection = (
  inspection: StateQueueMutationLeasesDB.LeaseInspection,
) => ({
  status: inspection.activeLease === undefined ? "idle" : "busy",
  dbNow: inspection.dbNow.toISOString(),
  activeLease: encodeStateQueueMutationLease(
    inspection.activeLease,
    inspection.dbNow,
  ),
  pendingFinalizations: inspection.pendingFinalizations.map((entry) => ({
    headerHash: entry.headerHash,
    submittedTxHash: entry.submittedTxHash,
    status: entry.status,
    createdAt: entry.createdAt.toISOString(),
    updatedAt: entry.updatedAt.toISOString(),
  })),
  recentLeases: inspection.recentLeases.map((lease) =>
    encodeStateQueueMutationLease(lease, inspection.dbNow),
  ),
});

export type StateQueueMutationLeaseEndpointResult = {
  readonly statusCode: number;
  readonly body: unknown;
};

export type StateQueueMutationLeaseEndpointStore<R = Database> = {
  readonly inspect: (args?: {
    readonly recentLimit?: number;
  }) => Effect.Effect<StateQueueMutationLeasesDB.LeaseInspection, unknown, R>;
  readonly tryAcquire: (args: {
    readonly holder: string;
    readonly ttlMs?: number;
  }) => Effect.Effect<
    StateQueueMutationLeasesDB.LeaseAcquireResult,
    unknown,
    R
  >;
  readonly renew: (args: {
    readonly token: string;
    readonly ttlMs?: number;
  }) => Effect.Effect<void, unknown, R>;
  readonly release: (token: string) => Effect.Effect<void, unknown, R>;
  readonly markFailed: (
    token: string,
    error: string,
  ) => Effect.Effect<void, unknown, R>;
};

const defaultStateQueueMutationLeaseEndpointStore: StateQueueMutationLeaseEndpointStore =
  {
    inspect: StateQueueMutationLeasesDB.inspect,
    tryAcquire: StateQueueMutationLeasesDB.tryAcquire,
    renew: StateQueueMutationLeasesDB.renew,
    release: StateQueueMutationLeasesDB.release,
    markFailed: StateQueueMutationLeasesDB.markFailed,
  };

export const resolveStateQueueMutationLeaseRequest = <R = Database>(
  body: unknown,
  store: StateQueueMutationLeaseEndpointStore<R> = defaultStateQueueMutationLeaseEndpointStore as StateQueueMutationLeaseEndpointStore<R>,
): Effect.Effect<StateQueueMutationLeaseEndpointResult, never, R> =>
  Effect.gen(function* () {
    if (typeof body !== "object" || body === null || !("action" in body)) {
      return {
        statusCode: 400,
        body: { error: 'Request body must include an "action" field.' },
      };
    }
    const action = (body as { readonly action?: unknown }).action;
    if (action === "inspect") {
      let recentLimit: number | undefined;
      try {
        recentLimit = parsePositiveInteger(
          (body as { readonly recentLimit?: unknown }).recentLimit,
          "recentLimit",
        );
      } catch (error) {
        return {
          statusCode: 400,
          body: {
            error: errorMessage(error),
          },
        };
      }
      const inspection = yield* store.inspect(
        recentLimit === undefined ? undefined : { recentLimit },
      );
      return {
        statusCode: 200,
        body: encodeStateQueueMutationLeaseInspection(inspection),
      };
    }
    if (action === "acquire") {
      const holderValue = (body as { readonly holder?: unknown }).holder;
      const holder =
        typeof holderValue === "string" && holderValue.trim().length > 0
          ? holderValue.trim()
          : "fault_proof_removal";
      let ttlMs: number | undefined;
      try {
        ttlMs = parsePositiveInteger(
          (body as { readonly ttlMs?: unknown }).ttlMs,
          "ttlMs",
        );
      } catch (error) {
        return {
          statusCode: 400,
          body: {
            error: errorMessage(error),
          },
        };
      }
      const result = yield* store.tryAcquire({
        holder,
        ...(ttlMs === undefined ? {} : { ttlMs }),
      });
      if (result._tag === "Busy") {
        return {
          statusCode: 409,
          body: {
            status: "busy",
            activeLease: encodeStateQueueMutationLease(result.activeLease),
          },
        };
      }
      return {
        statusCode: 200,
        body: {
          status: "acquired",
          token: result.token,
        },
      };
    }

    const token = (body as { readonly token?: unknown }).token;
    if (typeof token !== "string" || token.trim().length === 0) {
      return {
        statusCode: 400,
        body: { error: '"token" must be a non-empty string.' },
      };
    }
    if (action === "renew") {
      let ttlMs: number | undefined;
      try {
        ttlMs = parsePositiveInteger(
          (body as { readonly ttlMs?: unknown }).ttlMs,
          "ttlMs",
        );
      } catch (error) {
        return {
          statusCode: 400,
          body: {
            error: errorMessage(error),
          },
        };
      }
      yield* store.renew({
        token: token.trim(),
        ...(ttlMs === undefined ? {} : { ttlMs }),
      });
      return { statusCode: 200, body: { status: "renewed" } };
    }
    if (action === "release") {
      yield* store.release(token.trim());
      return { statusCode: 200, body: { status: "released" } };
    }
    if (action === "fail") {
      const errorValue = (body as { readonly error?: unknown }).error;
      const error =
        typeof errorValue === "string"
          ? errorValue
          : "external state-queue mutation failed";
      yield* store.markFailed(token.trim(), error);
      return { statusCode: 200, body: { status: "failed" } };
    }

    return {
      statusCode: 400,
      body: { error: `Unsupported action: ${String(action)}` },
    };
  }).pipe(
    Effect.catchAll((error) =>
      Effect.succeed({
        statusCode: 500,
        body: {
          error: "State-queue mutation lease request failed.",
          detail: String(error),
        },
      }),
    ),
  );

/**
 * Wraps a route handler with admin-key authorization when the path belongs to
 * the admin-only route set.
 */
const withAdminAccess = <E, R>(
  endpoint: string,
  handler: Effect.Effect<HttpServerResponse.HttpServerResponse, E, R>,
): Effect.Effect<
  HttpServerResponse.HttpServerResponse,
  E | HttpBodyError,
  R | NodeConfig | HttpServerRequest.HttpServerRequest
> =>
  Effect.gen(function* () {
    const routePath = `/${endpoint}`;
    if (!isAdminRoutePath(routePath)) {
      return yield* handler;
    }
    const request = yield* HttpServerRequest.HttpServerRequest;
    const nodeConfig = yield* NodeConfig;
    const auth = authorizeAdminRoute(
      nodeConfig.ADMIN_API_KEY,
      request.headers["x-midgard-admin-key"],
    );
    if (!auth.authorized) {
      yield* Effect.logWarning(
        `Denied admin route ${routePath}: ${auth.error} (${auth.status})`,
      );
      return yield* HttpServerResponse.json(
        { error: auth.error },
        { status: auth.status },
      );
    }
    return yield* handler;
  });

/**
 * `GET /tx`: returns the CBOR for a known tx hash from mempool or immutable
 * storage.
 */
const getTxHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const txHashParam = params["tx_hash"];
  const txHashBytes = parseFixedHexParam(txHashParam, 32);
  if (txHashBytes === null) {
    yield* Effect.logInfo(
      `GET /${TX_ENDPOINT} - Invalid transaction hash: ${txHashParam}`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid transaction hash: ${txHashParam}` },
      { status: 404 },
    );
  }
  yield* Effect.logInfo("txHashBytes", txHashBytes);
  const foundCbor: Buffer = yield* MempoolDB.retrieveTxCborByHash(
    txHashBytes,
  ).pipe(
    Effect.catchAll((_e) =>
      Effect.gen(function* () {
        const fromImmutable =
          yield* ImmutableDB.retrieveTxCborByHash(txHashBytes);
        yield* Effect.logInfo(
          `GET /${TX_ENDPOINT} - Transaction found in ImmutableDB: ${txHashParam}`,
        );
        return fromImmutable;
      }),
    ),
  );
  yield* Effect.logInfo(
    `GET /${TX_ENDPOINT} - Transaction found in mempool: ${txHashParam}`,
  );
  yield* Effect.logInfo("foundCbor", SDK.bufferToHex(foundCbor));
  return yield* HttpServerResponse.json({
    tx: SDK.bufferToHex(foundCbor),
  });
}).pipe(
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
 * `GET /utxos`: returns spendable mempool-ledger UTxOs for an address.
 */
const getUtxosHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const addr = params["address"];

  if (typeof addr !== "string") {
    yield* Effect.logInfo(
      `GET /${UTXOS_ENDPOINT} - Invalid address type: ${addr}`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid address type: ${addr}` },
      { status: 400 },
    );
  }
  try {
    const address = parseAddressArgument(addr);

    const utxosWithAddress =
      yield* MempoolLedgerDB.retrieveSpendableByAddress(address);
    const response = UtxosCommand.encodeStoredUtxos(utxosWithAddress);

    yield* Effect.logInfo(`Found ${response.length} UTxOs for ${addr}`);
    return yield* HttpServerResponse.json({
      utxos: response,
    });
  } catch (_error) {
    yield* Effect.logInfo(`Invalid address: ${addr}`);
    return yield* HttpServerResponse.json(
      { error: `Invalid address: ${addr}` },
      { status: 400 },
    );
  }
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", UTXOS_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      UTXOS_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `GET /utxo`: returns one spendable mempool-ledger UTxO by raw TxOutRef CBOR
 * hex.
 */
const getUtxoHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const rawTxOutRef = params["txOutRef"];

  let txOutRef: Buffer;
  try {
    txOutRef = parseTxOutRefCborHex(rawTxOutRef, "txOutRef");
  } catch (error) {
    const message = errorMessage(error);
    yield* Effect.logInfo(
      `GET /${UTXO_ENDPOINT} - invalid txOutRef: ${message}`,
    );
    return yield* HttpServerResponse.json({ error: message }, { status: 400 });
  }

  const matched = yield* UtxosCommand.utxosByTxOutRefsProgram([txOutRef]);
  if (matched.length === 0) {
    return yield* HttpServerResponse.json(
      { error: `UTxO not found for txOutRef ${txOutRef.toString("hex")}` },
      { status: 404 },
    );
  }

  return yield* HttpServerResponse.json({
    utxo: UtxosCommand.encodeStoredUtxo(matched[0]),
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", UTXO_ENDPOINT, e)),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      UTXO_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `POST /utxos?by-outrefs`: returns spendable mempool-ledger UTxOs for a
 * requested list of `txHash#outputIndex` identifiers.
 */
const postUtxosByTxOutRefsHandler = Effect.gen(function* () {
  const request = yield* HttpServerRequest.HttpServerRequest;
  const params = yield* ParsedSearchParams;

  try {
    UtxosCommand.requireByOutRefsSelector(params);
  } catch (error) {
    const message = errorMessage(error);
    yield* Effect.logInfo(
      `POST /${UTXOS_ENDPOINT} - missing selector: ${message}`,
    );
    return yield* HttpServerResponse.json({ error: message }, { status: 400 });
  }

  const parsedBody = yield* Effect.either(request.json);
  if (parsedBody._tag === "Left") {
    yield* Effect.logInfo(
      `POST /${UTXOS_ENDPOINT} - invalid JSON request body`,
    );
    return yield* HttpServerResponse.json(
      { error: "Request body must be valid JSON." },
      { status: 400 },
    );
  }

  let txOutRefs: readonly Buffer[];
  try {
    txOutRefs = UtxosCommand.parseTxOutRefsRequest(parsedBody.right);
  } catch (error) {
    const message = errorMessage(error);
    yield* Effect.logInfo(
      `POST /${UTXOS_ENDPOINT} - invalid request: ${message}`,
    );
    return yield* HttpServerResponse.json({ error: message }, { status: 400 });
  }

  const matched = yield* UtxosCommand.utxosByTxOutRefsProgram(txOutRefs);
  return yield* HttpServerResponse.json({
    utxos: UtxosCommand.encodeStoredUtxos(matched),
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("POST", UTXOS_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      UTXOS_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `GET /tx-status`: resolves the node's canonical status for a tx hash.
 */
const getTxStatusHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const txHashParam = params["tx_hash"];
  const txHashBytes = parseFixedHexParam(txHashParam, 32);
  if (txHashBytes === null) {
    return yield* HttpServerResponse.json(
      { error: `Invalid transaction hash: ${txHashParam}` },
      { status: 400 },
    );
  }

  const globals = yield* Globals;
  const rejected = yield* TxRejectionsDB.retrieveByTxId(txHashBytes);
  const admission = yield* TxAdmissionsDB.getByTxId(txHashBytes);
  const inImmutable = yield* ImmutableDB.retrieveTxCborsByHashes([txHashBytes]);
  const inMempool = yield* MempoolDB.retrieveTxCborsByHashes([txHashBytes]);
  const inProcessedMempool = yield* ProcessedMempoolDB.retrieveTxCborsByHashes([
    txHashBytes,
  ]);

  const resolved = resolveTxStatus({
    txIdHex: txHashParam as string,
    rejection:
      rejected.length > 0
        ? {
            rejectCode: rejected[0].reject_code,
            rejectDetail: rejected[0].reject_detail,
            createdAtIso: rejected[0].created_at.toISOString(),
          }
        : null,
    admissionStatus: admission?.status ?? null,
    inImmutable: inImmutable.length > 0,
    inMempool: inMempool.length > 0,
    inProcessedMempool: inProcessedMempool.length > 0,
    localFinalizationPending: yield* Ref.get(
      globals.LOCAL_FINALIZATION_PENDING,
    ),
  });

  if (resolved.status === "not_found") {
    return yield* HttpServerResponse.json(resolved, { status: 404 });
  }

  return yield* HttpServerResponse.json(resolved);
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", TX_STATUS_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      TX_STATUS_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

type TxStatusBatchRejectionRow = {
  readonly tx_hash: string;
  readonly reject_code: string;
  readonly reject_detail: string | null;
  readonly created_at: Date;
};

type TxStatusBatchAdmissionRow = {
  readonly tx_hash: string;
  readonly status: TxAdmissionsDB.Status;
};

type TxStatusBatchMembershipRow = {
  readonly tx_hash: string;
};

type TxStatusBatchHeaderRow = {
  readonly tx_hash: string;
  readonly header_hash: string;
  readonly status: string;
};

const postTxStatusBatchHandler = Effect.gen(function* () {
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
    headerRows,
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
      sql<TxStatusBatchHeaderRow>`SELECT
          encode(member.member_id, 'hex') AS tx_hash,
          encode(member.header_hash, 'hex') AS header_hash,
          pending.status
        FROM pending_block_finalization_txs AS member
        JOIN pending_block_finalizations AS pending
          ON pending.header_hash = member.header_hash
        WHERE ${sql.in("member.member_id", txIds)}`,
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
  const headerEvidenceByTxId = new Map(
    headerRows.map((row) => [
      row.tx_hash,
      {
        headerHash: row.header_hash,
        headerStatus: row.status,
        mergeStatus: row.status === "finalized" ? "finalized" : "not_finalized",
        confirmedLedgerFinalized: row.status === "finalized",
      },
    ]),
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
 * `POST /stateQueueMutationLease`: admin-only coordination endpoint for
 * external state-queue mutators such as manual fault-proof removal.
 */
const postStateQueueMutationLeaseHandler = Effect.gen(function* () {
  const request = yield* HttpServerRequest.HttpServerRequest;
  const parsedBody = yield* Effect.either(request.json);
  if (parsedBody._tag === "Left") {
    return yield* HttpServerResponse.json(
      { error: "Request body must be valid JSON." },
      { status: 400 },
    );
  }
  const result = yield* resolveStateQueueMutationLeaseRequest(parsedBody.right);
  return yield* HttpServerResponse.json(result.body, {
    status: result.statusCode,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("POST", STATE_QUEUE_MUTATION_LEASE_ENDPOINT, e),
  ),
);

const getStateQueueMutationLeaseHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const recentLimitParam = params["recent_limit"];
  let recentLimit: number | undefined;
  try {
    recentLimit =
      recentLimitParam === undefined
        ? undefined
        : parsePositiveInteger(Number(recentLimitParam), "recent_limit");
  } catch (error) {
    return yield* HttpServerResponse.json(
      { error: errorMessage(error) },
      { status: 400 },
    );
  }
  const inspection = yield* StateQueueMutationLeasesDB.inspect(
    recentLimit === undefined ? undefined : { recentLimit },
  );
  return yield* HttpServerResponse.json(
    encodeStateQueueMutationLeaseInspection(inspection),
  );
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", STATE_QUEUE_MUTATION_LEASE_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      STATE_QUEUE_MUTATION_LEASE_ENDPOINT,
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `GET /deposit-status`: returns one serialized deposit row by event id or L1
 * tx hash.
 */
const getDepositStatusHandler = Effect.gen(function* () {
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
const getHealthHandler = Effect.gen(function* () {
  return yield* HttpServerResponse.json({
    status: "ok",
    now: new Date().toISOString(),
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", HEALTH_ENDPOINT, e),
  ),
);

/**
 * `GET /readyz`: readiness endpoint that checks worker heartbeats, queue depth,
 * local recovery state, and database connectivity.
 */
const getReadinessHandler = Effect.gen(function* () {
  const globals = yield* Globals;
  const nodeConfig = yield* NodeConfig;
  const sql = yield* SqlClient;

  const durableAdmissionBacklog = yield* TxAdmissionsDB.countBacklog;
  const durableAdmissionOldestAgeMs = yield* TxAdmissionsDB.oldestQueuedAgeMs;
  const unfinishedMutationJobs = yield* MutationJobsDB.countUnfinished;
  const mempoolTxCount = yield* MempoolDB.retrieveTxCount;
  const nowMillis = Date.now();
  const stateQueueBlocksInQueue = yield* Ref.get(globals.BLOCKS_IN_QUEUE);
  const resetInProgress = yield* Ref.get(globals.RESET_IN_PROGRESS);
  const commitWorkerActive = yield* Ref.get(globals.COMMIT_WORKER_ACTIVE);
  const commitPipelinePhase = yield* Ref.get(globals.COMMIT_PIPELINE_PHASE);
  const blockCommitmentHeartbeat = yield* Ref.get(
    globals.HEARTBEAT_BLOCK_COMMITMENT,
  );
  const blockConfirmationHeartbeat = yield* Ref.get(
    globals.HEARTBEAT_BLOCK_CONFIRMATION,
  );
  const mergeHeartbeat = yield* Ref.get(globals.HEARTBEAT_MERGE);
  const depositFetchHeartbeat = yield* Ref.get(globals.HEARTBEAT_DEPOSIT_FETCH);
  const withdrawalFetchHeartbeat = yield* Ref.get(
    globals.HEARTBEAT_WITHDRAWAL_FETCH,
  );
  const txQueueProcessorHeartbeat = yield* Ref.get(
    globals.HEARTBEAT_TX_QUEUE_PROCESSOR,
  );
  const localFinalizationPending = yield* Ref.get(
    globals.LOCAL_FINALIZATION_PENDING,
  );
  const unconfirmedSubmittedBlockTxHash = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
  );
  const unconfirmedSubmittedBlockSinceMs = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
  );
  const unresolvedBlockSubmissionAgeMs =
    unconfirmedSubmittedBlockTxHash === "" ||
    unconfirmedSubmittedBlockSinceMs <= 0
      ? 0
      : nowMillis - unconfirmedSubmittedBlockSinceMs;

  const dbProbe = yield* Effect.either(sql`SELECT 1 AS ok`);
  const dbHealthy = dbProbe._tag === "Right";
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const providerProbe = yield* Effect.either(
    Initialization.fetchHubOracleWitness(lucid.api, contracts),
  );
  const localOgmiosSlotProbe = yield* Effect.either(
    readLocalOgmiosSubmitSlot({
      ogmiosUrl: nodeConfig.L1_OGMIOS_KEY,
      timeoutMs: nodeConfig.L1_PROVIDER_PREFLIGHT_TIMEOUT_MS,
    }),
  );
  const leaseInspection = yield* StateQueueMutationLeasesDB.inspect({
    recentLimit: 3,
  });
  const encodedLeaseInspection =
    encodeStateQueueMutationLeaseInspection(leaseInspection);
  const activeLease = leaseInspection.activeLease;
  const activeLeaseRemainingMs =
    activeLease === undefined
      ? null
      : activeLease[StateQueueMutationLeasesDB.Columns.EXPIRES_AT].getTime() -
        leaseInspection.dbNow.getTime();

  const baseReadiness = evaluateReadiness({
    nowMillis,
    maxHeartbeatAgeMs: nodeConfig.READINESS_MAX_HEARTBEAT_AGE_MS,
    maxQueueDepth: nodeConfig.READINESS_MAX_DURABLE_ADMISSION_BACKLOG,
    queueDepth: Number(durableAdmissionBacklog),
    workerHeartbeats: {
      blockCommitment: blockCommitmentHeartbeat,
      blockConfirmation: blockConfirmationHeartbeat,
      merge: mergeHeartbeat,
      depositFetch: depositFetchHeartbeat,
      withdrawalFetch: withdrawalFetchHeartbeat,
      txQueueProcessor: txQueueProcessorHeartbeat,
    },
    localFinalizationPending,
    unresolvedBlockSubmissionAgeMs,
    maxUnresolvedBlockSubmissionAgeMs: nodeConfig.UNCONFIRMED_BLOCK_MAX_AGE_MS,
    dbHealthy,
    stateQueueMutationLease: {
      active: activeLease !== undefined,
      stale:
        activeLeaseRemainingMs !== null &&
        activeLeaseRemainingMs <
          -nodeConfig.STATE_QUEUE_MUTATION_LEASE_STALE_GRACE_MS,
      remainingMs: activeLeaseRemainingMs,
      holder: activeLease?.[StateQueueMutationLeasesDB.Columns.HOLDER] ?? null,
    },
  });
  const reasons = [...baseReadiness.reasons];
  if (providerProbe._tag === "Left") {
    reasons.push("provider_query_unhealthy:hub-oracle");
  }
  if (localOgmiosSlotProbe._tag === "Left") {
    reasons.push("provider_query_unhealthy:local-ogmios-slot");
  }
  if (
    durableAdmissionOldestAgeMs >
    nodeConfig.READINESS_MAX_DURABLE_ADMISSION_AGE_MS
  ) {
    reasons.push(
      `durable_admission_oldest_age_exceeded:${durableAdmissionOldestAgeMs}:${nodeConfig.READINESS_MAX_DURABLE_ADMISSION_AGE_MS}`,
    );
  }
  if (unfinishedMutationJobs > 0n) {
    reasons.push(
      `unfinished_local_mutation_jobs:${unfinishedMutationJobs.toString()}`,
    );
  }
  const mergeReadiness = planMergePreflight({
    force: false,
    queueLength: stateQueueBlocksInQueue,
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
  const readiness = {
    ready: reasons.length === 0,
    reasons,
    durableAdmissionBacklog: durableAdmissionBacklog.toString(),
    durableAdmissionOldestAgeMs,
    mempoolTxCount: mempoolTxCount.toString(),
    unfinishedLocalMutationJobs: unfinishedMutationJobs.toString(),
    unresolvedBlockSubmissionAgeMs,
    providerQueryHealthy: providerProbe._tag === "Right",
    localOgmiosSlot:
      localOgmiosSlotProbe._tag === "Right"
        ? {
            ...localOgmiosSlotProbe.right,
            evidence: localOgmiosSubmitSlotEvidence(localOgmiosSlotProbe.right),
          }
        : {
            error: String(localOgmiosSlotProbe.left),
          },
    stateQueueMutationLease: encodedLeaseInspection,
    blockCommitmentCoordination: {
      commitWorkerActive,
      commitPipelinePhase,
    },
    mergeReadiness,
  };

  return yield* HttpServerResponse.json(readiness, {
    status: readiness.ready ? 200 : 503,
  });
});

type PipelineStatusCountRow = {
  readonly status: string;
  readonly count: bigint | number | string;
};

type PipelineStatusOldestActiveRow = {
  readonly header_hash: string;
  readonly submitted_tx_hash: string | null;
  readonly status: string;
  readonly created_at: Date;
  readonly updated_at: Date;
  readonly observed_confirmed_at_ms: bigint | number | string | null;
};

type PipelineStatusCountOnlyRow = {
  readonly count: bigint | number | string;
};

const bigintString = (value: bigint | number | string): string =>
  BigInt(value).toString();

const getPipelineStatusHandler = Effect.gen(function* () {
  const globals = yield* Globals;
  const sql = yield* SqlClient;
  const now = new Date();
  const [
    pendingCounts,
    oldestActiveRows,
    durableAdmissionBacklog,
    mempoolTxCountRows,
    processedMempoolTxCountRows,
    unfinishedMutationJobs,
    leaseInspection,
  ] = yield* Effect.all(
    [
      sql<PipelineStatusCountRow>`SELECT
          status,
          COUNT(*)::bigint AS count
        FROM pending_block_finalizations
        GROUP BY status
        ORDER BY status`,
      sql<PipelineStatusOldestActiveRow>`SELECT
          encode(header_hash, 'hex') AS header_hash,
          encode(submitted_tx_hash, 'hex') AS submitted_tx_hash,
          status,
          created_at,
          updated_at,
          observed_confirmed_at_ms
        FROM pending_block_finalizations
        WHERE status IN ('prepared', 'submitted', 'confirmed')
        ORDER BY created_at ASC
        LIMIT 1`,
      TxAdmissionsDB.countBacklog,
      sql<PipelineStatusCountOnlyRow>`SELECT COUNT(*)::bigint AS count FROM mempool`,
      sql<PipelineStatusCountOnlyRow>`SELECT COUNT(*)::bigint AS count FROM processed_mempool`,
      MutationJobsDB.countUnfinished,
      StateQueueMutationLeasesDB.inspect({ recentLimit: 5 }),
    ],
    { concurrency: "unbounded" },
  );
  const queueLength = yield* Ref.get(globals.BLOCKS_IN_QUEUE);
  const localFinalizationPending = yield* Ref.get(
    globals.LOCAL_FINALIZATION_PENDING,
  );
  const unconfirmedSubmittedBlockTxHash = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH,
  );
  const unconfirmedSubmittedBlockSinceMs = yield* Ref.get(
    globals.UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS,
  );
  const oldestActive = oldestActiveRows[0];
  return yield* HttpServerResponse.json({
    status: "ok",
    now: now.toISOString(),
    finalityVocabulary: {
      txStatusCommittedMeaning:
        "immutable_db_inclusion_not_confirmed_ledger_merge",
      confirmedDrainMeaning:
        "containing state-queue header merged to confirmed ledger and locally finalized after L1 confirmation",
    },
    durableAdmission: {
      backlog: durableAdmissionBacklog.toString(),
    },
    localResidue: {
      mempoolTxCount: bigintString(mempoolTxCountRows[0]?.count ?? 0),
      processedMempoolTxCount: bigintString(
        processedMempoolTxCountRows[0]?.count ?? 0,
      ),
    },
    pendingBlockFinalizations: {
      countsByStatus: Object.fromEntries(
        pendingCounts.map((row) => [row.status, bigintString(row.count)]),
      ),
      oldestActive:
        oldestActive === undefined
          ? null
          : {
              headerHash: oldestActive.header_hash,
              submittedTxHash: oldestActive.submitted_tx_hash,
              status: oldestActive.status,
              ageMs: Math.max(
                0,
                now.getTime() - oldestActive.created_at.getTime(),
              ),
              createdAt: oldestActive.created_at.toISOString(),
              updatedAt: oldestActive.updated_at.toISOString(),
              observedConfirmedAt:
                oldestActive.observed_confirmed_at_ms === null
                  ? null
                  : new Date(
                      Number(oldestActive.observed_confirmed_at_ms),
                    ).toISOString(),
            },
    },
    stateQueue: {
      queueLength,
      unconfirmedSubmittedBlockTxHash:
        unconfirmedSubmittedBlockTxHash === ""
          ? null
          : unconfirmedSubmittedBlockTxHash,
      unconfirmedSubmittedBlockAgeMs:
        unconfirmedSubmittedBlockTxHash === "" ||
        unconfirmedSubmittedBlockSinceMs <= 0
          ? 0
          : Math.max(0, now.getTime() - unconfirmedSubmittedBlockSinceMs),
      localFinalizationPending,
    },
    stateQueueMutationLease:
      encodeStateQueueMutationLeaseInspection(leaseInspection),
    localMutationJobs: {
      unfinished: unfinishedMutationJobs.toString(),
    },
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", PIPELINE_STATUS_ENDPOINT, e),
  ),
  Effect.catchTag("SqlError", (e) =>
    failWith500(
      "GET",
      PIPELINE_STATUS_ENDPOINT,
      e.cause,
      "pipeline status query failed",
    ),
  ),
);

/**
 * `GET /protocol-info`: returns stable public facts needed by external
 * Midgard transaction builders.
 */
const getProtocolInfoHandler = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;
  const lucid = yield* Lucid;
  const response = yield* Effect.try({
    try: () =>
      ProtocolInfoCommand.encodeProtocolInfo({
        nodeConfig,
        currentSlot: lucid.api.currentSlot(),
      }),
    catch: (error) => error,
  });
  return yield* HttpServerResponse.json(response);
}).pipe(Effect.catchAll((e) => failWith500("GET", PROTOCOL_INFO_ENDPOINT, e)));

/**
 * `GET /block`: returns tx hashes referenced by a committed block header.
 */
const getBlockHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const hdrHash = params["header_hash"];
  yield* Effect.logInfo(
    `GET /block - Request received for header_hash: ${hdrHash}`,
  );

  const headerHash = parseFixedHexParam(hdrHash, 28);
  if (headerHash === null) {
    yield* Effect.logInfo(
      `GET /${BLOCK_ENDPOINT} - Invalid block hash: ${hdrHash}`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid block hash: ${hdrHash}` },
      { status: 400 },
    );
  }
  const hashes = yield* BlocksDB.retrieveTxHashesByHeaderHash(headerHash);
  yield* Effect.logInfo(
    `GET /${BLOCK_ENDPOINT} - Found ${hashes.length} txs for block: ${hdrHash}`,
  );
  return yield* HttpServerResponse.json({
    hashes: hashes.map(SDK.bufferToHex),
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
 * `GET /init`: initializes protocol state when startup policy and on-chain
 * topology allow it.
 */
const getInitHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✨ Initialization request received`);
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const topology = yield* fetchStateQueueTopologyProgram(
    lucid.api,
    contracts.stateQueue,
  );
  if (topology.initialized) {
    const details = formatStateQueueTopology(topology);
    if (!topology.healthy) {
      yield* Effect.logWarning(
        `GET /${INIT_ENDPOINT} - Refusing to initialize over invalid state_queue topology (${details}): ${topology.reason ?? "unknown"}`,
      );
      return yield* HttpServerResponse.json(
        {
          error:
            "Cannot initialize: configured state_queue policy already has invalid on-chain topology",
          details,
          reason: topology.reason ?? "unknown",
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

  const txHash = yield* Initialization.program;
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
const getCommitEndpoint = Effect.gen(function* () {
  yield* Effect.logInfo(
    `GET /${COMMIT_ENDPOINT} - Manual block commitment order received`,
  );
  const result = yield* blockCommitmentAction;
  yield* Effect.logInfo(
    `GET /${COMMIT_ENDPOINT} - Block commitment successful: ${result}`,
  );
  return yield* HttpServerResponse.json({
    message: `Block commitment successful: ${result}`,
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
const getMergeHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`GET /${MERGE_ENDPOINT} - Manual merge order received`);
  const result = yield* mergeAction(true);
  yield* Effect.logInfo(
    `GET /${MERGE_ENDPOINT} - Merge result: ${JSON.stringify(result)}`,
  );
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
const getTxsOfAddressHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const addr = params["address"];

  if (typeof addr !== "string") {
    yield* Effect.logInfo(
      `GET /${ADDRESS_HISTORY_ENDPOINT} - Invalid address type: ${addr}`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid address type: ${addr}` },
      { status: 400 },
    );
  }
  try {
    const address = parseAddressArgument(addr);
    const cbors = yield* AddressHistoryDB.retrieve(address);
    yield* Effect.logInfo(`Found ${cbors.length} CBORs with ${addr}`);
    return yield* HttpServerResponse.json({
      txs: cbors.map(SDK.bufferToHex),
    });
  } catch (_error) {
    yield* Effect.logInfo(`Invalid address: ${addr}`);
    return yield* HttpServerResponse.json(
      { error: `Invalid address: ${addr}` },
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
const getStateQueueHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✍  Drawing state queue UTxOs...`);
  const globals = yield* Globals;
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
  const nodeConfig = yield* NodeConfig;
  const fetchConfig: SDK.StateQueueFetchConfig = {
    stateQueuePolicyId: contracts.stateQueue.policyId,
    stateQueueAddress: contracts.stateQueue.spendingScriptAddress,
  };
  const sortedUTxOs = yield* SDK.fetchSortedStateQueueUTxOsProgram(
    lucid.api,
    fetchConfig,
  );
  const headers = sortedUTxOs.flatMap((u) =>
    u.datum.key === "Empty" ? [] : [u.datum.key.Key.key],
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
            Number(blockHeader.endTime),
          );
          return classifyOldestQueuedBlockReadiness({
            headerHash,
            currentDaAttestation: stateQueueNode.da_attestation,
            requiredDaAttestation: contracts.daAttestation.policyId,
            readyAfterUnixTime: maturity.readyAfterUnixTime,
            nowUnixTime: Date.now(),
          });
        });
  let drawn = `
---------------------------- STATE QUEUE ----------------------------`;
  yield* Effect.allSuccesses(
    sortedUTxOs.map((u) =>
      Effect.gen(function* () {
        let info = "";
        const isHead = u.datum.key === "Empty";
        const isEnd = u.datum.next === "Empty";
        const emoji = isHead ? "🚢" : isEnd ? "⚓" : "⛓ ";
        if (u.datum.key !== "Empty") {
          const icon = isEnd ? "  " : emoji;
          info = `
${icon} ╰─ header: ${u.datum.key.Key.key}`;
        }
        drawn = `${drawn}
${emoji} ${u.utxo.txHash}#${u.utxo.outputIndex}${info}`;
      }),
    ),
  );
  drawn += `
---------------------------------------------------------------------
`;
  yield* Effect.logInfo(drawn);
  return yield* HttpServerResponse.json({
    headers,
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
  Effect.catchTag("LinkedListError", (e) =>
    failWith500("GET", "logStateQueue", e.cause, e.message),
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
  Effect.catchTag("LucidError", (e) =>
    failWith500("GET", "logStateQueue", e.cause, e.message),
  ),
);

/**
 * `GET /logBlocksDB`: logs a compact summary of block-link rows.
 */
const getLogBlocksDBHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✍  Querying BlocksDB...`);
  const allBlocksData = yield* BlocksDB.retrieve;
  const keyValues: Record<string, number> = allBlocksData.reduce(
    (acc: Record<string, number>, entry) => {
      const bHex = toHex(entry.header_hash);
      if (!acc[bHex]) {
        acc[bHex] = 1;
      } else {
        acc[bHex] += 1;
      }
      return acc;
    },
    {} as Record<string, number>,
  );
  let drawn = `
------------------------------ BLOCKS DB ----------------------------`;
  for (const bHex in keyValues) {
    drawn = `${drawn}
${bHex} -──▶ ${keyValues[bHex]} tx(s)`;
  }
  drawn += `
---------------------------------------------------------------------
`;
  yield* Effect.logInfo(drawn);
  return yield* HttpServerResponse.json({
    message: `BlocksDB drawn in server logs!`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "logBlocksDB", e)),
  Effect.catchTag("DatabaseError", (e) =>
    failWith500(
      "GET",
      "logBlocksDB",
      e.cause,
      `db failure with table ${e.table}`,
    ),
  ),
);

/**
 * `GET /logGlobals`: logs the current process-global coordination state.
 */
const getLogGlobalsHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✍  Logging global variables...`);
  const globals = yield* Globals;
  const BLOCKS_IN_QUEUE: number = yield* Ref.get(globals.BLOCKS_IN_QUEUE);
  const LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH: number = yield* Ref.get(
    globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
  );
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
  const HEARTBEAT_DEPOSIT_FETCH: number = yield* Ref.get(
    globals.HEARTBEAT_DEPOSIT_FETCH,
  );
  const HEARTBEAT_WITHDRAWAL_FETCH: number = yield* Ref.get(
    globals.HEARTBEAT_WITHDRAWAL_FETCH,
  );
  const HEARTBEAT_TX_QUEUE_PROCESSOR: number = yield* Ref.get(
    globals.HEARTBEAT_TX_QUEUE_PROCESSOR,
  );

  yield* Effect.logInfo(`
  BLOCKS_IN_QUEUE ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${BLOCKS_IN_QUEUE}
  LATEST_SYNC ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${new Date(Number(LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH)).toLocaleString()}
  RESET_IN_PROGRESS ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${RESET_IN_PROGRESS}
  COMMIT_WORKER_ACTIVE ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${COMMIT_WORKER_ACTIVE}
  COMMIT_PIPELINE_PHASE ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${COMMIT_PIPELINE_PHASE}
  AVAILABLE_CONFIRMED_BLOCK ⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${JSON.stringify(AVAILABLE_CONFIRMED_BLOCK)}
  PROCESSED_UNSUBMITTED_TXS_COUNT ⋅⋅⋅ ${PROCESSED_UNSUBMITTED_TXS_COUNT}
  PROCESSED_UNSUBMITTED_TXS_SIZE ⋅⋅⋅⋅ ${PROCESSED_UNSUBMITTED_TXS_SIZE}
  UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH ⋅⋅⋅⋅⋅⋅⋅ ${UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH}
  UNCONFIRMED_SUBMITTED_BLOCK_SINCE ⋅⋅⋅⋅⋅⋅⋅ ${UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS > 0 ? new Date(Number(UNCONFIRMED_SUBMITTED_BLOCK_SINCE_MS)).toLocaleString() : "N/A"} (${unconfirmedSubmittedBlockAgeMs}ms)
  LOCAL_FINALIZATION_PENDING ⋅⋅⋅⋅⋅⋅⋅⋅ ${LOCAL_FINALIZATION_PENDING}
  HEARTBEAT_BLOCK_COMMITMENT ⋅⋅ ${new Date(Number(HEARTBEAT_BLOCK_COMMITMENT)).toLocaleString()}
  HEARTBEAT_BLOCK_CONFIRMATION ⋅ ${new Date(Number(HEARTBEAT_BLOCK_CONFIRMATION)).toLocaleString()}
  HEARTBEAT_MERGE ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${new Date(Number(HEARTBEAT_MERGE)).toLocaleString()}
  HEARTBEAT_DEPOSIT_FETCH ⋅⋅⋅⋅ ${new Date(Number(HEARTBEAT_DEPOSIT_FETCH)).toLocaleString()}
  HEARTBEAT_WITHDRAWAL_FETCH ⋅ ${new Date(Number(HEARTBEAT_WITHDRAWAL_FETCH)).toLocaleString()}
  HEARTBEAT_TX_QUEUE_PROCESSOR ⋅ ${new Date(Number(HEARTBEAT_TX_QUEUE_PROCESSOR)).toLocaleString()}
`);
  return yield* HttpServerResponse.json({
    message: `Global variables logged!`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "logGlobals", e)),
);

/**
 * `POST /deposit/build`: builds an unsigned L1 deposit transaction from a
 * caller-supplied wallet view and returns the CBOR for external signing.
 */
const postDepositBuildHandler = Effect.gen(function* () {
  const request = yield* HttpServerRequest.HttpServerRequest;
  const parsedBody = yield* Effect.either(request.json);
  if (parsedBody._tag === "Left") {
    yield* Effect.logInfo(
      `POST /${DEPOSIT_BUILD_ENDPOINT} - invalid JSON request body`,
    );
    return yield* HttpServerResponse.json(
      { error: "Request body must be valid JSON." },
      { status: 400 },
    );
  }

  const lucid = yield* Lucid;
  let buildRequest: SubmitDeposit.BuildDepositRequest;
  try {
    buildRequest = SubmitDeposit.parseBuildDepositRequest(parsedBody.right, {
      expectedNetwork: lucid.api.config().network,
    });
  } catch (error) {
    const message = errorMessage(error);
    yield* Effect.logInfo(
      `POST /${DEPOSIT_BUILD_ENDPOINT} - invalid request: ${message}`,
    );
    return yield* HttpServerResponse.json({ error: message }, { status: 400 });
  }

  const contracts = yield* MidgardContracts;
  const depositReferenceScripts = yield* fetchReferenceScriptUtxosProgram(
    lucid.api,
    lucid.referenceScriptsAddress,
    referenceScriptTargetsByCommand(contracts).deposit,
    contracts.referenceScriptAuth,
  ).pipe(
    Effect.map((resolved) => ({
      depositMinting: referenceScriptByName(resolved, "deposit minting"),
    })),
  );
  const built =
    yield* SubmitDeposit.buildUnsignedDepositTxFromFundingContextProgram(
      lucid.api,
      contracts,
      { ...buildRequest, referenceScripts: depositReferenceScripts },
    );
  return yield* HttpServerResponse.json(built);
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("POST", DEPOSIT_BUILD_ENDPOINT, e),
  ),
  Effect.catchTag("SubmitDepositError", (e) =>
    failWith500("POST", DEPOSIT_BUILD_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("StateQueueError", (e) =>
    failWith500("POST", DEPOSIT_BUILD_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("HubOracleError", (e) =>
    failWith500("POST", DEPOSIT_BUILD_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("LucidError", (e) =>
    failWith500("POST", DEPOSIT_BUILD_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("Bech32DeserializationError", (e) =>
    failWith500("POST", DEPOSIT_BUILD_ENDPOINT, e.cause, e.message),
  ),
  Effect.catchTag("HashingError", (e) =>
    failWith500("POST", DEPOSIT_BUILD_ENDPOINT, e.cause, e.message),
  ),
);

/**
 * `POST /submit`: validates, normalizes, and enqueues a submitted L2
 * transaction.
 */
const postSubmitHandler = (withMonitoring?: boolean) =>
  Effect.gen(function* () {
    const startedAt = withMonitoring === true ? Date.now() : 0;
    const recordLatency = () =>
      withMonitoring === true
        ? submitHandlerLatencyTimer(
            Effect.succeed(Duration.millis(Date.now() - startedAt)),
          )
        : Effect.void;
    return yield* Effect.gen(function* () {
      const nodeConfig = yield* NodeConfig;
      const request = yield* HttpServerRequest.HttpServerRequest;

      if (!isApplicationCbor(request.headers["content-type"])) {
        yield* Effect.logInfo(
          `▫️ Invalid submit payload: expected application/cbor`,
        );
        yield* recordLatency();
        return yield* HttpServerResponse.json(
          {
            error:
              "Request body must be raw Midgard canonical transaction CBOR with Content-Type application/cbor",
          },
          { status: 415 },
        );
      }

      const bodyReadStartedAt = Date.now();
      const bodyBytes = yield* Effect.either(request.arrayBuffer);
      yield* submitBodyReadDurationTimer(
        Effect.succeed(Duration.millis(Date.now() - bodyReadStartedAt)),
      );
      if (bodyBytes._tag === "Left") {
        yield* Effect.logInfo(
          `▫️ Submit rejected: failed to read request body`,
        );
        yield* recordLatency();
        return yield* HttpServerResponse.json(
          { error: "Invalid canonical transaction CBOR payload" },
          { status: 400 },
        );
      }

      const normalizeStartedAt = Date.now();
      const validation = validateSubmitTxCanonicalCbor(
        new Uint8Array(bodyBytes.right),
        nodeConfig.MAX_SUBMIT_TX_CBOR_BYTES,
      );
      if (!validation.ok) {
        yield* submitNormalizeDurationTimer(
          Effect.succeed(Duration.millis(Date.now() - normalizeStartedAt)),
        );
        yield* Effect.logInfo(`▫️ Submit rejected: ${validation.error}`);
        yield* recordLatency();
        return yield* HttpServerResponse.json(
          { error: validation.error },
          { status: validation.status },
        );
      }

      const normalized = normalizeSubmitTxCanonicalCborToNative(
        validation.txCanonicalCbor,
      );
      if (!normalized.ok) {
        yield* submitNormalizeDurationTimer(
          Effect.succeed(Duration.millis(Date.now() - normalizeStartedAt)),
        );
        yield* Effect.logInfo(`▫️ ${normalized.error}`);
        yield* Effect.logInfo(`▫️ ${normalized.detail}`);
        yield* recordLatency();
        return yield* HttpServerResponse.json(
          { error: normalized.error },
          { status: 400 },
        );
      }
      yield* submitNormalizeDurationTimer(
        Effect.succeed(Duration.millis(Date.now() - normalizeStartedAt)),
      );

      const durableAdmissionStartedAt = Date.now();
      const admitted = yield* TxAdmissionsDB.admit({
        txId: normalized.txId,
        txCanonicalCbor: normalized.txCanonicalCbor,
        submitSource: normalized.source,
        maxBacklog: nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG,
      });
      yield* submitDurableAdmissionDurationTimer(
        Effect.succeed(Duration.millis(Date.now() - durableAdmissionStartedAt)),
      );
      if (admitted.kind === "new") {
        yield* requestTxQueueProcessorWakeup;
      }

      Effect.runSync(Metric.increment(txCounter));
      yield* recordLatency();
      const responseStartedAt = Date.now();
      const response = yield* HttpServerResponse.json(
        {
          txId: normalized.txIdHex,
          status: admitted.entry.status,
          firstSeenAt: admitted.entry.first_seen_at.toISOString(),
          lastSeenAt: admitted.entry.last_seen_at.toISOString(),
          duplicate: admitted.kind === "duplicate",
        },
        { status: admitted.kind === "new" ? 202 : 200 },
      );
      yield* submitResponseDurationTimer(
        Effect.succeed(Duration.millis(Date.now() - responseStartedAt)),
      );
      return response;
    }).pipe(
      Effect.catchTag("TxAdmissionConflictError", (e) =>
        Effect.gen(function* () {
          yield* recordLatency();
          return yield* HttpServerResponse.json(
            {
              error: "E_TX_ID_BYTES_CONFLICT",
              message: e.message,
              txId: e.txIdHex,
            },
            { status: 409 },
          );
        }),
      ),
      Effect.catchTag("TxAdmissionBacklogFullError", (e) =>
        Effect.gen(function* () {
          yield* Metric.increment(submitQueueOfferFailureCounter);
          yield* recordLatency();
          return yield* HttpServerResponse.json(
            {
              error: "Durable submission admission backlog is full",
              backlog: e.backlog.toString(),
              maxBacklog: e.maxBacklog.toString(),
            },
            { status: 503 },
          );
        }),
      ),
      Effect.catchTag("DatabaseError", (e) =>
        Effect.gen(function* () {
          yield* recordLatency();
          return yield* failWith500(
            "POST",
            "submit",
            e.cause,
            "durable transaction admission failed",
          );
        }),
      ),
      Effect.catchTag("HttpBodyError", (e) =>
        failWith500("POST", "submit", e, "▫️ L2 transaction failed"),
      ),
    );
  });

/**
 * Builds the full HTTP router for the node command server.
 */
export const buildListenRouter = (
  withMonitoring?: boolean,
): Effect.Effect<
  HttpServerResponse.HttpServerResponse,
  HttpBodyError,
  | Database
  | Lucid
  | NodeConfig
  | MidgardContracts
  | SqlClient
  | HttpServerRequest.HttpServerRequest
  | Globals
> =>
  HttpRouter.empty
    .pipe(
      HttpRouter.get(`/${HEALTH_ENDPOINT}`, getHealthHandler),
      HttpRouter.get(`/${READINESS_ENDPOINT}`, getReadinessHandler),
      HttpRouter.get(`/${PIPELINE_STATUS_ENDPOINT}`, getPipelineStatusHandler),
      HttpRouter.get(`/${PROTOCOL_INFO_ENDPOINT}`, getProtocolInfoHandler),
      HttpRouter.get(`/${TX_ENDPOINT}`, getTxHandler),
      HttpRouter.get(`/${TX_STATUS_ENDPOINT}`, getTxStatusHandler),
      HttpRouter.post(`/${TX_STATUS_ENDPOINT}`, postTxStatusBatchHandler),
      HttpRouter.get(`/${DEPOSIT_STATUS_ENDPOINT}`, getDepositStatusHandler),
      HttpRouter.get(`/${ADDRESS_HISTORY_ENDPOINT}`, getTxsOfAddressHandler),
      HttpRouter.get(`/${UTXO_ENDPOINT}`, getUtxoHandler),
      HttpRouter.get(`/${UTXOS_ENDPOINT}`, getUtxosHandler),
      HttpRouter.get(`/${BLOCK_ENDPOINT}`, getBlockHandler),
    )
    .pipe(
      HttpRouter.get(
        `/${INIT_ENDPOINT}`,
        withAdminAccess(INIT_ENDPOINT, getInitHandler),
      ),
      HttpRouter.get(
        `/${COMMIT_ENDPOINT}`,
        withAdminAccess(COMMIT_ENDPOINT, getCommitEndpoint),
      ),
      HttpRouter.get(
        `/${MERGE_ENDPOINT}`,
        withAdminAccess(MERGE_ENDPOINT, getMergeHandler),
      ),
      HttpRouter.get(
        `/${STATE_QUEUE_ENDPOINT}`,
        withAdminAccess(STATE_QUEUE_ENDPOINT, getStateQueueHandler),
      ),
      HttpRouter.get(
        `/${STATE_QUEUE_MUTATION_LEASE_ENDPOINT}`,
        withAdminAccess(
          STATE_QUEUE_MUTATION_LEASE_ENDPOINT,
          getStateQueueMutationLeaseHandler,
        ),
      ),
      HttpRouter.post(
        `/${STATE_QUEUE_MUTATION_LEASE_ENDPOINT}`,
        withAdminAccess(
          STATE_QUEUE_MUTATION_LEASE_ENDPOINT,
          postStateQueueMutationLeaseHandler,
        ),
      ),
      HttpRouter.get(
        `/logBlocksDB`,
        withAdminAccess("logBlocksDB", getLogBlocksDBHandler),
      ),
      HttpRouter.get(
        `/logGlobals`,
        withAdminAccess("logGlobals", getLogGlobalsHandler),
      ),
      HttpRouter.post(`/${UTXOS_ENDPOINT}`, postUtxosByTxOutRefsHandler),
      HttpRouter.post(`/${DEPOSIT_BUILD_ENDPOINT}`, postDepositBuildHandler),
      HttpRouter.post(`/${SUBMIT_ENDPOINT}`, postSubmitHandler(withMonitoring)),
    )
    .pipe(
      Effect.catchAllCause((cause) =>
        failWith500("GET", "router", Cause.pretty(cause), "unknown endpoint"),
      ),
    );
