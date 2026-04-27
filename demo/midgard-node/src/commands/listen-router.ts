/**
 * Explicit HTTP route graph for the node's command server.
 * This module groups endpoint handlers and access control in one place while
 * delegating startup checks and response shaping to narrower modules.
 */
import {
  AddressHistoryDB,
  BlocksDB,
  ImmutableDB,
  MempoolDB,
  MempoolLedgerDB,
  MutationJobsDB,
  ProcessedMempoolDB,
  TxAdmissionsDB,
  TxRejectionsDB,
} from "@/database/index.js";
import { Database } from "@/services/index.js";
import {
  Globals,
  Lucid,
  MidgardContracts,
  NodeConfig,
} from "@/services/index.js";
import { isHexString } from "@/utils.js";
import { QueuedTxPayload } from "@/validation/index.js";
import {
  authorizeAdminRoute,
  extractSubmitTxHex,
  extractSubmitTxHexFromQueryParams,
  isAdminRoutePath,
  normalizeSubmitTxHexToNative,
  validateSubmitTxHex,
} from "@/commands/listen-utils.js";
import { evaluateReadiness } from "@/commands/readiness.js";
import {
  failWith500,
  handleDBGetFailure,
  handleGenericGetFailure,
  handleStateQueueGetFailure,
  handleTxGetFailure,
} from "@/commands/listen-response.js";
import * as DepositStatusCommand from "@/commands/deposit-status.js";
import { resolveTxStatus } from "@/commands/tx-status.js";
import * as UtxosCommand from "@/commands/utxos.js";
import * as SubmitDeposit from "@/transactions/submit-deposit.js";
import {
  fetchReferenceScriptUtxosProgram,
  referenceScriptByName,
  referenceScriptTargetsByCommand,
} from "@/transactions/reference-scripts.js";
import { fromHex, getAddressDetails, toHex } from "@lucid-evolution/lucid";
import * as SDK from "@al-ft/midgard-sdk";
import { Cause, Duration, Effect, Metric, Queue, Ref } from "effect";
import {
  HttpBodyError,
  HttpRouter,
  HttpServerRequest,
  HttpServerResponse,
} from "@effect/platform";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { DatabaseError } from "@/database/utils/common.js";
import { SqlClient } from "@effect/sql/SqlClient";
import { blockCommitmentAction, mergeAction } from "@/fibers/index.js";
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
const DEPOSIT_STATUS_ENDPOINT: string = "deposit-status";
const HEALTH_ENDPOINT: string = "healthz";
const READINESS_ENDPOINT: string = "readyz";

const txCounter = Metric.counter("tx_count", {
  description: "A counter for tracking submit transactions",
  bigint: true,
  incremental: true,
});

const submitHandlerLatencyTimer = Metric.timer(
  "submit_handler_latency",
  "Latency of POST /submit handler responses in milliseconds",
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
  if (
    typeof txHashParam !== "string" ||
    !isHexString(txHashParam) ||
    txHashParam.length !== 64
  ) {
    yield* Effect.logInfo(
      `GET /${TX_ENDPOINT} - Invalid transaction hash: ${txHashParam}`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid transaction hash: ${txHashParam}` },
      { status: 404 },
    );
  }
  const txHashBytes = Buffer.from(fromHex(txHashParam));
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
  Effect.catchTag("DatabaseError", (e) => handleDBGetFailure(TX_ENDPOINT, e)),
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
    const addrDetails = getAddressDetails(addr);
    if (!addrDetails.paymentCredential) {
      yield* Effect.logInfo(`Invalid address format: ${addr}`);
      return yield* HttpServerResponse.json(
        { error: `Invalid address format: ${addr}` },
        { status: 400 },
      );
    }

    const utxosWithAddress = yield* MempoolLedgerDB.retrieveByAddress(
      addrDetails.address.bech32,
    );
    const response = UtxosCommand.encodeStoredUtxos(
      utxosWithAddress.map((entry) => ({
        outref: entry.outref,
        output: entry.output,
      })),
    );

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
    handleDBGetFailure(UTXOS_ENDPOINT, e),
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
    txOutRef = UtxosCommand.parseTxOutRefCborHex(rawTxOutRef, "txOutRef");
  } catch (error) {
    const message = error instanceof Error ? error.message : String(error);
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
  Effect.catchTag("DatabaseError", (e) => handleDBGetFailure(UTXO_ENDPOINT, e)),
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
    const message = error instanceof Error ? error.message : String(error);
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
    const message = error instanceof Error ? error.message : String(error);
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
    handleDBGetFailure(UTXOS_ENDPOINT, e),
  ),
);

/**
 * `GET /tx-status`: resolves the node's canonical status for a tx hash.
 */
const getTxStatusHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const txHashParam = params["tx_hash"];
  if (
    typeof txHashParam !== "string" ||
    !isHexString(txHashParam) ||
    txHashParam.length !== 64
  ) {
    return yield* HttpServerResponse.json(
      { error: `Invalid transaction hash: ${txHashParam}` },
      { status: 400 },
    );
  }

  const txHashBytes = Buffer.from(fromHex(txHashParam));
  const globals = yield* Globals;
  const rejected = yield* TxRejectionsDB.retrieveByTxId(txHashBytes);
  const admission = yield* TxAdmissionsDB.getByTxId(txHashBytes);
  const inImmutable = yield* ImmutableDB.retrieveTxCborsByHashes([txHashBytes]);
  const inMempool = yield* MempoolDB.retrieveTxCborsByHashes([txHashBytes]);
  const inProcessedMempool = yield* ProcessedMempoolDB.retrieveTxCborsByHashes([
    txHashBytes,
  ]);

  const resolved = resolveTxStatus({
    txIdHex: txHashParam,
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
    handleDBGetFailure(TX_STATUS_ENDPOINT, e),
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
    const message = error instanceof Error ? error.message : String(error);
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
    handleDBGetFailure(DEPOSIT_STATUS_ENDPOINT, e),
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
const getReadinessHandler = (txQueue: Queue.Dequeue<QueuedTxPayload>) =>
  Effect.gen(function* () {
    const globals = yield* Globals;
    const nodeConfig = yield* NodeConfig;
    const sql = yield* SqlClient;

    const legacyQueueDepth = yield* txQueue.size;
    const durableAdmissionBacklog = yield* TxAdmissionsDB.countBacklog;
    const durableAdmissionOldestAgeMs = yield* TxAdmissionsDB.oldestQueuedAgeMs;
    const unfinishedMutationJobs = yield* MutationJobsDB.countUnfinished;
    const nowMillis = Date.now();
    const blockCommitmentHeartbeat = yield* Ref.get(
      globals.HEARTBEAT_BLOCK_COMMITMENT,
    );
    const blockConfirmationHeartbeat = yield* Ref.get(
      globals.HEARTBEAT_BLOCK_CONFIRMATION,
    );
    const mergeHeartbeat = yield* Ref.get(globals.HEARTBEAT_MERGE);
    const depositFetchHeartbeat = yield* Ref.get(
      globals.HEARTBEAT_DEPOSIT_FETCH,
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
        txQueueProcessor: txQueueProcessorHeartbeat,
      },
      localFinalizationPending,
      unresolvedBlockSubmissionAgeMs,
      maxUnresolvedBlockSubmissionAgeMs:
        nodeConfig.UNCONFIRMED_BLOCK_MAX_AGE_MS,
      dbHealthy,
    });
    const reasons = [...baseReadiness.reasons];
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
    const readiness = {
      ready: reasons.length === 0,
      reasons,
      durableAdmissionBacklog: durableAdmissionBacklog.toString(),
      durableAdmissionOldestAgeMs,
      unfinishedLocalMutationJobs: unfinishedMutationJobs.toString(),
      unresolvedBlockSubmissionAgeMs,
      legacyInMemoryQueueDepth: legacyQueueDepth,
    };

    return yield* HttpServerResponse.json(readiness, {
      status: readiness.ready ? 200 : 503,
    });
  });

/**
 * `GET /block`: returns tx hashes referenced by a committed block header.
 */
const getBlockHandler = Effect.gen(function* () {
  const params = yield* ParsedSearchParams;
  const hdrHash = params["header_hash"];
  yield* Effect.logInfo(
    `GET /block - Request received for header_hash: ${hdrHash}`,
  );

  if (
    typeof hdrHash !== "string" ||
    !isHexString(hdrHash) ||
    hdrHash.length !== 56
  ) {
    yield* Effect.logInfo(
      `GET /${BLOCK_ENDPOINT} - Invalid block hash: ${hdrHash}`,
    );
    return yield* HttpServerResponse.json(
      { error: `Invalid block hash: ${hdrHash}` },
      { status: 400 },
    );
  }
  const hashes = yield* BlocksDB.retrieveTxHashesByHeaderHash(
    Buffer.from(fromHex(hdrHash)),
  );
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
    handleDBGetFailure(BLOCK_ENDPOINT, e),
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
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", INIT_ENDPOINT, e)),
  Effect.catchTag("LucidError", (e) =>
    handleGenericGetFailure(INIT_ENDPOINT, e),
  ),
  Effect.catchTag("MptError", (e) => handleGenericGetFailure(INIT_ENDPOINT, e)),
  Effect.catchTag("TxSubmitError", (e) => handleTxGetFailure(INIT_ENDPOINT, e)),
  Effect.catchTag("TxSignError", (e) => handleTxGetFailure(INIT_ENDPOINT, e)),
  Effect.catchTag("UnspecifiedNetworkError", (e) =>
    handleGenericGetFailure(INIT_ENDPOINT, e),
  ),
);

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
    `GET /${MERGE_ENDPOINT} - Merging confirmed state successful: ${result}`,
  );
  return yield* HttpServerResponse.json({
    message: `Merging confirmed state successful: ${result}`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", MERGE_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    handleDBGetFailure(MERGE_ENDPOINT, e),
  ),
  Effect.catchTag("TxSubmitError", (e) =>
    handleTxGetFailure(MERGE_ENDPOINT, e),
  ),
  Effect.catchTag("TxConfirmError", (e) =>
    handleTxGetFailure(MERGE_ENDPOINT, e),
  ),
  Effect.catchTag("TxSignError", (e) => handleTxGetFailure(MERGE_ENDPOINT, e)),
  Effect.catchTag("CmlDeserializationError", (e) =>
    handleGenericGetFailure(MERGE_ENDPOINT, e),
  ),
  Effect.catchTag("DataCoercionError", (e) =>
    handleGenericGetFailure(MERGE_ENDPOINT, e),
  ),
  Effect.catchTag("LinkedListError", (e) =>
    handleGenericGetFailure(MERGE_ENDPOINT, e),
  ),
  Effect.catchTag("HashingError", (e) =>
    handleGenericGetFailure(MERGE_ENDPOINT, e),
  ),
  Effect.catchTag("LucidError", (e) =>
    handleGenericGetFailure(MERGE_ENDPOINT, e),
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
    const addrDetails = getAddressDetails(addr);
    if (!addrDetails.paymentCredential) {
      yield* Effect.logInfo(`Invalid address format: ${addr}`);
      return yield* HttpServerResponse.json(
        { error: `Invalid address format: ${addr}` },
        { status: 400 },
      );
    }

    const cbors = yield* AddressHistoryDB.retrieve(addrDetails.address.bech32);
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
    handleDBGetFailure(ADDRESS_HISTORY_ENDPOINT, e),
  ),
);

/**
 * `GET /stateQueue`: logs and returns the current ordered state-queue headers.
 */
const getStateQueueHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✍  Drawing state queue UTxOs...`);
  const lucid = yield* Lucid;
  const contracts = yield* MidgardContracts;
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
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", "logStateQueue", e),
  ),
  Effect.catchTag("LinkedListError", (e) =>
    handleGenericGetFailure("logStateQueue", e),
  ),
  Effect.catchTag("LucidError", (e) =>
    handleGenericGetFailure("logStateQueue", e),
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
  Effect.catchTag("DatabaseError", (e) => handleDBGetFailure("logBlocksDB", e)),
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
  const HEARTBEAT_TX_QUEUE_PROCESSOR: number = yield* Ref.get(
    globals.HEARTBEAT_TX_QUEUE_PROCESSOR,
  );

  yield* Effect.logInfo(`
  BLOCKS_IN_QUEUE ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${BLOCKS_IN_QUEUE}
  LATEST_SYNC ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${new Date(Number(LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH)).toLocaleString()}
  RESET_IN_PROGRESS ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${RESET_IN_PROGRESS}
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
    const message = error instanceof Error ? error.message : String(error);
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
const postSubmitHandler = (
  _txQueue: Queue.Enqueue<QueuedTxPayload>,
  withMonitoring?: boolean,
) =>
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
      const params = yield* ParsedSearchParams;
      const queryTxHex = extractSubmitTxHexFromQueryParams(params);
      let bodyTxHex: string | undefined = undefined;
      if (queryTxHex === undefined) {
        const parsedBody = yield* Effect.either(request.json);
        if (parsedBody._tag === "Right") {
          bodyTxHex = extractSubmitTxHex(parsedBody.right);
        }
      }
      const txString = queryTxHex ?? bodyTxHex;

      if (txString === undefined) {
        yield* Effect.logInfo(`▫️ Invalid submit payload: missing tx_cbor`);
        yield* recordLatency();
        return yield* HttpServerResponse.json(
          { error: "Request body must include `tx_cbor` as a hex string" },
          { status: 400 },
        );
      }

      const validation = validateSubmitTxHex(
        txString,
        nodeConfig.MAX_SUBMIT_TX_CBOR_BYTES,
      );
      if (!validation.ok) {
        yield* Effect.logInfo(`▫️ Submit rejected: ${validation.error}`);
        yield* recordLatency();
        return yield* HttpServerResponse.json(
          { error: validation.error },
          { status: validation.status },
        );
      }

      const normalized = normalizeSubmitTxHexToNative(validation.txHex);
      if (!normalized.ok) {
        yield* Effect.logInfo(`▫️ ${normalized.error}`);
        yield* Effect.logInfo(`▫️ ${normalized.detail}`);
        yield* recordLatency();
        return yield* HttpServerResponse.json(
          { error: normalized.error },
          { status: 400 },
        );
      }

      if (normalized.source === "cardano-converted") {
        yield* Effect.logInfo(
          `▫️ Accepted Cardano tx and converted to Midgard-native format`,
        );
      }

      if (normalized.txCbor.length > nodeConfig.MAX_SUBMIT_TX_CBOR_BYTES) {
        yield* recordLatency();
        return yield* HttpServerResponse.json(
          {
            error: `Transaction CBOR exceeds max size (${normalized.txCbor.length} > ${nodeConfig.MAX_SUBMIT_TX_CBOR_BYTES})`,
          },
          { status: 413 },
        );
      }

      const admitted = yield* TxAdmissionsDB.admit({
        txId: normalized.txId,
        txCbor: normalized.txCbor,
        submitSource: normalized.source,
        maxBacklog: nodeConfig.MAX_DURABLE_ADMISSION_BACKLOG,
      });

      Effect.runSync(Metric.increment(txCounter));
      yield* recordLatency();
      return yield* HttpServerResponse.json(
        {
          txId: normalized.txIdHex,
          status: admitted.entry.status,
          firstSeenAt: admitted.entry.first_seen_at.toISOString(),
          lastSeenAt: admitted.entry.last_seen_at.toISOString(),
          duplicate: admitted.kind === "duplicate",
        },
        { status: admitted.kind === "new" ? 202 : 200 },
      );
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
  txQueue: Queue.Queue<QueuedTxPayload>,
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
      HttpRouter.get(`/${READINESS_ENDPOINT}`, getReadinessHandler(txQueue)),
      HttpRouter.get(`/${TX_ENDPOINT}`, getTxHandler),
      HttpRouter.get(`/${TX_STATUS_ENDPOINT}`, getTxStatusHandler),
      HttpRouter.get(`/${DEPOSIT_STATUS_ENDPOINT}`, getDepositStatusHandler),
      HttpRouter.get(`/${ADDRESS_HISTORY_ENDPOINT}`, getTxsOfAddressHandler),
      HttpRouter.get(`/${UTXO_ENDPOINT}`, getUtxoHandler),
      HttpRouter.get(`/${UTXOS_ENDPOINT}`, getUtxosHandler),
      HttpRouter.get(`/${BLOCK_ENDPOINT}`, getBlockHandler),
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
        `/logBlocksDB`,
        withAdminAccess("logBlocksDB", getLogBlocksDBHandler),
      ),
      HttpRouter.get(
        `/logGlobals`,
        withAdminAccess("logGlobals", getLogGlobalsHandler),
      ),
      HttpRouter.post(`/${UTXOS_ENDPOINT}`, postUtxosByTxOutRefsHandler),
      HttpRouter.post(`/${DEPOSIT_BUILD_ENDPOINT}`, postDepositBuildHandler),
      HttpRouter.post(
        `/${SUBMIT_ENDPOINT}`,
        postSubmitHandler(txQueue, withMonitoring),
      ),
    )
    .pipe(
      Effect.catchAllCause((cause) =>
        failWith500("GET", "router", Cause.pretty(cause), "unknown endpoint"),
      ),
    );
