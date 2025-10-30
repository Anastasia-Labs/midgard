import {
  Database,
  NodeConfig,
  Lucid,
  AlwaysSucceedsContract,
  Globals,
} from "@/services/index.js";
import { StateQueueTx, Reset } from "@/transactions/index.js";
import * as SDK from "@al-ft/midgard-sdk";
import { NodeSdk } from "@effect/opentelemetry";
import {
  TxSubmitError,
  fromHex,
  getAddressDetails,
  toHex,
} from "@lucid-evolution/lucid";
import { PrometheusExporter } from "@opentelemetry/exporter-prometheus";
import { OTLPTraceExporter } from "@opentelemetry/exporter-trace-otlp-http";
import { BatchSpanProcessor } from "@opentelemetry/sdk-trace-base";
import {
  Cause,
  Duration,
  Effect,
  Layer,
  Metric,
  pipe,
  Queue,
  Ref,
  Schedule,
} from "effect";
import {
  AddressHistoryDB,
  BlocksDB,
  ConfirmedLedgerDB,
  ImmutableDB,
  InitDB,
  LatestLedgerDB,
  MempoolDB,
  MempoolLedgerDB,
  ProcessedMempoolDB,
} from "../database/index.js";
import { isHexString } from "../utils.js";
import {
  HttpRouter,
  HttpServer,
  HttpServerRequest,
  HttpServerResponse,
} from "@effect/platform";
import { ParsedSearchParams } from "@effect/platform/HttpServerRequest";
import { createServer } from "node:http";
import { NodeHttpServer } from "@effect/platform-node";
import { HttpBodyError } from "@effect/platform/HttpBody";
import * as Genesis from "@/genesis.js";
import { deleteLedgerMpt, deleteMempoolMpt } from "@/workers/utils/mpt.js";
import { SerializedStateQueueUTxO } from "@/workers/utils/commit-block-header.js";
import { DatabaseError } from "@/database/utils/common.js";
import { TxConfirmError, TxSignError } from "@/transactions/utils.js";
import {
  fetchAndInsertDepositUTxOsFiber,
  blockConfirmationFiber,
  blockCommitmentFiber,
  blockCommitmentAction,
  mergeFiber,
  mergeAction,
  monitorMempoolFiber,
  txQueueProcessorFiber,
} from "@/fibers/index.js";

const TX_ENDPOINT: string = "tx";
const ADDRESS_HISTORY_ENDPOINT: string = "txs";
const MERGE_ENDPOINT: string = "merge";
const UTXOS_ENDPOINT: string = "utxos";
const BLOCK_ENDPOINT: string = "block";
const INIT_ENDPOINT: string = "init";
const COMMIT_ENDPOINT: string = "commit";
const RESET_ENDPOINT: string = "reset";
const SUBMIT_ENDPOINT: string = "submit";

const txCounter = Metric.counter("tx_count", {
  description: "A counter for tracking submit transactions",
  bigint: true,
  incremental: true,
});

const failWith500Helper = (
  logLabel: string,
  logMsg: string,
  error: any,
  msgOverride?: string,
) =>
  Effect.gen(function* () {
    yield* Effect.logInfo(`${logLabel} - ${logMsg}: ${error}`);
    return yield* HttpServerResponse.json(
      { error: msgOverride ?? "Something went wrong" },
      { status: 500 },
    );
  });

const failWith500 = (
  method: "GET" | "POST",
  endpoint: string,
  error: HttpBodyError | string | any,
  msgOverride?: string,
) => failWith500Helper(`${method} /${endpoint}`, "failure", error, msgOverride);

const handleDBGetFailure = (endpoint: string, e: DatabaseError) =>
  failWith500("GET", endpoint, e.cause, `db failure with table ${e.table}`);

const handleTxGetFailure = (
  endpoint: string,
  e: TxSignError | TxConfirmError | TxSubmitError,
) => failWith500("GET", endpoint, e.cause, `${e._tag}: ${e.message}`);

const handleGenericGetFailure = (
  endpoint: string,
  e: SDK.Utils.GenericErrorFields,
) => failWith500("GET", endpoint, e.cause, e.message);

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
  yield* Effect.logInfo("foundCbor", SDK.Utils.bufferToHex(foundCbor));
  return yield* HttpServerResponse.json({
    tx: SDK.Utils.bufferToHex(foundCbor),
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", TX_ENDPOINT, e)),
  Effect.catchTag("DatabaseError", (e) => handleDBGetFailure(TX_ENDPOINT, e)),
);

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

    const response = utxosWithAddress.map((entry) => ({
      outref: SDK.Utils.bufferToHex(entry.outref),
      value: SDK.Utils.bufferToHex(entry.output),
    }));

    yield* Effect.logInfo(`Found ${response.length} UTxOs for ${addr}`);
    return yield* HttpServerResponse.json({
      utxos: response,
    });
  } catch (error) {
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
    hashes: hashes.map(SDK.Utils.bufferToHex),
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) =>
    failWith500("GET", BLOCK_ENDPOINT, e),
  ),
  Effect.catchTag("DatabaseError", (e) =>
    handleDBGetFailure(BLOCK_ENDPOINT, e),
  ),
);

const getInitHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✨ Initialization request received`);
  const result = yield* StateQueueTx.stateQueueInit;
  yield* Genesis.program;
  yield* Effect.logInfo(
    `GET /${INIT_ENDPOINT} - Initialization successful: ${result}`,
  );
  return yield* HttpServerResponse.json({
    message: `Initiation successful: ${result}`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", INIT_ENDPOINT, e)),
  Effect.catchTag("LucidError", (e) =>
    handleGenericGetFailure(INIT_ENDPOINT, e),
  ),
  Effect.catchTag("TxSubmitError", (e) => handleTxGetFailure(INIT_ENDPOINT, e)),
  Effect.catchTag("TxSignError", (e) => handleTxGetFailure(INIT_ENDPOINT, e)),
);

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

const getMergeHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`GET /${MERGE_ENDPOINT} - Manual merge order received`);
  const result = yield* mergeAction;
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
    handleGenericGetFailure(MERGE_ENDPOINT, e),
  ),
);

const getResetHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`🚧 Reset request received`);
  yield* Reset.resetStateQueue;
  yield* Reset.resetDeposits;

  yield* Effect.all(
    [
      MempoolDB.clear,
      MempoolLedgerDB.clear,
      ProcessedMempoolDB.clear,
      BlocksDB.clear,
      ImmutableDB.clear,
      LatestLedgerDB.clear,
      ConfirmedLedgerDB.clear,
      AddressHistoryDB.clear,
      deleteMempoolMpt,
      deleteLedgerMpt,
    ],
    { discard: true },
  );
  return yield* HttpServerResponse.json({
    message: `Collected all UTxOs successfully!`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "reset", e)),
  Effect.catchTag("DatabaseError", (e) =>
    handleDBGetFailure(RESET_ENDPOINT, e),
  ),
  Effect.catchTag("TxSubmitError", (e) =>
    handleTxGetFailure(RESET_ENDPOINT, e),
  ),
  Effect.catchTag("TxSignError", (e) => handleTxGetFailure(RESET_ENDPOINT, e)),
  Effect.catchTag("LucidError", (e) =>
    handleGenericGetFailure(RESET_ENDPOINT, e),
  ),
);

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
      txs: cbors.map(SDK.Utils.bufferToHex),
    });
  } catch (error) {
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

const getLogStateQueueHandler = Effect.gen(function* () {
  yield* Effect.logInfo(`✍  Drawing state queue UTxOs...`);
  const lucid = yield* Lucid;
  const alwaysSucceeds = yield* AlwaysSucceedsContract;
  const fetchConfig: SDK.TxBuilder.StateQueue.FetchConfig = {
    stateQueuePolicyId: alwaysSucceeds.stateQueueAuthValidator.policyId,
    stateQueueAddress:
      alwaysSucceeds.stateQueueAuthValidator.spendScriptAddress,
  };
  const sortedUTxOs =
    yield* SDK.Endpoints.StateQueue.fetchSortedStateQueueUTxOsProgram(
      lucid.api,
      fetchConfig,
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
          // if (isHead) {
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
    message: `State queue drawn in server logs!`,
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

  yield* Effect.logInfo(`
  BLOCKS_IN_QUEUE ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${BLOCKS_IN_QUEUE}
  LATEST_SYNC ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${new Date(Number(LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH)).toLocaleString()}
  RESET_IN_PROGRESS ⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${RESET_IN_PROGRESS}
  AVAILABLE_CONFIRMED_BLOCK ⋅⋅⋅⋅⋅⋅⋅⋅⋅ ${JSON.stringify(AVAILABLE_CONFIRMED_BLOCK)}
  PROCESSED_UNSUBMITTED_TXS_COUNT ⋅⋅⋅ ${PROCESSED_UNSUBMITTED_TXS_COUNT}
  PROCESSED_UNSUBMITTED_TXS_SIZE ⋅⋅⋅⋅ ${PROCESSED_UNSUBMITTED_TXS_SIZE}
  UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH ⋅⋅⋅⋅⋅⋅⋅ ${UNCONFIRMED_SUBMITTED_BLOCK_TX_HASH}
`);
  return yield* HttpServerResponse.json({
    message: `Global variables logged!`,
  });
}).pipe(
  Effect.catchTag("HttpBodyError", (e) => failWith500("GET", "logGlobals", e)),
);

const postSubmitHandler = (txQueue: Queue.Enqueue<string>) =>
  Effect.gen(function* () {
    // yield* Effect.logInfo(`◻️  Submit request received for transaction`);
    const params = yield* ParsedSearchParams;
    const txStringParam = params["tx_cbor"];
    if (typeof txStringParam !== "string" || !isHexString(txStringParam)) {
      yield* Effect.logInfo(`▫️ Invalid CBOR provided`);
      return yield* HttpServerResponse.json(
        { error: `Invalid CBOR provided` },
        { status: 400 },
      );
    } else {
      const txString = txStringParam;
      yield* txQueue.offer(txString);
      Effect.runSync(Metric.increment(txCounter));
      return yield* HttpServerResponse.json({
        message: `Successfully added the transaction to the queue`,
      });
    }
  }).pipe(
    Effect.catchTag("HttpBodyError", (e) =>
      failWith500("POST", "submit", e, "▫️ L2 transaction failed"),
    ),
  );

const router = (
  txQueue: Queue.Queue<string>,
): Effect.Effect<
  HttpServerResponse.HttpServerResponse,
  HttpBodyError,
  | Database
  | Lucid
  | NodeConfig
  | AlwaysSucceedsContract
  | HttpServerRequest.HttpServerRequest
  | Globals
> =>
  HttpRouter.empty
    .pipe(
      HttpRouter.get(`/${TX_ENDPOINT}`, getTxHandler),
      HttpRouter.get(`/${ADDRESS_HISTORY_ENDPOINT}`, getTxsOfAddressHandler),
      HttpRouter.get(`/${UTXOS_ENDPOINT}`, getUtxosHandler),
      HttpRouter.get(`/${BLOCK_ENDPOINT}`, getBlockHandler),
      HttpRouter.get(`/${INIT_ENDPOINT}`, getInitHandler),
      HttpRouter.get(`/${COMMIT_ENDPOINT}`, getCommitEndpoint),
      HttpRouter.get(`/${MERGE_ENDPOINT}`, getMergeHandler),
      HttpRouter.get(`/${RESET_ENDPOINT}`, getResetHandler),
      HttpRouter.get(`/logStateQueue`, getLogStateQueueHandler),
      HttpRouter.get(`/logBlocksDB`, getLogBlocksDBHandler),
      HttpRouter.get(`/logGlobals`, getLogGlobalsHandler),
      HttpRouter.post(`/${SUBMIT_ENDPOINT}`, postSubmitHandler(txQueue)),
    )
    .pipe(
      Effect.catchAllCause((cause) =>
        failWith500Helper(
          "Router unexpected failure",
          "unknown endpoint",
          Cause.pretty(cause),
        ),
      ),
    );

export const runNode = Effect.gen(function* () {
  const nodeConfig = yield* NodeConfig;

  const prometheusExporter = new PrometheusExporter(
    {
      port: nodeConfig.PROM_METRICS_PORT,
    },
    () => {
      `Prometheus metrics available at http://localhost:${nodeConfig.PROM_METRICS_PORT}/metrics`;
    },
  );

  const originalStop = prometheusExporter.stopServer;
  prometheusExporter.stopServer = async function () {
    Effect.runSync(Effect.logInfo("Prometheus exporter is stopping!"));
    return originalStop();
  };

  const MetricsLive = NodeSdk.layer(() => ({
    resource: { serviceName: "midgard-node" },
    metricReader: prometheusExporter,
    spanProcessor: new BatchSpanProcessor(
      new OTLPTraceExporter({ url: nodeConfig.OLTP_EXPORTER_URL }),
    ),
  }));

  yield* InitDB.initializeDb();

  yield* Genesis.program;

  const txQueue = yield* Queue.unbounded<string>();
  const appThread = Layer.launch(
    Layer.provide(
      HttpServer.serve(router(txQueue)),
      NodeHttpServer.layer(createServer, { port: nodeConfig.PORT }),
    ),
  );

  const program = Effect.all(
    [
      appThread,
      blockCommitmentFiber(
        Schedule.spaced(
          Duration.millis(nodeConfig.WAIT_BETWEEN_BLOCK_COMMITMENT),
        ),
      ),
      blockConfirmationFiber(
        Schedule.spaced(
          Duration.millis(nodeConfig.WAIT_BETWEEN_BLOCK_CONFIRMATION),
        ),
      ),
      fetchAndInsertDepositUTxOsFiber(
        Schedule.spaced(
          Duration.millis(nodeConfig.WAIT_BETWEEN_DEPOSIT_UTXO_FETCHES),
        ),
      ),
      mergeFiber(
        Schedule.spaced(Duration.millis(nodeConfig.WAIT_BETWEEN_MERGE_TXS)),
      ),
      monitorMempoolFiber(Schedule.spaced(Duration.millis(1000))),
      txQueueProcessorFiber(Schedule.spaced(Duration.millis(500)), txQueue),
    ],
    {
      concurrency: "unbounded",
    },
  ).pipe(
    Effect.provide(Database.layer),
    Effect.provide(AlwaysSucceedsContract.Default),
    Effect.provide(Lucid.Default),
    Effect.provide(NodeConfig.layer),
    Effect.provide(Globals.Default),
  );

  pipe(
    program,
    Effect.withSpan("midgard"),
    Effect.provide(MetricsLive),
    Effect.catchAllCause(Effect.logError),
    Effect.runPromise,
  );
});
